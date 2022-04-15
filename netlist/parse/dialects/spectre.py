"""
# Spectre-Dialect Parsing 
"""

from typing import Optional, Union, List

# Local Imports
from ...data import *
from ..lex import Tokens
from .spice import DialectParser, SpiceDialectParser


class SpectreMixin:
    """ Spectre-stuff to be mixed-in, 
    primarily related to the capacity for `DialectChanges` 
    via a `simulator lang` statement. """

    def parse_dialect_change(self) -> Optional[DialectChange]:
        """ Parse a DialectChange. Leaves its trailing NEWLINE to be parsed by a (likely new) DialectParser. """

        self.expect(Tokens.SIMULATOR)
        self.expect(Tokens.LANG)
        self.expect(Tokens.EQUALS)
        self.expect(Tokens.IDENT)
        d = DialectChange(self.cur.val)

        # FIXME: ignoring additional parameters e.g. `insensitive`
        while self.nxt and self.nxt.tp != Tokens.NEWLINE:
            self.advance()
        # self.expect(Tokens.NEWLINE) # Note this is left for the *new* dialect to parse

        self.parent.notify(d)
        return d


class SpectreSpiceDialectParser(SpectreMixin, SpiceDialectParser):
    """ Spice-Style Syntax, as Interpreted by Spectre. 

    Primarily differs from the base SpiceDialect in its capacity for
    `simulator lang` statements which produce `DialectChanges`. """

    enum = NetlistDialects.SPECTRE_SPICE

    def parse_statement(self) -> Optional[Statement]:
        """ Mix-in the `simulator lang` DialectChange Statments """
        self.eat_blanks()
        pk = self.peek()
        if pk and pk.tp == Tokens.SIMULATOR:
            return self.parse_dialect_change()
        return super().parse_statement()


class SpectreDialectParser(SpectreMixin, DialectParser):
    """ Spectre-Language Dialect. 
    Probably more of a separate language really, but it fits our Dialect paradigm well enough. """

    enum = NetlistDialects.SPECTRE

    def parse_statement(self) -> Optional[Statement]:
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. 
        Returns `None` at end. """

        self.eat_blanks()
        pk = self.peek()

        if pk is None:  # End-of-input case
            return None

        rules = {
            Tokens.SIMULATOR: self.parse_dialect_change,
            Tokens.PARAMETERS: self.parse_param_statement,
            Tokens.INLINE: self.parse_subckt_start,
            Tokens.SUBCKT: self.parse_subckt_start,
            Tokens.ENDS: self.parse_end_sub,
            Tokens.MODEL: self.parse_model,
            Tokens.STATS: self.parse_statistics_block,
            Tokens.AHDL: self.parse_ahdl,
            Tokens.LIBRARY: self.parse_start_lib,
            Tokens.SECTION: self.parse_start_section,
            Tokens.ENDSECTION: self.parse_end_section,
            Tokens.INCLUDE: self.parse_include,
            Tokens.REAL: self.parse_function_def,
            Tokens.PROT: self.parse_protect,
            Tokens.PROTECT: self.parse_protect,
            Tokens.UNPROT: self.parse_unprotect,
            Tokens.UNPROTECT: self.parse_unprotect,
            Tokens.IDENT: self.parse_named,  # Catch-all for any non-keyword identifier
        }
        pk = self.peek()
        if pk.tp not in rules:
            # No match - error time.
            return self.fail(f"Unexpected token to begin statement: {pk}")
        # Call the type-specific parsing function
        type_parser = rules[pk.tp]
        return type_parser()

    def parse_named(self):
        """ Parse an identifier-named statement. 
        Instances, Options, and Analyses fall into this category,
        by beginning with their name, then their type-keyword. 
        The general method is to read one token ahead, then rewind 
        before dispatching to more detailed parsing methods. """
        self.expect(Tokens.IDENT)
        if self.nxt is None:
            self.fail()
        if self.nxt.tp == Tokens.OPTIONS:
            self.rewind()
            return self.parse_options()
        if self.nxt.tp in (Tokens.IDENT, Tokens.INT, Tokens.LPAREN):
            self.rewind()
            return self.parse_instance()
        # No match - error time.
        self.fail()

    def parse_model(self) -> Union[ModelDef, ModelFamily]:
        """ Parse a Model statement """
        self.expect(Tokens.MODEL)
        mname = self.parse_ident()
        mtype = self.parse_ident()
        if self.match(Tokens.LBRACKET):
            self.expect(Tokens.NEWLINE)
            # Multi-Variant Model Family
            vars = []
            while not self.match(Tokens.RBRACKET):
                self.expect(Tokens.IDENT, Tokens.INT)
                vname = Ident(str(self.cur.val))
                self.expect(Tokens.COLON)
                params = self.parse_param_declarations()
                vars.append(ModelVariant(mname, vname, mtype, [], params))
            self.expect(Tokens.NEWLINE)
            return ModelFamily(mname, mtype, vars)
        # Single ModelDef
        params = self.parse_param_declarations()
        return ModelDef(mname, mtype, [], params)

    def parse_param_statement(self) -> ParamDecls:
        """ Parse a Parameter-Declaration Statement """
        from .base import _endargs_startkwargs

        self.expect(Tokens.PARAMETERS)
        # Parse an initial list of identifiers, i.e. non-default-valued parameters
        args = self.parse_ident_list(_endargs_startkwargs)
        # If we landed on a key-value param key, rewind it
        if self.nxt and self.nxt.tp == Tokens.EQUALS:
            self.rewind()
            args.pop()
        args = [ParamDecl(a, None) for a in args]
        # Parse the remaining default-valued params
        vals = self.parse_param_declarations()  # NEWLINE is captured inside
        return ParamDecls(args + vals)

    def parse_variations(self) -> List[Variation]:
        """ Parse a list of variation-statements, of the form
        `{ 
            vary param1 dist=distname std=stdval 
            vary param2 dist=distname std=stdval 
        }\n` 
        Consumes the both opening and closing brackets, 
        and the (required) newline following the closing bracket. """
        self.expect(Tokens.LBRACKET)
        self.expect(Tokens.NEWLINE)
        vars = []
        while not self.match(Tokens.RBRACKET):
            self.expect(Tokens.IDENT)
            if self.cur.val != "vary":
                self.fail()
            self.expect(Tokens.IDENT)
            name = Ident(self.cur.val)

            dist = None
            std = None
            percent = None  # FIXME: roll in
            while not self.match(Tokens.NEWLINE):
                self.expect(Tokens.IDENT)
                if self.cur.val == "dist":
                    if dist is not None:
                        self.fail()
                    self.expect(Tokens.EQUALS)
                    self.expect(Tokens.IDENT)
                    dist = str(self.cur.val)
                elif self.cur.val == "std":
                    if std is not None:
                        self.fail()
                    self.expect(Tokens.EQUALS)
                    std = self.parse_expr()
                elif self.cur.val == "percent":
                    self.expect(Tokens.EQUALS)
                    percent = self.parse_expr()
                else:
                    self.fail()
            vars.append(Variation(name, dist, std))  # FIXME: roll in `percent`

        self.expect(Tokens.NEWLINE)
        return vars

    def parse_statistics_block(self) -> StatisticsBlock:
        """ Parse the `statistics` block """

        self.expect(Tokens.STATS)
        self.expect(Tokens.LBRACKET)
        self.expect(Tokens.NEWLINE)

        process = None
        mismatch = None

        while not self.match(Tokens.RBRACKET):
            self.expect(Tokens.IDENT)
            if self.cur.val == "process":
                if process is not None:
                    self.fail()
                process = self.parse_variations()
            elif self.cur.val == "mismatch":
                if mismatch is not None:
                    self.fail()
                mismatch = self.parse_variations()
            else:
                self.fail()

        self.expect(Tokens.NEWLINE)
        return StatisticsBlock(process=process, mismatch=mismatch)

    def parse_ahdl(self):
        """ Parse an `ahdl_include` statement """
        self.expect(Tokens.AHDL)
        path = self.parse_quote_string()
        rv = AhdlInclude(path)
        self.expect(Tokens.NEWLINE)
        return rv

    def parse_instance_param_values(self) -> List[ParamVal]:
        """ Parse a list of instance parameter-values, 
        including the fun-fact that Spectre allows arbitrary dangling closing parens. """
        term = (
            lambda s: s.nxt is None or s.match(Tokens.NEWLINE) or s.match(Tokens.RPAREN)
        )
        vals = self.parse_list(self.parse_param_val, term=term)
        if self.match(Tokens.RPAREN):  # Eat potential dangling r-parens
            while self.nxt is not None and not self.match(Tokens.NEWLINE):
                self.expect(Tokens.RPAREN)
        return vals

    def are_stars_comments_now(self) -> bool:
        # Stars are comments only to begin lines, and at the beginning of a file. (We think?)
        return not self.lex.lexed_nonwhite_on_this_line

    def parse_start_lib(self):
        self.expect(Tokens.LIBRARY)
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.NEWLINE)
        return StartLib(name)

    def parse_start_section(self):
        self.expect(Tokens.SECTION)

        if self.match(Tokens.EQUALS):
            ...  # Apparently there is an optional "=" character here

        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.NEWLINE)
        return StartLibSection(name)

    def parse_end_section(self):
        self.expect(Tokens.ENDSECTION)
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.NEWLINE)
        return EndLibSection(name)

    def parse_options(self):
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.OPTIONS)
        vals = self.parse_option_values()
        return Options(name=name, vals=vals)

    def parse_include(self) -> Union[Include, UseLib]:
        """ Parse an Include Statement """
        self.expect(Tokens.INCLUDE)
        path = self.parse_quote_string()
        if self.match(Tokens.NEWLINE):  # Non-sectioned `Include`
            return Include(path)
        # Otherwise expect a library `Section`
        self.expect(Tokens.SECTION)
        self.expect(Tokens.EQUALS)
        self.expect(Tokens.IDENT)
        section = Ident(self.cur.val)
        return UseLib(path, section)

    def parse_function_def(self):
        """ Yes, Spectre does have function definitions! 
        Syntax: `rtype name (argtype argname, argtype argname) {
            statements;
            return rval;
        }`
        Caveats:
        * Only `real` return and argument types are supported
        * Only single-statement functions comprising a `return Expr;` are supported
        """
        self.expect(Tokens.REAL)  # Return type. FIXME: support more types than REAL
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.LPAREN)
        # Parse arguments
        args = []
        MAX_ARGS = 100  # Set a "time-out" so that we don't get stuck here.
        for i in range(MAX_ARGS, -1, -1):
            if self.match(Tokens.RPAREN):
                break
            self.expect(
                Tokens.REAL
            )  # Argument type. FIXME: support more types than REAL
            self.expect(Tokens.IDENT)
            a = TypedArg(tp=Ident("real"), name=Ident(self.cur.val))
            args.append(a)
            if self.match(Tokens.RPAREN):
                break
            self.expect(Tokens.COMMA)
        if i <= 0:  # Check the time-out
            self.fail()

        self.expect(Tokens.LBRACKET)
        self.expect(Tokens.NEWLINE)
        # Return-statement
        self.expect(Tokens.RETURN)
        rv = self.parse_expr()
        ret = Return(rv)
        self.expect(Tokens.SEMICOLON)
        self.expect(Tokens.NEWLINE)
        # Function-Closing
        self.expect(Tokens.RBRACKET)
        self.expect(Tokens.NEWLINE)

        return FunctionDef(name=name, rtype=Ident("real"), args=args, stmts=[ret])
