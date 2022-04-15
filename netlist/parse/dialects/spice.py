"""
# Spice-Dialect Parsing 
"""
from typing import Optional, Union

# Local Imports
from ...data import *
from .base import DialectParser, Tokens


class SpiceDialectParser(DialectParser):
    """ Family of SPICE-like syntax dialects, complete with 
    * Dot-based "control cards", e.g. `.subckt`, `.ac`, `.end`, 
    * Prefix-based primitive element instances
    Further specializations are made for HSPICE, NGSPICE, etc. 
    """

    enum = NetlistDialects.SPICE

    def parse_statement(self) -> Optional[ast.Statement]:
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. 
        Returns `None` at end. """

        self.eat_blanks()
        pk = self.peek()

        if pk is None:  # End-of-input case
            return None

        if self.match(Tokens.DOT):  # Control Statements
            rules = {
                Tokens.PARAM: self.parse_param_statement,
                Tokens.SUBCKT: self.parse_subckt_start,
                Tokens.ENDS: self.parse_end_sub,
                Tokens.MODEL: self.parse_model,
                Tokens.OPTION: self.parse_options,
                Tokens.INC: self.parse_include,
                Tokens.INCLUDE: self.parse_include,
                Tokens.LIB: self.parse_lib_statement,
                Tokens.ENDL: self.parse_endl,
                Tokens.PROT: self.parse_protect,
                Tokens.PROTECT: self.parse_protect,
                Tokens.UNPROT: self.parse_unprotect,
                Tokens.UNPROTECT: self.parse_unprotect,
            }
            pk = self.peek()
            if pk.tp not in rules:
                return self.fail(f"Invalid or unsupported dot-statement: {pk}")
            # Call the type-specific parsing function
            type_parser = rules[pk.tp]
            return type_parser()

        elif pk.tp == Tokens.IDENT:
            if pk.val.lower().startswith("x"):
                return self.parse_instance()
            return self.parse_primitive()

        # No match - error time.
        return self.fail(f"Unexpected token to begin statement: {pk}")

    def parse_lib_statement(self) -> Union[ast.StartLibSection, ast.UseLib]:
        """ 
        Parse a Spice-format `.lib` statement, 
        which sadly can mean either of 
        * (a) Start defining a `LibSection`, or 
        * (b) *Include* the content of a `LibSection` 
        The difference depends on how many fields show up after `.lib`. 
        """
        self.expect(Tokens.LIB)

        # Parse that first argument, which can be either a file-path or an identifier.
        # Parse it to string as a file-path, which *should* keep identifiers intact.
        first_arg = self.parse_path()

        if self.peek().tp == Tokens.IDENT:
            # Three-argument form. Create a `UseLib`.
            section = self.parse_ident()
            self.expect(Tokens.NEWLINE)
            return ast.UseLib(path=first_arg, section=section)

        # Two-argument form. Create a `StartLibSection`
        self.expect(Tokens.NEWLINE)
        return ast.StartLibSection(Ident(first_arg))

    def parse_include(self) -> ast.Include:
        """ Parse an Include Statement """
        self.expect(Tokens.INC, Tokens.INCLUDE)
        path = self.parse_path()
        self.expect(Tokens.NEWLINE)
        return ast.Include(path)

    def parse_endl(self) -> ast.EndLibSection:
        """ Parse an end-lib-section Statement """
        self.expect(Tokens.ENDL)
        name = self.parse_ident()  # FIXME: is this optional?
        self.expect(Tokens.NEWLINE)
        return ast.EndLibSection(name)

    def parse_path(self) -> str:
        """ Parse a file-system path, either quote-delimited or not """
        if self.peek().tp in (Tokens.TICK, Tokens.DUBQUOTE):
            path = self.parse_quote_string()
        else:
            # Parse a non-quoted path, which will generally be comprised of more than one token.
            path = ""
            while self.match_any(
                Tokens.IDENT, Tokens.DOT, Tokens.SLASH, Tokens.BACKSLASH
            ):
                path += self.cur.val
        return path

    def parse_param_statement(self) -> ParamDecls:
        """ Parse a Parameter-Declaration Statement """
        self.expect(Tokens.PARAM)
        vals = self.parse_param_declarations()  # NEWLINE is captured inside
        return ParamDecls(vals)

    def parse_model(self) -> Union[ast.ModelDef, ast.ModelVariant]:
        """ Parse SPICE .model Statements """
        from .base import _endargs_startkwargs

        self.expect(Tokens.MODEL)

        if self.match(Tokens.MODEL_VARIANT):  # `model.variant` ModelVariant form
            spl = self.cur.val.split(".")
            if len(spl) != 2:
                self.fail(f"Invalid model-variant name: {spl.join('.')}")
            mname = Ident(spl[0])
            variant = Ident(str(spl[1]))
            mtype = self.parse_ident()
            partial = lambda args_and_params: ast.ModelVariant(
                mname, variant, mtype, *args_and_params
            )

        else:  # Single ModelDef
            mname = self.parse_ident()
            mtype = self.parse_ident()
            partial = lambda args_and_params: ast.ModelDef(
                mname, mtype, *args_and_params
            )

        # Get to some work shared among the two: parsing positional args and by-keyword params
        args = self.parse_ident_list(_endargs_startkwargs)

        # If we landed on a key-value param key, rewind it
        if self.nxt and self.nxt.tp == Tokens.EQUALS:
            self.rewind()
            args.pop()

        # Model parameters (apparently) get optional parentheses around them.
        if self.match(Tokens.LPAREN):
            term = lambda s: s.nxt is None or s.match(Tokens.RPAREN)
            params = self.parse_list(
                self.parse_param_declaration, term=term, MAXN=100_000
            )
            self.expect(Tokens.NEWLINE)
        else:  # NEWLINE delimited parameter-declarations
            params = self.parse_param_declarations()

        # Call that closure we created, making either a `ModelDef` or `ModelVariant`
        return partial((args, params))

    def parse_options(self):
        self.match(Tokens.OPTION)
        vals = self.parse_option_values()
        self.match(Tokens.NEWLINE)
        return Options(name=None, vals=vals)

    def are_stars_comments_now(self) -> bool:
        from .base import ParserState

        return self.state != ParserState.EXPR


class NgSpiceDialectParser(SpiceDialectParser):
    # FIXME: actually specialize!
    enum = NetlistDialects.NGSPICE


class HspiceDialectParser(SpiceDialectParser):
    # FIXME: actually specialize!
    enum = NetlistDialects.HSPICE
