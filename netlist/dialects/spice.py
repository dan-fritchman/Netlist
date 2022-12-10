"""
# Spice-Dialect Parsing 
"""
from typing import Optional, Union

# Local Imports
from ..data import *
from .base import DialectParser, Tokens


class SpiceDialectParser(DialectParser):
    """Family of SPICE-like syntax dialects, complete with
    * Dot-based "control cards", e.g. `.subckt`, `.ac`, `.end`,
    * Prefix-based primitive element instances
    Further specializations are made for HSPICE, NGSPICE, etc.
    """

    enum = NetlistDialects.SPICE

    def parse_statement(self) -> Optional[Statement]:
        """Statement Parser
        Dispatches to type-specific parsers based on prioritized set of matching rules.
        Returns `None` at end."""

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

    def parse_lib_statement(self) -> Union[StartLibSection, UseLibSection]:
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
            return UseLibSection(path=first_arg, section=section)

        # Two-argument form. Create a `StartLibSection`
        self.expect(Tokens.NEWLINE)
        return StartLibSection(Ident(first_arg))

    def parse_include(self) -> Include:
        """Parse an Include Statement"""
        self.expect(Tokens.INC, Tokens.INCLUDE)
        path = self.parse_path()
        self.expect(Tokens.NEWLINE)
        return Include(path)

    def parse_endl(self) -> EndLibSection:
        """Parse an end-lib-section Statement"""
        self.expect(Tokens.ENDL)
        name = self.parse_ident()  # FIXME: is this optional?
        self.expect(Tokens.NEWLINE)
        return EndLibSection(name)

    def parse_path(self) -> str:
        """Parse a file-system path, either quote-delimited or not"""
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

    def parse_param_statement(self) -> Union[ParamDecls, FunctionDef]:
        """
        Parse a `.param` statement, which defines one of:
        * (a) A set of parameter-declaration `ParamDecl`s, or
        * (b) A *single* "parameter function" `FunctionDef`.

        Netlist syntax mixing the two, e.g.
        ```
        .param a=5 b=6 func(x,y) 'x*a +y*b'
        ```
        is not supported.
        """
        self.expect(Tokens.PARAM)

        # Parse the first key-name, so we can see what follows
        _ = self.parse_ident()
        if self.nxt and self.nxt.tp == Tokens.LPAREN:
            # This is a function definition.
            returnfunc = self.parse_function_def
        else:  # Otherwise, parse a set of parameter-declarations
            returnfunc = lambda: ParamDecls(self.parse_param_declarations())

        # Either way, push the first ident back on before calling `returnfunc`
        self.rewind()
        # And call the parsing function for either the `ParamDecls` or `FunctionDef`
        return returnfunc()

    def parse_model(self) -> Union[ModelDef, ModelVariant]:
        """Parse SPICE .model Statements"""
        from .base import _endargs_startkwargs

        self.expect(Tokens.MODEL)

        if self.match(Tokens.MODEL_VARIANT):  # `model.variant` ModelVariant form
            spl = self.cur.val.split(".")
            if len(spl) != 2:
                self.fail(f"Invalid model-variant name: {spl.join('.')}")
            mname = Ident(spl[0])
            variant = Ident(str(spl[1]))
            mtype = self.parse_ident()
            partial = lambda args_and_params: ModelVariant(
                mname, variant, mtype, *args_and_params
            )

        else:  # Single ModelDef
            mname = self.parse_ident()
            mtype = self.parse_ident()
            partial = lambda args_and_params: ModelDef(mname, mtype, *args_and_params)

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

    def parse_options(self) -> Options:
        self.match(Tokens.OPTION)
        vals = self.parse_option_values()
        self.match(Tokens.NEWLINE)
        return Options(name=None, vals=vals)

    def parse_function_def(self) -> FunctionDef:
        """Yes, Spice netlists (or at least *some* versions thereof) do have function definitions!
        Sort of. They are more like Python's lambda-functions in being limited to a single-line, single-expression.

        Syntax: `funcname (argname1, argname2) 'return_expr'`

        While arguments are stored as `TypedArg`s, all of their `tp` fields are set to `ArgType.UNKNOWN`.
        """
        name = self.parse_ident()
        self.expect(Tokens.LPAREN)
        # Parse arguments
        args = []
        MAX_ARGS = 100  # Set a "time-out" so that we don't get stuck here.
        for i in range(MAX_ARGS, -1, -1):
            if self.match(Tokens.RPAREN):
                break  # Note we can have zero-argument cases, I guess.
            a = TypedArg(tp=ArgType.UNKNOWN, name=self.parse_ident())
            args.append(a)
            if self.match(Tokens.RPAREN):
                break
            self.expect(Tokens.COMMA)
        if i <= 0:  # Check the time-out
            self.fail(f"Unable to parse argument list for spice-function {name.name}")

        self.expect(Tokens.EQUALS)
        self.expect(Tokens.TICK)
        # Parse the return-expression
        ret = Return(self.parse_expr())
        self.expect(Tokens.TICK)
        self.expect(Tokens.NEWLINE)

        return FunctionDef(name=name, rtype=ArgType.UNKNOWN, args=args, stmts=[ret])

    def are_stars_comments_now(self) -> bool:
        from .base import ParserState

        return self.state != ParserState.EXPR


class NgSpiceDialectParser(SpiceDialectParser):
    # FIXME: actually specialize!
    enum = NetlistDialects.NGSPICE


class HspiceDialectParser(SpiceDialectParser):
    # FIXME: actually specialize!
    enum = NetlistDialects.HSPICE
