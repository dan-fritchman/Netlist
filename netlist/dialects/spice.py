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
            }
            pk = self.peek()
            for tok, func in rules.items():
                if pk.tp == tok:
                    return func()
            self.fail(f"Invalid or unsupported dot-statement: {pk}")

        elif pk.tp == Tokens.IDENT:
            if pk.val.lower().startswith("x"):
                return self.parse_instance()
            return self.parse_primitive()

        # No match - error time.
        self.fail(f"Unexpected token to begin statement: {pk}")

    def parse_include(self) -> Include:
        """Parse an Include Statement"""
        self.expect(Tokens.INC, Tokens.INCLUDE)
        if self.peek().tp in (Tokens.TICK, Tokens.DUBQUOTE):
            path = self.parse_quote_string()
        else:
            # Parse a non-quoted path, which will generally be comprised of more than one token.
            path = ""
            while self.match_any(
                Tokens.IDENT, Tokens.DOT, Tokens.SLASH, Tokens.BACKSLASH
            ):
                path += self.cur.val
        self.expect(Tokens.NEWLINE)
        return Include(path)

    def parse_param_statement(self) -> ParamDecls:
        """Parse a Parameter-Declaration Statement"""
        self.expect(Tokens.PARAM)
        vals = self.parse_param_declarations()  # NEWLINE is captured inside
        return ParamDecls(vals)

    def parse_model(self) -> Union[ModelDef, ModelVariant]:
        """Parse SPICE .model Statements"""
        from .base import _endargs_startkwargs

        self.expect(Tokens.MODEL)

        if self.match(Tokens.MODEL_VARIANT):  # `model.variant` ModelVariant form
            spl = self.cur.val.split(".")
            if len(spl) != 2:
                self.fail()
            mname = Ident(spl[0])
            variant = Ident(str(spl[1]))
            self.expect(Tokens.IDENT)
            mtype = Ident(self.cur.val)

            args = self.parse_ident_list(_endargs_startkwargs)
            # If we landed on a key-value param key, rewind it
            if self.nxt and self.nxt.tp == Tokens.EQUALS:
                self.rewind()
                args.pop()
            params = self.parse_param_declarations()
            return ModelVariant(mname, variant, mtype, args, params)

        # Single ModelDef
        self.expect(Tokens.IDENT)
        mname = Ident(self.cur.val)
        self.expect(Tokens.IDENT)
        mtype = Ident(self.cur.val)
        args = self.parse_ident_list(_endargs_startkwargs)
        # If we landed on a key-value param key, rewind it
        if self.nxt and self.nxt.tp == Tokens.EQUALS:
            self.rewind()
            args.pop()
        params = self.parse_param_declarations()
        return ModelDef(mname, mtype, args, params)

    def parse_options(self):
        self.match(Tokens.OPTION)
        vals = self.parse_param_values()
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
