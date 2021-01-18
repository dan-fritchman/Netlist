from ..data import *
from .base import DialectParser, Tokens


class SpiceDialectParser(DialectParser):
    """ Family of SPICE-like syntax dialects, complete with 
    * Dot-based "control cards", e.g. `.subckt`, `.ac`, `.end`, 
    * Prefix-based primitive element instances
    Further specializations are made for HSPICE, NGSPICE, etc. 
    """

    def parse_statement(self) -> Optional[Statement]:
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
            }
            pk = self.peek()
            for tok, func in rules.items():
                if pk.tp == tok:
                    return func()

        elif pk.tp == Tokens.IDENT:
            if pk.val.lower().startswith("x"):
                return self.parse_instance()
            return self.parse_primitive()

        # No match - error time.
        NetlistParseError.throw()

    def parse_include(self) -> Include:
        """ Parse an Include Statement """
        self.expect(Tokens.INC, Tokens.INCLUDE)
        self.expect(Tokens.QUOTESTR)
        path = self.cur.val[1:-1]  # Strip out the quotes
        self.expect(Tokens.NEWLINE)
        return Include(path)

    def parse_param_statement(self) -> ParamDecls:
        """ Parse a Parameter-Declaration Statement """
        self.expect(Tokens.PARAM)
        vals = self.parse_param_declarations()  # NEWLINE is captured inside
        return ParamDecls(vals)

    def parse_model(self) -> Union[ModelDef, ModelVariant]:
        """ Parse SPICE .model Statements """
        from .base import _endargs_startkwargs

        self.expect(Tokens.MODEL)

        if self.match(Tokens.MODEL_VARIANT):  # `model.variant` ModelVariant form
            spl = self.cur.val.split(".")
            if len(spl) != 2:
                NetlistParseError.throw()
            mname = Ident(spl[0])
            variant = Ident(str(spl[1]))
            self.expect(Tokens.IDENT)  # FIXME: is this defined here or elsewhere?
            mtype = Ident(self.cur.val)  # FIXME: Not stored, at least for now

            args = self.parse_ident_list(_endargs_startkwargs)
            # If we landed on a key-value param key, rewind it
            if self.nxt and self.nxt.tp == Tokens.EQUALS:
                self.rewind()
                args.pop()
            params = self.parse_param_declarations()
            return ModelVariant(mname, variant, args, params)

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
        return Options(vals)

    def parse_dot_lib(self):
        """ Parse a line beginning with `.lib`, which may be *defining* or *using* the library! """
        parts = line.split()
        if parts[0].lower() != ".lib":
            NetlistParseError.throw()
        if len(parts) == 2:
            return StartLib(Ident(parts[1]))
        elif len(parts) == 3:
            return UseLib(path=Path(parts[1], section=Ident(parts[2])))
        NetlistParseError.throw()

    def parse_inc(self):
        txt = line.replace(".include", "").replace(".inc", "").strip()
        if txt.startswith('"'):
            if txt.endswith('"'):
                return Include(Path(txt[1:-1]))
            else:
                NetlistParseError.throw("Unclosed String")
        if txt.startswith("'"):
            if txt.endswith("'"):
                return Include(Path(txt[1:-1]))
            else:
                NetlistParseError.throw("Unclosed String")
        return Include(Path(txt))

    def are_stars_comments_now(self) -> bool:
        from .base import ParserState

        return self.state != ParserState.EXPR

    def parse_expr(self) -> Expr:
        """ Parse an Expression 
        expr0 | 'expr0' | {expr0} """
        # Note: moves into our `EXPR` state require a `peek`/`expect` combo,
        # otherwise we can mis-understand multiplication vs comment.
        from .base import ParserState

        if self.nxt and self.nxt.tp == Tokens.TICK:
            self.state = ParserState.EXPR  # Note: this comes first
            self.expect(Tokens.TICK)
            e = self.parse_expr0()
            self.state = ParserState.PROGRAM  # Note: this comes first
            self.expect(Tokens.TICK)
            return e
        if self.nxt and self.nxt.tp == Tokens.LBRACKET:
            self.state = ParserState.EXPR  # Note: this comes first
            self.expect(Tokens.LBRACKET)
            e = self.parse_expr0()
            self.state = ParserState.PROGRAM  # Note: this comes first
            self.expect(Tokens.RBRACKET)
            return e
        return self.parse_expr0()


class NgSpiceDialectParser(SpiceDialectParser):
    # FIXME: actually specialize!
    enum = NetlistDialects.NGSPICE

