from ..data import *
from .spice import DialectParser, SpiceDialectParser
from .base import Tokens


class SpectreMixin:
    """ Spectre-stuff to be mixed-in, 
    primarily related to the capacity for `DialectChanges` 
    via a `simulator lang` statement. """

    COMMENT_CHARS = ["*", "//", "$"]

    def parse_dialect_change(self) -> Optional[DialectChange]:
        """ Parse a DialectChange. Leaves its trailing NEWLINE to be parsed by a (likely new) DialectParser. """

        self.expect(Tokens.SIMULATOR)
        self.expect(Tokens.LANG)
        self.expect(Tokens.EQUALS)
        self.expect(Tokens.IDENT)
        d = DialectChange(self.cur.val)
        # self.expect(Tokens.NEWLINE) # Note this is left for the *new* dialect to parse

        self.parent.notify(d)
        return d


class SpectreSpiceDialectParser(SpectreMixin, SpiceDialectParser):
    """ Spice-Style Syntax, as Interpreted by Spectre """

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
            Tokens.IDENT: self.parse_instance,
        }
        for tok, func in rules.items():
            if pk.tp == tok:
                return func()

        # No match - error time.
        NetlistParseError.throw()

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
        return ParamDecls(vals)

    def parse_statistics_block(self) -> StatisticsBlock:
        """ Parse the `statistics` block, kinda. 
        FIXME: Just soaks up everything between its outer squiggly-brackets as a string, for now. """

        self.expect(Tokens.STATS)
        self.expect(Tokens.LBRACKET)
        self.expect(Tokens.NEWLINE)
        brack_count = 1
        while brack_count > 0:
            if self.match(Tokens.RBRACKET):
                brack_count -= 1
            elif self.match(Tokens.LBRACKET):
                brack_count += 1
            else:
                self.advance()
        self.expect(Tokens.NEWLINE)
        return Unknown("STATS")

    def parse_ahdl(self):
        self.expect(Tokens.AHDL)
        self.expect(Tokens.QUOTESTR)
        rv = AhdlInclude(self.cur.val)
        self.expect(Tokens.NEWLINE)
        return rv

    def are_stars_comments_now(self) -> bool:
        # Stars are comments only to begin lines. (?)
        return self.cur and self.cur.tp == Tokens.NEWLINE

    def parse_options(self):
        raise NotImplementedError

    def parse_lib(self):
        raise NotImplementedError

    def parse_inc(self):
        raise NotImplementedError

