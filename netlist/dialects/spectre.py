from ..data import *
from .spice import Dialect, SpiceDialect


class SpectreMixin:
    """ Misc Spectre-Stuff to be mixed-in """

    COMMENT_CHARS = ["*", "//", "$"]

    @classmethod
    def is_comment(cls, s: str) -> bool:
        return any([s.startswith(c) for c in cls.COMMENT_CHARS])

    @classmethod
    def parse_comment(cls, line: str):
        if not cls.is_comment(line):
            return None
        return Comment(line.strip())

    def parse_dialect_change(self, txt: str) -> Optional[DialectChange]:
        """ Potentially parse a DialectChange. 
        If successful, notify our parent and then return it. 
        If DialectChange does not match, returns `None`. """
        DIALECT_CHANGE_RE = rf"\s*simulator\s+lang\s*=\s*({self.IDENT_RE})\s*$"
        m = re.match(re.compile(DIALECT_CHANGE_RE), txt)
        if m is None:
            return None
        lang = m.group(1)
        d = DialectChange(lang)
        self.parent.notify(d)
        return d


class SpectreSpiceDialect(SpectreMixin, SpiceDialect):
    """ Spice-Style Syntax, as Interpreted by Spectre """

    enum = NetlistDialects.SPECTRE_SPICE

    def parse_stmt(self, lines: List[str]):
        """ Inject a few rules specific to Spectre-Spice """
        p = self.parse_comment(''.join(lines))
        if p is not None:
            return p
        p = self.parse_dialect_change(''.join(lines))
        if p is not None:
            return p
        return super().parse_stmt(lines)


class SpectreDialect(SpectreMixin, Dialect):
    """ Spectre-Language Dialect. 
    Probably more of a separate language really, but it fits our Dialect paradigm well enough. """

    enum = NetlistDialects.SPECTRE

    HIER_PATH_SEP = "."
    CONTINUATION_CHAR = "+"

    IDENT_START_RE = rf"[A-Za-z_]"  # Valid characters to start an identifier
    IDENT_CONT_RE = rf"[A-Za-z0-9_]"  # Valid (non-start) characters in identifiers
    IDENT_RE = rf"{IDENT_START_RE}{IDENT_CONT_RE}*"
    HIER_IDENT_RE = rf"({IDENT_RE})(\.{IDENT_RE})*"

    def parse_stmt(self, lines: List[str]):
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. """

        line = ''.join(lines)

        if line.startswith('ahdl'): 
            return AhdlInclude(Path("???")) # FIXME!

        rules = [
            self.parse_comment,
            self.parse_dialect_change,
            self.parse_param_decls,
            self.parse_subckt_start,
            self.parse_subckt_end,
            self.parse_model_def,
            self.parse_stats,
            self.parse_instance,
        ]
        for rule in rules:
            stmt = rule(line)
            if stmt is not None:
                return stmt
        raise NetlistParseError

    def parse_stats(self, line: str) -> Optional[StatisticsBlock]:
        """ Parse the `statistics` block, kinda. 
        FIXME: Just soaks up everything between its outer squiggly-brackets as a string, for now. """
        m = re.match("statistics", line, re.M)
        if m is None:
            return None
        return StatisticsBlock(self.parse_bracketed(line))

    def parse_model_def(self, line: str) -> Union[ModelDef, ModelFamily]:
        m = re.match("model", line, re.M)
        if m is None:
            return None

        from .. import LineParser

        txt = self.parse_bracketed(line)
        p = LineParser(txt, self)
        return p.parse(p.parse_model)

    def parse_bracketed(self, line: str) -> str:
        """ Parse multi-line bracketed text """
        txt = line[:]
        depth = txt.count("{") - txt.count("}")
        while depth > 0:
            nxt = self.parent.advance()
            txt += nxt[:]
            depth = txt.count("{") - txt.count("}")
        return txt

    SUBCKT_START_RE = rf"(inline\s+)?(subckt)"

    def parse_subckt_start(self, line: str) -> Optional[StartSubckt]:
        m = re.match(self.SUBCKT_START_RE, line, re.M)
        if m is None:
            return None

        from .. import LineParser

        p = LineParser(line, self)
        return p.parse(p.parse_subckt_start)

    def parse_subckt_end(self, txt: str) -> Optional[EndSubckt]:
        SUBCKT_END_RE = rf"ends"
        m = re.match(re.compile(SUBCKT_END_RE), txt)
        if m is None:
            return None

        
        from .. import LineParser

        p = LineParser(txt, self)
        return p.parse(p.parse_end_sub) 

    PARAM_DECL_RE = rf"\s*parameters\s+"

    def parse_param_decls(self, line: str) -> Optional[ParamDecls]:
        m = re.match(self.PARAM_DECL_RE, line, re.M)
        if m is None:
            return None
        txt = line.lstrip().lstrip("parameters") 
        return ParamDecls(self.parse_param_declarations(txt))

    # Instance expressions
    INSTANCE_RE = IDENT_RE

    @classmethod
    def parse_options(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_lib(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_inc(cls, line: str):
        raise NotImplementedError

