from ..data import * 
from .spice import Dialect, SpiceDialect

class SpectreMixin:
    """ Misc Spectre-Stuff to be mixed-in """

    COMMENT_CHARS = ["*", "//"]

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

    def parse_stmt(self, line: str):
        """ Inject a few rules specific to Spectre-Spice """
        p = self.parse_comment(line)
        if p is not None:
            return p
        p = self.parse_dialect_change(line)
        if p is not None:
            return p
        return super().parse_stmt(line)


class SpectreDialect(SpectreMixin, Dialect):
    """ Spectre-Language Dialect. 
    Probably more of a separate language really, but it fits our Dialect paradigm well enough. """

    enum = NetlistDialects.SPECTRE

    HIER_PATH_SEP = "."
    CONTINUATION_CHAR = "+"
    COMMENT_CHAR = "*"

    IDENT_START_RE = rf"[A-Za-z_]"  # Valid characters to start an identifier
    IDENT_CONT_RE = rf"[A-Za-z0-9_]"  # Valid (non-start) characters in identifiers
    IDENT_RE = rf"{IDENT_START_RE}{IDENT_CONT_RE}*"
    IDENTS_LIST_RE = (
        rf"({IDENT_RE}\s+)+{IDENT_RE}?"  # FIXME: (\s+|$)) Form: name1 name2 a0 _b etc
    )
    HIER_IDENT_RE = rf"({IDENT_RE})(\.{IDENT_RE})*"
    MODEL_NAME_RE = rf"({IDENT_RE})(\.([A-Za-z_0-9]*))?"  # E.g. `nmos`, `nmos.0`
    NODE_NAME_RE = rf"([A-Za-z0-9_:]+)"  # Node names are much more foregiving. `i:`, `___`, `123`, etc are all valid
    NODE_LIST_RE = rf"({NODE_NAME_RE})(\s+{NODE_NAME_RE})*"
    PORT_CONN_RE = rf"({NODE_LIST_RE}|\(\s*{NODE_LIST_RE}\s*\))"  # Optionally paren-surrounded node-list, e.g. `d g s b`, `(d g s b)`

    EXPR_RE = r"([^=]+\n|[^\s=]+|'[^=]+'|{[^=]+})"  # Parameter expression, inside or outside bracketing
    EXPR_LIST_RE = rf"({EXPR_RE})(\s+{EXPR_RE})*"  # FIXME: (\s+|$)
    UNITS_RE = rf"$([.*])\n"
    PARAM_SET_RE = rf"({IDENT_RE})\s*\=\s*({EXPR_RE})"  ## FIXME: units ## (\s*|({UNITS_RE})?)' # Form a=5 b=6 c=7
    PARAM_KWARGS_RE = rf"({PARAM_SET_RE})(\s+{PARAM_SET_RE})*"
    IDENTS_AND_PARAMS_RE = rf"({IDENTS_LIST_RE})({PARAM_KWARGS_RE})?"  # Form: name arg1 arg2 arg3 a=3 b=4 c=5
    PARAMS_ARG_AND_KWARG_RE = rf"({EXPR_LIST_RE})\s+({PARAM_KWARGS_RE})?"  # Form: 'expr' 'expr1*expr2' '1e-9' a=3 b='16-11' c=0

    def parse_stmt(self, line: str):
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. """

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
        Just soaks up everything between its outer squiggly-brackets as a string, for now. """
        m = re.match("statistics", line, re.M)
        if m is None:
            return None
        return StatisticsBlock(self.parse_bracketed(line))

    def parse_model_def(self, line: str):
        # FIXME: parsing as `Unknown`, bracketed text
        m = re.match("model", line, re.M)
        if m is None:
            return None
        return Unknown(self.parse_bracketed(line))

    def parse_bracketed(self, line: str) -> str:
        """ Parse multi-line bracketed text """
        txt = line[:]
        depth = txt.count("{") - txt.count("}")
        while depth > 0:
            nxt = self.parent.advance()
            txt += nxt[:]
            depth = txt.count("{") - txt.count("}")
        return txt

    SUBCKT_LEFT_RE = rf"(inline\s+)?(subckt)\s+({IDENT_RE})\s+({PORT_CONN_RE})"
    SUBCKT_START_RE = rf"({SUBCKT_LEFT_RE})(\s+{PARAM_KWARGS_RE})?\s*$"

    def parse_subckt_start(self, line: str) -> Optional[StartSubckt]:
        m = re.match(self.SUBCKT_START_RE, line, re.M)
        if m is None:
            return None
        left = m.group(1)
        rest = line.replace(left, "")
        params = self.parse_param_values(rest)

        ml = re.match(self.SUBCKT_LEFT_RE, left, re.M)
        if ml is None:
            raise NetlistParseError
        name = Ident(ml.group(3))
        ports = [Ident(p) for p in ml.group(4).split()]
        return StartSubckt(name, ports, params)

    def parse_subckt_end(self, txt: str) -> Optional[EndSubckt]:
        SUBCKT_END_RE = rf"(ends)\s+({self.IDENT_RE})\s*$"
        m = re.match(re.compile(SUBCKT_END_RE), txt)
        if m is None:
            return None
        name = m.group(1)
        ident = None if not name else name
        return EndSubckt(Ident(ident))

    PARAM_DECL_RE = rf"\s*parameters\s+"

    def parse_param_decls(self, line: str):
        m = re.match(self.PARAM_DECL_RE, line, re.M)
        if m is None:
            return None
        txt = line.lstrip().lstrip("parameters").lstrip()
        return self.parse_param_values(txt)

    # Instance expressions
    INST_LEFT_RE = rf"({IDENT_RE})\s+({PORT_CONN_RE})\s+({IDENT_RE})"
    INSTANCE_RE = rf"({INST_LEFT_RE})\s+({PARAM_KWARGS_RE})\s*$"

    def parse_idents_and_params(self, txt: str) -> (List[Ident], ParamDecls):
        """ Parses (fairly common) strings of the form `xabc a b c mymodel d=1 e=2 f=3.9e19 """
        m = re.match(re.compile(self.IDENTS_AND_PARAMS_RE), txt)
        if m is None:
            raise NetlistParseError
        names = m.group(1)
        rest = txt.replace(names, "")
        names = [Ident(s) for s in names.split()]
        params = self.parse_param_values(rest)
        return (names, params)

    @classmethod
    def parse_options(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_lib(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_inc(cls, line: str):
        raise NotImplementedError

