from ..data import *
from .base import Dialect


class SpiceDialect(Dialect):
    """ Family of SPICE-like syntax dialects, complete with 
    * Dot-based "control cards", e.g. `.subckt`, `.ac`, `.end`, 
    * Prefix-based primitive element instances
    Further specializations are made for HSPICE, NGSPICE, etc. 
    """

    HIER_PATH_SEP = "."
    CONTINUATION_CHAR = "+"
    COMMENT_CHAR = "*"

    IDENT_START_RE = rf"[A-Za-z_]"  # Valid characters to start an identifier
    IDENT_CONT_RE = rf"[A-Za-z0-9_]"  # Valid (non-start) characters in identifiers
    IDENT_RE = rf"{IDENT_START_RE}{IDENT_CONT_RE}*"
    IDENTS_LIST_RE = rf"\s*({IDENT_RE}\s+)+({IDENT_RE}(\s+|$))?"
    HIER_IDENT_RE = rf"({IDENT_RE})(\.{IDENT_RE})*"
    MODEL_NAME_RE = rf"({IDENT_RE})(\.([A-Za-z_0-9]*))?"  # E.g. `nmos`, `nmos.0`
    NODE_NAME_RE = rf"([A-Za-z0-9_:]+)"  # Node names are much more foregiving. `i:`, `___`, `123`, etc are all valid

    EXPR_RE = r"([^=]+\n|[^\s=]+|'[^=]+'|{[^=]+})"  # Parameter expression, inside or outside bracketing
    EXPR_LIST_RE = rf"({EXPR_RE})(\s+{EXPR_RE})*"  # FIXME: (\s+|$)
    UNITS_RE = rf"$([.*])\n"
    PARAM_SET_RE = rf"({IDENT_RE})\s*\=\s*({EXPR_RE})"  ## FIXME: units ## (\s*|({UNITS_RE})?)' # Form a=5 b=6 c=7
    PARAM_KWARGS_RE = rf"({PARAM_SET_RE})(\s+{PARAM_SET_RE})*"

    IDENTS_AND_PARAMS_RE = rf"({IDENTS_LIST_RE})({PARAM_KWARGS_RE})?"  # Form: name arg1 arg2 arg3 a=3 b=4 c=5
    PARAMS_ARG_AND_KWARG_RE = rf"({EXPR_LIST_RE})\s+({PARAM_KWARGS_RE})?"  # Form: 'expr' 'expr1*expr2' '1e-9' a=3 b='16-11' c=0

    MODEL_DEF_RE = rf"(.model|.MODEL)\s+({MODEL_NAME_RE})\s+({IDENTS_AND_PARAMS_RE})"
    SUBCKT_START_RE = rf"(.subckt|.SUBCKT)\s+({IDENTS_AND_PARAMS_RE})"
    SUBCKT_END_RE = rf"(.ends|.ENDS)(\s+{IDENT_RE}\s*)?"

    def parse_stmt(self, lines: List[str]):
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. """

        # Mix this up in our new and old-style ways 
        line = ''.join(lines)
        oldline = lines[0].rstrip() + ' '.join(l[1:].rstrip() for l in lines[1:])

        # Comments get highest priority
        if lines[0].strip().startswith(self.COMMENT_CHAR):
            return self.parse_comment(line)

        # In-progress porting to `Optional[Statement]` return type,
        # Where `None` indicates no-match
        rules = [
            lambda l: self.parse_primitive(oldline),
            self.parse_instance,
            self.parse_subckt_start,
            self.parse_subckt_end,
        ]
        for rule in rules:
            s = rule(line)
            if s is not None:
                return s

        # More-manual, even-older-school rules
        lc = line.lower()
        if lc.startswith(".model"):
            return self.parse_model_def(oldline)
        if lc.startswith(".param"):
            return self.parse_param_decls(line)
        if lc.startswith(".inc"):
            return self.parse_inc(oldline)
        if lc.startswith(".option"):
            return self.parse_options(oldline)
        if lc.startswith(".lib"):
            return self.parse_dot_lib(oldline)
        NetlistParseError.throw(f"Invalid Statement: {line}")

    PRIMITIVE_LEFT_RE = rf"[RrCcIiVvDdMmQq]{IDENT_CONT_RE}+\s+"
    # PRIMITIVE_LEFT_RE = rf"([RrCcIiVvDdMmQq]{IDENT_CONT_RE}+\s+{EXPR_LIST_RE})"
    # PRIMITIVE_RE = rf"({PRIMITIVE_LEFT_RE})(\s+{PARAM_KWARGS_RE})?\s*$"

    INSTANCE_RE = rf"[Xx]{IDENT_CONT_RE}+"

    def parse_primitive(self, txt: str) -> Optional[Primitive]:
        """ Parse a Primitive Instance """
        m = re.match(re.compile(self.PRIMITIVE_LEFT_RE), txt)
        if m is None:
            return None

        return Primitive(name=Ident("FAKE"), args=[], kwargs=[]) # FIXME! 
        exprs = m.group(1)
        rest = txt.replace(exprs, "")
        kwargs = self.parse_param_values(rest)
        exprs = exprs.split()
        name = Ident(exprs.pop(0))

        from .. import LineParser

        args = [LineParser(s, self).parse() for s in exprs]
        return Primitive(name, args, kwargs)


    @classmethod
    def parse_subckt_end(cls, txt: str) -> EndSubckt:
        m = re.match(r"\.ends|\.ENDS", txt)
        if m is None:
            return None

        name = txt.replace(".ends", "").replace(".ENDS", "").strip()
        ident = None if not name else name
        return EndSubckt(Ident(ident))

    def parse_subckt_start(self, line: str) -> Optional[StartSubckt]:
        m = re.match(r"\.subckt|\.SUBCKT", line)
        if m is None:
            return None 

        txt = line.lstrip().lstrip(".subckt").lstrip(".SUBCKT")

        from .. import LineParser

        p = LineParser("subckt " + txt, self) # FIXME! this cheater Spectre-method here 
        return p.parse(p.parse_subckt_start) 

    @classmethod
    def parse_hier_path(cls, txt: str):
        return HierPath([Ident(i) for i in txt.split(cls.HIER_PATH_SEP)])

    def parse_model_def(self, line: str) -> Union[ModelDef, ModelVariant]:
        return ModelDef(Ident("fake"), mtype=Ident("fakemodel"), args=[], params=[]) # FIXME !



        txt = line.replace(".model", "").replace(".MODEL", "")
        spl = txt.split()
        fullname = spl[0].strip()
        mtype = Ident(spl[1].strip())
        args = [Ident(s.strip()) for s in spl[2:]]
        rest = " ".join(spl[2:])
        params = self.parse_param_declarations(rest)

        # Split the name into potential variants 
        namepath = fullname.split('.')
        if len(namepath) == 2:
            return ModelVariant(model=Ident(namepath[0]), variant=Ident(namepath[1]), args=args, params=params) 
        elif len(namepath) == 1:
            return ModelDef(Ident(fullname), mtype, args, params)
        NetlistParseError.throw() # Invalid Name 

    def parse_param_decls(self, line: str):
        txt = line.lower().replace(".param", "")
        return ParamDecls(self.parse_param_declarations(txt))

    def parse_options(self, line: str):
        txt = line.lower().replace(".option", "")
        vals = self.parse_param_values(txt)
        return Options(vals)

    @classmethod
    def parse_dot_lib(cls, line: str):
        """ Parse a line beginning with `.lib`, which may be *defining* or *using* the library! """
        parts = line.split()
        if parts[0].lower() != ".lib":
            NetlistParseError.throw()
        if len(parts) == 2:
            return StartLib(Ident(parts[1]))
        elif len(parts) == 3:
            return UseLib(path=Path(parts[1], section=Ident(parts[2])))
        NetlistParseError.throw()

    @classmethod
    def parse_inc(cls, line: str):
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

    @classmethod
    def parse_comment(cls, line: str):
        return Comment(line.strip().lstrip(cls.COMMENT_CHAR))


class NgSpiceDialect(SpiceDialect):
    # FIXME: actually specialize!
    enum = NetlistDialects.NGSPICE

