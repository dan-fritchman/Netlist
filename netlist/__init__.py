""" 
Netlists 

Library for interacting with, parsing, and generating popular formats of circuit netlist. 
"""

__version__ = "0.1.0"


import os
from pathlib import Path
import re


from pydantic.dataclasses import dataclass
from typing import Optional, Any, Dict, Union, List


def to_json(arg) -> str:
    """ Dump any `pydantic.dataclass` or simple combination thereof to JSON string. 
    (Unfortunately doesn't seem to like our `Dict` types yet.) """
    import json
    from pydantic.json import pydantic_encoder

    return json.dumps(arg, indent=2, default=pydantic_encoder)


from enum import Enum


class SpiceDialects(Enum):
    SPECTRE = 1
    SPECTRE_SPICE = 2
    SPICE = 3
    HSPICE = 4
    NGSPICE = 5


@dataclass
class SourceInfo:
    path: Path
    line: int
    dialect: SpiceDialects


@dataclass(eq=True, frozen=True)
class Ident:
    name: str


@dataclass
class HierPath:
    """ Hierarchical Path Identifier """

    path: List[Ident]


@dataclass
class ParamValue:
    val: Union[int, float, str]
    unit: Optional[str] = None


@dataclass
class Instance:
    """ Subckt/Module Instance """

    name: Ident
    module: Ident
    conns: Union[List[Ident], Dict[Ident, Ident]]
    params: Dict[Ident, Optional[ParamValue]]


@dataclass
class Primitive:
    """ Simulator-Defined Primitive Instance 
    Note at parsing-time, before models are sorted out, 
    it is not always clear what is a port, model name, and parameter value. 
    Primitives instead store positional and keyword arguments `args` and `kwargs`. """

    name: Ident
    args: List[ParamValue]
    kwargs: Dict[Ident, Optional[ParamValue]]


@dataclass
class ParamDecls:
    """ Parameter Declarations (One or More, in Dict form) """

    vals: Dict[Ident, Optional[ParamValue]]


@dataclass
class Options:
    vals: Dict[Ident, Optional[ParamValue]]


@dataclass
class StartSubckt:
    name: Ident
    ports: List[Ident]
    params: Dict[Ident, Optional[ParamValue]]


@dataclass
class EndSubckt:
    name: Optional[Ident]


@dataclass
class ModelDef:
    name: HierPath  # FIXME: may need to be specialized
    args: List[Ident]
    params: Dict[Ident, Optional[ParamValue]]


@dataclass
class Include:
    path: Path


@dataclass
class StartLib:
    name: Ident


@dataclass
class EndLib:
    name: Optional[Ident]


@dataclass
class UseLib:
    path: Path
    section: Ident


class End:
    """ Empty class represents `.end` Statements """

    ...


@dataclass
class StatisticsBlock:
    """ Statistical Descriptions. Largely un-parsed for now. """

    txt: str


@dataclass
class Unknown:
    """ Error/ Unknown Netlist Statement """

    txt: str


@dataclass
class Comment:
    txt: str


@dataclass
class DialectChange:
    """ Netlist Dialect Changes, e.g. `simulator lang=xyz` """

    dialect: str


Statement = Union[Instance, ModelDef, Comment]  # ... and a lot more things to come


@dataclass
class Entry:
    content: Any  # FIXME: Statement
    source_info: Optional[SourceInfo] = None


class NetlistParseError(Exception):
    ...


class Dialect:
    @classmethod
    def from_enum(cls, dialect: Optional["SpiceDialects"] = None):
        """ Return a Dialect sub-class based on the `SpiceDialects` enum. 
        Returns the default class if argument `dialect` is not provided or `None`. """
        if dialect is None:
            return SpectreDialect
        if dialect == SpiceDialects.SPECTRE:
            return SpectreDialect
        if dialect == SpiceDialects.SPECTRE_SPICE:
            return SpectreSpiceDialect
        if dialect == SpiceDialects.NGSPICE:
            return NgSpiceDialect
        raise ValueError

    def __init__(self, parent=None):
        self.parent = parent

    @classmethod
    def is_continuation(cls, s: str) -> bool:
        # Continuation-characters, comments, and blank-lines all manifest "continuation lines "
        return (
            s.startswith(cls.CONTINUATION_CHAR)
            or cls.is_comment(s)
            or not len(s.strip())
        )

    @classmethod
    def is_comment(cls, s: str) -> bool:
        return s.startswith(cls.COMMENT_CHAR)


class SpiceDialect(Dialect):
    """ Family of SPICE-like syntax dialects, complete with 
    * Dot-based "control cards", e.g. `.subckt`, `.ac`, `.end`, 
    * Prefix-based primitive element instances
    Further specializations are made for HSPICE, NGSPICE, etc. 
    """

    enum = None

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

    EXPR_RE = r"([^\s=]+|'[^=]+'|{[^=]+})"  # Parameter expression, inside or outside bracketing
    EXPR_LIST_RE = rf"({EXPR_RE})(\s+{EXPR_RE})*"
    UNITS_RE = rf"$([.*])\n"
    PARAM_SET_RE = rf"({IDENT_RE})\s*=\s*({EXPR_RE})(\s+\$.*\n)?"  ## FIXME: units ## (\s*|({UNITS_RE})?)' # Form a=5 b=6 c=7 $ comment
    PARAM_KWARGS_RE = rf"({PARAM_SET_RE})+"
    IDENTS_AND_PARAMS_RE = rf"({IDENTS_LIST_RE})({PARAM_KWARGS_RE})?"  # Form: name arg1 arg2 arg3 a=3 b=4 c=5
    PARAMS_ARG_AND_KWARG_RE = rf"({EXPR_LIST_RE})\s+({PARAM_KWARGS_RE})?"  # Form: 'expr' 'expr1*expr2' '1e-9' a=3 b='16-11' c=0

    MODEL_DEF_RE = rf"(.model|.MODEL)\s+({MODEL_NAME_RE})\s+({IDENTS_AND_PARAMS_RE})"
    SUBCKT_START_RE = rf"(.subckt|.SUBCKT)\s+({IDENTS_AND_PARAMS_RE})"
    SUBCKT_END_RE = rf"(.ends|.ENDS)(\s+{IDENT_RE}\s*)?"

    # Primitives are extra-fun, as many have optional ports, models, and parameters by-position! For now we just grab the all as Expressions
    PRIMITIVE_LEFT_RE = rf"([RrCcIiVvDdMmQq]{IDENT_CONT_RE}+\s+{EXPR_LIST_RE})"
    PRIMITIVE_RE = rf"({PRIMITIVE_LEFT_RE})\s+({PARAM_KWARGS_RE})?"
    # Module instances, in contrast, have the relative sanity of requiring all ports, and passing parameters by name
    INST_LEFT_RE = f"[Xx]({IDENT_CONT_RE}+)\s+({NODE_NAME_RE}\s+)+({IDENT_RE})"  # Form `xname p1 p2 p3 modulename`
    INSTANCE_RE = f"({INST_LEFT_RE})\s+({PARAM_KWARGS_RE})"  # Form `xname p1 p2 p3 modulename a=1 b=2 c=3`

    @classmethod
    def parse_stmt(cls, line: str):
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. """

        # Comments get highest priority
        if line.strip().startswith(cls.COMMENT_CHAR):
            return cls.parse_comment(line)

        # In-progress porting to `Optional[Statement]` return type,
        # Where `None` indicates no-match
        new_fangled_rules = [cls.parse_primitive]
        for rule in new_fangled_rules:
            s = rule(line)
            if s is not None:
                return s

        # Dictionary of rules, in priority-order
        rules = {
            cls.INSTANCE_RE: cls.parse_instance,
            cls.SUBCKT_START_RE: cls.parse_subckt_start,
            cls.SUBCKT_END_RE: cls.parse_subckt_end,
        }
        for pattern, func in rules.items():
            if re.match(re.compile(pattern), line):
                return func(line)

        # More-manual, older-school rules
        lc = line.lower()
        if lc.startswith(".inc"):
            return cls.parse_inc(line)
        if lc.startswith(".option"):
            return cls.parse_options(line)
        if lc.startswith(".param"):
            return cls.parse_param_decls(line)
        if lc.startswith(".lib"):
            return cls.parse_dot_lib(line)
        if lc.startswith(".model"):
            return cls.parse_model_def(line)
        raise NetlistParseError(f"Invalid Statement: {line}")

    @classmethod
    def parse_instance(cls, txt: str):
        """ Parse a Subckt/Module Instance """
        m = re.match(re.compile(cls.INST_LEFT_RE), txt)
        if m is None:
            raise NetlistParseError

        names = m.group(1)
        rest = txt.replace(names, "")
        params = cls.parse_param_values(rest)

        names = [Ident(s) for s in names.split()]
        name = names[0]
        conns = names[1:-1]
        module = names[-1]
        return Instance(name, module, conns, params)

    @classmethod
    def parse_primitive(cls, txt: str) -> Optional[Primitive]:
        """ Parse a Primitive Instance """
        m = re.match(re.compile(cls.PRIMITIVE_RE), txt)
        if m is None:
            return None
        exprs = m.group(1)
        rest = txt.replace(exprs, "")
        kwargs = cls.parse_param_values(rest)
        exprs = exprs.split()
        name = Ident(exprs.pop(0))
        args = [ParamValue(s) for s in exprs]
        return Primitive(name, args, kwargs)

    @classmethod
    def parse_param_values(cls, line: str) -> Dict[Ident, ParamValue]:
        return {
            Ident(i.group(1)): ParamValue(i.group(2))
            for i in re.compile(cls.PARAM_SET_RE).finditer(line)
        }

    @classmethod
    def parse_exprs_and_params(
        cls, txt: str
    ) -> (List[ParamValue], Dict[Ident, ParamValue]):
        m = re.match(re.compile(cls.PARAMS_ARG_AND_KWARG_RE), txt)
        if m is None:
            raise NetlistParseError
        exprs = m.group(1)
        rest = txt.replace(exprs, "")
        exprs = [ParamValue(s) for s in exprs.split()]
        params = cls.parse_param_values(rest)
        return (exprs, params)

    @classmethod
    def parse_idents_and_params(
        cls, txt: str
    ) -> (List[Ident], Dict[Ident, ParamValue]):
        """ Parsers (fairly common) strings of the form `xabc a b c mymodel d=1 e=2 f=3.9e19 """
        m = re.match(re.compile(cls.IDENTS_AND_PARAMS_RE), txt)
        if m is None:
            raise NetlistParseError
        names = m.group(1)
        rest = txt.replace(names, "")
        names = [Ident(s) for s in names.split()]
        params = cls.parse_param_values(rest)
        return (names, params)

    @classmethod
    def parse_subckt_end(cls, txt: str) -> EndSubckt:
        name = txt.replace(".ends", "").replace(".ENDS", "").strip()
        ident = None if not name else name
        return EndSubckt(Ident(ident))

    @classmethod
    def parse_subckt_start(cls, line: str):
        txt = line.replace(".subckt", "").replace(".SUBCKT", "")
        names, params = cls.parse_idents_and_params(txt)
        name = names[0]
        ports = names[1:]
        return StartSubckt(name, ports, params)

    @classmethod
    def parse_hier_path(cls, txt: str):
        return HierPath([Ident(i) for i in txt.split(cls.HIER_PATH_SEP)])

    @classmethod
    def parse_model_def(cls, line: str):
        txt = line.replace(".model", "").replace(".MODEL", "")
        spl = txt.split()
        name = cls.parse_hier_path(
            spl[0]
        )  # FIXME: this may require specialty processing for Identifiers such as `0` in `nmos.0`
        rest = " ".join(spl[1:])
        args, params = cls.parse_idents_and_params(rest)
        return ModelDef(name, args, params)

    @classmethod
    def parse_param_decls(cls, line: str):
        txt = line.lower().replace(".param", "")
        vals = cls.parse_param_values(txt)
        return ParamDecls(vals)

    @classmethod
    def parse_options(cls, line: str):
        txt = line.lower().replace(".option", "")
        vals = cls.parse_param_values(txt)
        return Options(vals)

    @classmethod
    def parse_dot_lib(cls, line: str):
        """ Parse a line beginning with `.lib`, which may be *defining* or *using* the library! """
        parts = line.split()
        if parts[0].lower() != ".lib":
            raise NetlistParseError
        if len(parts) == 2:
            return StartLib(Ident(parts[1]))
        elif len(parts) == 3:
            return UseLib(path=Path(parts[1], section=Ident(parts[2])))
        raise NetlistParseError

    @classmethod
    def parse_inc(cls, line: str):
        txt = line.replace(".include", "").replace(".inc", "").strip()
        if txt.startswith('"'):
            if txt.endswith('"'):
                return Include(Path(txt[1:-1]))
            else:
                raise NetlistParseError("Unclosed String")
        if txt.startswith("'"):
            if txt.endswith("'"):
                return Include(Path(txt[1:-1]))
            else:
                raise NetlistParseError("Unclosed String")
        return Include(Path(txt))

    @classmethod
    def parse_comment(cls, line: str):
        return Comment(line.strip().lstrip(cls.COMMENT_CHAR))


class NgSpiceDialect(SpiceDialect):
    # FIXME: actually specialize!
    enum = SpiceDialects.NGSPICE


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

    enum = SpiceDialects.SPECTRE_SPICE

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

    enum = SpiceDialects.SPECTRE

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

    MODEL_DEF_RE = rf"(.model|.MODEL)\s+({MODEL_NAME_RE})\s+({IDENTS_AND_PARAMS_RE})"

    SUBCKT_END_RE = rf"(.ends|.ENDS)(\s+{IDENT_RE}\s*)?"

    def parse_stmt(self, line: str):
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. """

        rules = [
            self.parse_comment,
            self.parse_dialect_change,
            self.parse_param_decls,
            self.parse_subckt_start,
            self.parse_subckt_end,
            self.parse_instance,
            self.parse_stats,
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

        txt = line[:]
        depth = txt.count("{") - txt.count("}")
        while depth > 0:
            nxt = self.parent.advance()
            txt += nxt[:]
            depth = txt.count("{") - txt.count("}")
        return StatisticsBlock(txt)

    SUBCKT_LEFT_RE = rf"(inline\s+)?(subckt)\s+({IDENT_RE})\s+({PORT_CONN_RE})"
    SUBCKT_START_RE = rf"({SUBCKT_LEFT_RE})(\s+{PARAM_KWARGS_RE})?\s*$"

    @classmethod
    def parse_subckt_start(cls, line: str) -> Optional[StartSubckt]:
        m = re.match(cls.SUBCKT_START_RE, line, re.M)
        if m is None:
            return None
        left = m.group(1)
        rest = line.replace(left, "")
        params = cls.parse_param_values(rest)

        ml = re.match(cls.SUBCKT_LEFT_RE, left, re.M)
        if ml is None:
            raise NetlistParseError
        name = Ident(ml.group(3))
        ports = [Ident(p) for p in ml.group(4).split()]
        return StartSubckt(name, ports, params)

    @classmethod
    def parse_subckt_end(cls, txt: str) -> Optional[EndSubckt]:
        SUBCKT_END_RE = rf"(ends)\s+({cls.IDENT_RE})\s*$"
        m = re.match(re.compile(SUBCKT_END_RE), txt)
        if m is None:
            return None
        name = m.group(1)
        ident = None if not name else name
        return EndSubckt(Ident(ident))

    PARAM_DECL_RE = rf"(parameters)\s+({PARAM_KWARGS_RE})\s*$"

    @classmethod
    def parse_param_decls(cls, line: str):
        m = re.match(cls.PARAM_DECL_RE, line, re.M)
        if m is None:
            return None
        txt = line.lstrip().lstrip("parameters").lstrip()
        vals = cls.parse_param_values(txt)
        return ParamDecls(vals)

    # Instance expressions
    INST_LEFT_RE = rf"({IDENT_RE})\s+({PORT_CONN_RE})\s+({IDENT_RE})"
    INSTANCE_RE = rf"({INST_LEFT_RE})\s+({PARAM_KWARGS_RE})\s*$"

    @classmethod
    def parse_instance(cls, txt: str) -> Optional[Instance]:
        """ Parse a Subckt/Module Instance """
        m = re.match(cls.INSTANCE_RE, txt.lstrip(), re.M)
        if m is None:
            if txt.strip().startswith("c"):
                print(5)
            return None

        # Split into left (instance, port names) and right (parameters) halves
        left = m.group(1)
        rest = txt.replace(left, "")

        # Parse the right-half parameters
        params = cls.parse_param_values(rest)

        # Now parse the left-half into instance name, ports, and module name
        ml = re.match(cls.INST_LEFT_RE, left)
        if ml is None:
            raise NetlistParseError
        name = Ident(ml.group(1).strip())
        module = Ident(ml.group(3).strip())
        conns = [Ident(s) for s in ml.group(2).split()]
        return Instance(name, module, conns, params)

    @classmethod
    def parse_primitive(cls, txt: str):
        """ Parse a Primitive Instance """
        m = re.match(re.compile(cls.PRIMITIVE_RE), txt)
        if m is None:
            raise NetlistParseError
        exprs = m.group(1)
        rest = txt.replace(exprs, "")
        kwargs = cls.parse_param_values(rest)
        exprs = exprs.split()
        name = Ident(exprs.pop(0))
        args = [ParamValue(s) for s in exprs]
        return Primitive(name, args, kwargs)

    @classmethod
    def parse_param_values(cls, line: str) -> Dict[Ident, ParamValue]:
        return {
            Ident(i.group(1).strip()): ParamValue(i.group(2).strip())
            for i in re.compile(cls.PARAM_SET_RE).finditer(line)
        }

    @classmethod
    def parse_exprs_and_params(
        cls, txt: str
    ) -> (List[ParamValue], Dict[Ident, ParamValue]):
        m = re.match(re.compile(cls.PARAMS_ARG_AND_KWARG_RE), txt)
        if m is None:
            raise NetlistParseError
        exprs = m.group(1)
        rest = txt.replace(exprs, "")
        exprs = [ParamValue(s) for s in exprs.split()]
        params = cls.parse_param_values(rest)
        return (exprs, params)

    @classmethod
    def parse_idents_and_params(
        cls, txt: str
    ) -> (List[Ident], Dict[Ident, ParamValue]):
        """ Parsers (fairly common) strings of the form `xabc a b c mymodel d=1 e=2 f=3.9e19 """
        m = re.match(re.compile(cls.IDENTS_AND_PARAMS_RE), txt)
        if m is None:
            raise NetlistParseError
        names = m.group(1)
        rest = txt.replace(names, "")
        names = [Ident(s) for s in names.split()]
        params = cls.parse_param_values(rest)
        return (names, params)

    @classmethod
    def parse_model_def(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_options(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_lib(cls, line: str):
        raise NotImplementedError

    @classmethod
    def parse_inc(cls, line: str):
        raise NotImplementedError


class Parser:
    def __init__(self, dialect: Optional[SpiceDialects] = None):
        self.dialect = Dialect.from_enum(dialect)(self)
        self.deps: List[Path] = []
        self.entries: List[Entry] = []
        self.line = self.nxt = "*"  # Initialize our two lines worth of input strings
        self.line_num = 1

    @classmethod
    def default_dialect(cls, path: os.PathLike) -> "SpiceDialects":
        """ Infer a default dialect from a file name, particularly its suffix. 
        For files of suffix `scs` this is straightforwardly set to SPECTRE. 
        All other suffixes are less clear without knowing more context. 
        They are set to the most flexible SPECTRE_SPICE, which includes dialect-changes. """

        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)
        if p.suffix == "scs":
            return SpiceDialects.SPECTRE
        return SpiceDialects.SPECTRE_SPICE

    def notify(self, reason: DialectChange):
        """ Notification from dialect-parser that something's up. 
        E.g. for dialect changes via `simulator lang` statements. """

        s = reason.dialect.lower().strip()
        if s == "spectre":
            dialect = SpiceDialects.SPECTRE
        elif s == "spice":
            dialect = SpiceDialects.SPECTRE_SPICE
        else:
            raise NetlistParseError
        self.dialect = Dialect.from_enum(dialect)(self)

    def peek(self) -> str:
        return self.nxt

    def advance(self) -> str:
        self.line = self.nxt
        self.nxt = self.fp.readline()
        self.line_num += 1
        return self.line

    def _parse_file(self):
        """ Parse the (open) file-pointer at `self.fp` """
        entries = []
        self.line_num = start_line_num = 1
        self.advance()

        while self.line:
            # Iterate over multi-line statements
            line = self.line[:]  # Copies

            while self.dialect.is_continuation(self.nxt):
                if not self.dialect.is_comment(self.nxt):
                    line += self.nxt.lstrip()[1:]
                if not self.nxt:
                    break  # Break on end-of-file
                self.advance()

            if line.strip():  # Filter out empty lines
                try:  # Collected a statement, parse it
                    s = self.dialect.parse_stmt(line.lstrip())
                    e = Entry(
                        content=s,
                        source_info=SourceInfo(
                            path=self.path,
                            line=start_line_num,
                            dialect=self.dialect.enum,
                        ),
                    )
                    entries.append(e)
                except NetlistParseError as e:
                    raise NetlistParseError(
                        f"Netlist Parse Error in {self.path} Line {start_line_num}: {str(e)}"
                    )

            # Move to the next statement
            start_line_num = self.line_num
            self.advance()

        # Append everything to our running total
        self.entries.extend(entries)
        return entries

    def parse(self, path: os.PathLike):
        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)
        # Source Found; Start Parsing
        self.deps.append(p)
        with open(p, "r") as f:
            self.path = p
            self.fp = f
            entries = self._parse_file()

        # 2nd pass, parse include-files
        for e in entries:
            s = e.content
            if isinstance(s, Include):
                # Differentiate absolute vs relative paths, relative to active source-file
                incp = s.path if s.path.is_absolute() else p.parent / s.path
                if not incp.exists() or not incp.is_file():
                    raise FileNotFoundError(incp)
                if isinstance(self.dialect, SpectreMixin):
                    # Update our dialect based on the file-extension we load
                    d_enum = Parser.default_dialect(path)
                    self.dialect = Dialect.from_enum(d_enum)(self)
                self.parse(incp)
            if isinstance(s, UseLib):
                # Not supported, yet
                raise NetlistParseError


def parse(path: os.PathLike, *, dialect=None) -> Parser:
    if dialect is None:
        dialect = Parser.default_dialect(path)
    p = Parser(dialect)
    p.parse(path)
    return p

