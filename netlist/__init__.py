__version__ = '0.1.0'


import os 
from pathlib import Path 
import re


from pydantic.dataclasses import dataclass 
from typing import Optional, Any, Dict, Union, List  

@dataclass
class SourceInfo:
    file: Path
    line: int

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
    name: HierPath # FIXME: may need to be specialized 
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
class Unknown:
    """ Error/ Unknown Netlist Statement """
    content: str 

@dataclass 
class Comment:
    txt: str 

@dataclass
class DialectChange:
    """ Netlist Dialect Changes, e.g. `simulator lang=xyz` """
    dialect: str 

@dataclass
class Statement:
    content: Any 
    source_info: Optional[SourceInfo] = None

class NetlistParseError(Exception):
    ...

class HspiceDialect:
    HIER_PATH_SEP = '.'
    CONTINUATION_CHAR = '+'
    COMMENT_CHAR = '*'

    IDENT_START_RE = '[A-Za-z_]' # Valid characters to start an identifier 
    IDENT_CONT_RE = '[A-Za-z0-9_]' # Valid (non-start) characters in identifiers
    IDENT_RE = f'{IDENT_START_RE}{IDENT_CONT_RE}*'
    IDENTS_LIST_RE = f'\s*({IDENT_RE}\s+)+({IDENT_RE}(\s+|$))?'
    HIER_IDENT_RE = f'({IDENT_RE})(\.{IDENT_RE})*'
    MODEL_NAME_RE = f'({IDENT_RE})(\.([A-Za-z_0-9]*))?' # E.g. `nmos`, `nmos.0`
    NODE_NAME_RE = f'([A-Za-z0-9_:]+)' # Node names are much more foregiving. `i:`, `___`, `123`, etc are all valid

    EXPR_RE = "([^\s=]+|'[^=]+'|{[^=]+})" # Parameter expression, inside or outside bracketing
    EXPR_LIST_RE = f'\s*({EXPR_RE}\s+)+({EXPR_RE}(\s+|$))?'
    UNITS_RE = '$ Units: ([.*])\n'
    PARAM_SET_RE = f'\s*({IDENT_RE})\s*=\s*({EXPR_RE})\s*' ## FIXME: units ## (\s*|({UNITS_RE})?)' # Form a=5 b=6 c=7
    PARAM_KWARGS_RE = f'({PARAM_SET_RE})+'
    IDENTS_AND_PARAMS_RE = f'({IDENTS_LIST_RE})({PARAM_KWARGS_RE})?' # Form: name arg1 arg2 arg3 a=3 b=4 c=5
    PARAMS_ARG_AND_KWARG_RE = f'({EXPR_LIST_RE})({PARAM_KWARGS_RE})?' # Form: 'expr' 'expr1*expr2' '1e-9' a=3 b='16-11' c=0

    MODEL_DEF_RE = f'(.model|.MODEL)\s+({MODEL_NAME_RE})\s+({IDENTS_AND_PARAMS_RE})'
    SUBCKT_START_RE = f'(.subckt|.SUBCKT)\s+({IDENTS_AND_PARAMS_RE})'
    SUBCKT_END_RE = f'(.ends|.ENDS)(\s+{IDENT_RE}\s*)?' 

    # Primitives are extra-fun, as many have optional ports, models, and parameters by-position! For now we just grab the all as Expressions
    PRIMITIVE_RE = f'([RrCcIiVvDdMmQq]{IDENT_CONT_RE}+\s+{EXPR_LIST_RE})({PARAM_KWARGS_RE})?$'
    # Module instances, in contrast, have the relative sanity of requiring all ports, and passing parameters by name
    INST_LEFT_RE = f'[Xx]({IDENT_CONT_RE}+)\s+({NODE_NAME_RE}\s+)+({IDENT_RE})' # Form `xname p1 p2 p3 modulename`
    INSTANCE_RE = f'({INST_LEFT_RE})\s+({PARAM_KWARGS_RE})' # Form `xname p1 p2 p3 modulename a=1 b=2 c=3`

    @classmethod
    def is_continuation(cls, s: str) -> bool:
        return s.startswith(cls.COMMENT_CHAR) or s.startswith(cls.CONTINUATION_CHAR)
    
    @classmethod
    def is_comment(cls, s: str) -> bool:
        return s.startswith(cls.COMMENT_CHAR)

    @classmethod
    def parse_stmt(cls, line: str):

        # Comments get highest priority 
        if line.strip().startswith(cls.COMMENT_CHAR):
            return cls.parse_comment(line)
        
        # Dictionary of rules, in priority-order 
        rules = {
            cls.PRIMITIVE_RE: cls.parse_primitive,
            cls.INSTANCE_RE: cls.parse_instance,
            cls.SUBCKT_START_RE: cls.parse_subckt_start,
            cls.SUBCKT_END_RE: cls.parse_subckt_end,
        }
        for pattern, func in rules.items():
            if re.match(re.compile(pattern), line):
                return func(line) 

        # More-manual, older-school rules 
        lc = line.lower()
        if lc.startswith('.inc'):
            return cls.parse_inc(line)
        if lc.startswith('.option'):
            return cls.parse_options(line)
        if lc.startswith('.param'):
            return cls.parse_param_decls(line)
        if lc.startswith('.lib'):
            return cls.parse_dot_lib(line)
        if lc.startswith('.model'):
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
        return {Ident(i.group(1)): ParamValue(i.group(2)) for i in re.compile(cls.PARAM_SET_RE).finditer(line)}

    @classmethod
    def parse_exprs_and_params(cls, txt: str) -> (List[ParamValue], Dict[Ident, ParamValue]):
        m = re.match(re.compile(cls.PARAMS_ARG_AND_KWARG_RE), txt)
        if m is None:
            raise NetlistParseError
        exprs = m.group(1)
        rest = txt.replace(exprs, "")
        exprs = [ParamValue(s) for s in exprs.split()]
        params = cls.parse_param_values(rest)
        return (exprs, params)

    @classmethod
    def parse_idents_and_params(cls, txt: str) -> (List[Ident], Dict[Ident, ParamValue]):
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
        name = txt.replace('.ends', '').replace('.ENDS', '').strip()
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
        name = cls.parse_hier_path(spl[0]) # FIXME: this may require specialty processing for Identifiers such as `0` in `nmos.0`
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
        txt = line.lower().replace(".lib", "").strip()
        parts = line.split()
        if parts[0].lower() != '.lib':
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
        return Comment(line)


def parse(path: os.PathLike):
    p = Path(path).absolute()
    if not p.exists() or not p.is_file():
        raise FileNotFoundError(p)
    
    # Source Found; Start Parsing
    dialect = HspiceDialect()
    stmts = []

    with open(p, 'r') as f:
        line_num = start_line_num = 1
        line = f.readline()
        while line:
            nxt = f.readline()
            line_num += 1
            # Iterate over multi-line content 
            while dialect.is_continuation(nxt):
                if not dialect.is_comment(nxt):
                    line += nxt[1:]
                nxt = f.readline()
                line_num += 1
            # Filter out empty lines
            if line.strip(): 
                # Collected a statement, parse it 
                try: 
                    s = dialect.parse_stmt(line)
                except NetlistParseError as e:
                    raise NetlistParseError(f'Netlist Parse Error in {p} Line {start_line_num}: {str(e)}')
                # print(s)
                stmts.append(s)
            line = nxt 
            start_line_num = line_num 
    
    for s in stmts: print(s)
    # 2nd pass, parse include-files 
    for s in stmts:
        if isinstance(s, Include):
            # Differentiate absolute vs relative paths, relative to active source-file 
            incp = s.path if s.path.is_absolute() else p.parent / s.path 
            if not incp.exists() or not incp.is_file():
                raise FileNotFoundError(incp)
            print(f'Parsing Library {incp}')
            parse(incp)
        if isinstance(s, UseLib):
            raise NetlistParseError
