""" 

# Netlist Data Model 

All elements in the netlist-internal "IR", 
primarily in the form of dataclasses. 

"""

# Std-Lib Imports
from enum import Enum
from pathlib import Path
from typing import Optional, Any, Dict, Union, List, Tuple

# PyPi Imports
from pydantic.dataclasses import dataclass

# from pydantic import ValidationError


class NetlistParseError(Exception):
    """ Netlist Parse Error """

    @staticmethod
    def throw(*args, **kwargs):
        """ Exception-raising debug wrapper. Breakpoint to catch `NetlistParseError`s. """
        raise NetlistParseError(*args, **kwargs)


class NetlistDialects(Enum):
    """ Enumerated, Supported Netlist Dialects """

    SPECTRE = 1
    SPECTRE_SPICE = 2
    SPICE = 3
    HSPICE = 4
    NGSPICE = 5


def to_json(arg) -> str:
    """ Dump any `pydantic.dataclass` or simple combination thereof to JSON string. 
    (Unfortunately doesn't seem to like our `Dict` types yet.) """
    import json
    from pydantic.json import pydantic_encoder

    return json.dumps(arg, indent=2, default=pydantic_encoder)


# Keep a list of datatype defined here,
# primarily so that we can update their forward-references at the end of this module.
datatypes = []


def datatype(cls: type) -> type:
    """ Register a class as a datatype. """
    datatypes.append(cls)
    return cls


@datatype
@dataclass
class SourceInfo:
    """ Parser Source Information """

    line: int
    dialect: "NetlistDialects"


@datatype
@dataclass(eq=True, frozen=True)
class Ident:
    """ Identifier """

    name: str


@datatype
@dataclass
class HierPath:
    """ Hierarchical Path Identifier """

    path: List[Ident]


@datatype
@dataclass
class ParamDecl:
    """ Parameter Declaration 
    Includes Optional Distribution Information """

    name: Ident
    default: Optional["Expr"]
    distr: Optional[str] = None


@datatype
@dataclass
class ParamDecls:
    """ Parameter Declarations, 
    as via the `param` keywords. """

    params: List[ParamDecl]


@datatype
@dataclass
class ParamVal:
    """ Parameter Value-Set """

    name: Ident
    val: "Expr"


@datatype
@dataclass
class Instance:
    """ Subckt/Module Instance """

    name: Ident
    module: Ident
    conns: Union[List[Ident], List[Tuple[Ident, Ident]]]
    params: List[ParamVal]


@datatype
@dataclass
class Primitive:
    """ Simulator-Defined Primitive Instance 
    Note at parsing-time, before models are sorted out, 
    it is not always clear what is a port, model name, and parameter value. 
    Primitives instead store positional and keyword arguments `args` and `kwargs`. """

    name: Ident
    args: List["Expr"]
    kwargs: List[ParamVal]


@datatype
@dataclass
class Options:
    name: Optional[Ident]
    vals: List[ParamVal]


@datatype
@dataclass
class StartSubckt:
    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations


@datatype
@dataclass
class EndSubckt:
    name: Optional[Ident]


@datatype
@dataclass
class SubcktDef:
    """ Sub-Circuit / Module Definition """

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations
    entries: List["SubcktEntry"]


@datatype
@dataclass
class ModelDef:
    name: Ident  # Model Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
@dataclass
class ModelVariant:
    model: Ident  # Model Family Name
    variant: Ident  # Variant Name
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
@dataclass
class ModelFamily:
    name: Ident  # Model Family Name
    mtype: Ident  # Model Type
    variants: List[ModelVariant]  # Variants


@datatype
@dataclass
class Include:
    path: Path


@datatype
@dataclass
class AhdlInclude:
    path: Path


@datatype
@dataclass
class StartLib:
    name: Ident


@datatype
@dataclass
class EndLib:
    name: Optional[Ident]


@datatype
@dataclass
class StartLibSection:
    name: Ident


@datatype
@dataclass
class EndLibSection:
    name: Ident


@datatype
@dataclass
class LibSection:
    name: Ident
    entries: List["Entry"]


@datatype
@dataclass
class Library:
    name: Ident
    sections: List[LibSection]


@datatype
@dataclass
class UseLib:
    path: Path
    section: Ident


@datatype
@dataclass
class End:
    """ Empty class represents `.end` Statements """

    ...


@datatype
@dataclass
class Variation:
    """ Single-Parameter Variation Declaration """

    name: Ident
    dist: str
    std: "Expr"


@datatype
@dataclass
class StatisticsBlock:
    """ Statistical Descriptions """

    process: Optional[List[Variation]]
    mismatch: Optional[List[Variation]]


@datatype
@dataclass
class Unknown:
    """ Unknown Netlist Statement. Stored as an un-parsed string. """

    txt: str


@datatype
@dataclass
class DialectChange:
    """ Netlist Dialect Changes, e.g. `simulator lang=xyz` """

    dialect: str


# The big union-type of everything that makes a valid single-line statement

# A shorthand for a bunch of stuff, "mixed in" to several other type-unions.
MostStatements = Union[
    Instance,
    Primitive,
    ParamDecls,
    ModelDef,
    ModelVariant,
    ModelFamily,
    DialectChange,
    "FunctionDef",
    Unknown,
]

# (Flat) statements which can appear in sub-circuit definitions
SubcktStatement = Union[StartSubckt, EndSubckt, MostStatements]

# Nodes which can be the (direct) children of sub-circuit definitions
SubcktNode = Union[SubcktDef, MostStatements]

MostFileStatements = Union[
    Options,
    Include,
    AhdlInclude,
    StartLib,
    EndLib,
    UseLib,
    StartLibSection,
    EndLibSection,
    StatisticsBlock,
    End,
]
Statement = Union[SubcktStatement, MostFileStatements]

# Nodes which can be the (direct) children of `SourceFiles`
FileNode = Union[Library, SubcktDef, SubcktStatement, MostFileStatements]


@datatype
@dataclass
class Entry:
    content: Statement
    source_info: Optional[SourceInfo] = None


@datatype
@dataclass
class FileEntryFlat:
    content: FileNode
    source_info: Optional[SourceInfo] = None


@datatype
@dataclass
class FileEntry:
    content: FileNode
    source_info: Optional[SourceInfo] = None


@datatype
@dataclass
class SubcktEntry:
    content: SubcktNode
    source_info: Optional[SourceInfo] = None


@datatype
@dataclass
class SourceFile:
    path: Path  # Source File Path
    contents: List[FileEntry]  # Statements and their associated SourceInfo


@datatype
@dataclass
class Program:
    files: List[SourceFile]  # List of Source-File Contents


@datatype
@dataclass
class Int:
    val: int


@datatype
@dataclass
class Float:
    val: float


@datatype
@dataclass
class MetricNum:
    val: str  # No conversion, just stored as string for now


@datatype
@dataclass
class Call:
    """ 
    Function Call Node 

    All valid parameter-generating function calls return a single value, 
    usable in a mathematical expression (`Expr`) context. 
    All arguments are provided by position and stored in a List. 
    All arguments must also be resolvable as mathematical expressions. 
    
    Examples:
    `sqrt(2)` => Call(func=Ident("sqrt"), args=([Int(2)]),)
    """

    func: Ident  # Function Name
    args: List["Expr"]  # Arguments List


@datatype
@dataclass
class TypedArg:
    tp: Ident
    name: Ident


@datatype
@dataclass
class Return:
    val: "Expr"


# Types which can be used inside a function definition.
# Will of course grow, in time.
FuncStatement = Union[Return]


@datatype
@dataclass
class FunctionDef:
    name: Ident
    rtype: Ident
    args: List[TypedArg]
    stmts: List[FuncStatement]


Expr = Union["UnOp", "BinOp", "TernOp", Int, Float, MetricNum, Ident, Call]


@datatype
@dataclass
class UnOp:
    """ Unary Operation """

    tp: str
    targ: Expr


@datatype
@dataclass
class BinOp:
    """ Binary Operation """

    tp: str
    left: Expr
    right: Expr


@datatype
@dataclass
class TernOp:
    """ Ternary Operation """

    cond: Expr
    if_true: Expr
    if_false: Expr


# Update all the forward type-references
for tp in datatypes:
    tp.__pydantic_model__.update_forward_refs()

