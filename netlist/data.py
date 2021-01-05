import os
import re
from enum import Enum
from pathlib import Path
from typing import Optional, Any, Dict, Union, List, Tuple

from pydantic import ValidationError
from pydantic.dataclasses import dataclass


class NetlistParseError(Exception):
    @staticmethod
    def throw(*args, **kwargs):
        """ Exception-raising debug wrapper. Breakpoint to catch `NetlistParseError`s. """
        raise NetlistParseError(*args, **kwargs)


class NetlistDialects(Enum):
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


@dataclass
class SourceInfo:
    path: Path
    line: int
    dialect: "NetlistDialects"


@dataclass(eq=True, frozen=True)
class Ident:
    name: str


@dataclass
class HierPath:
    """ Hierarchical Path Identifier """

    path: List[Ident]


@dataclass(eq=True, frozen=True)
class ParamDecl:
    """ Parameter Declaration 
    Includes Optional Distribution & Unit Information """

    name: Ident
    default: "Expr"
    distr: Optional[str] = None


@dataclass(eq=True, frozen=True)
class ParamDecls:
    """ Parameter Declarations, 
    as via the `param` keywords. """

    params: List[ParamDecl]


@dataclass(eq=True, frozen=True)
class ParamVal:
    """ Parameter Value-Set """

    name: Ident
    val: "Expr"


@dataclass
class Instance:
    """ Subckt/Module Instance """

    name: Ident
    module: Ident
    conns: Union[List[Ident], List[Tuple[Ident, Ident]]]
    params: List[ParamVal]


@dataclass
class Primitive:
    """ Simulator-Defined Primitive Instance 
    Note at parsing-time, before models are sorted out, 
    it is not always clear what is a port, model name, and parameter value. 
    Primitives instead store positional and keyword arguments `args` and `kwargs`. """

    name: Ident
    args: List["Expr"]
    kwargs: List[ParamVal]


@dataclass
class Options:
    vals: List[ParamVal]


@dataclass
class StartSubckt:
    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations


@dataclass
class EndSubckt:
    name: Optional[Ident]


@dataclass
class ModelDef:
    name: Ident # Model Name 
    mtype: Ident # Model Type
    args: List[Ident] # Positional Arguments 
    params: List[ParamDecl] # Parameter Declarations & Defaults


@dataclass
class ModelVariant:
    model: Ident # Model Family Name 
    variant: Ident # Variant Name
    args: List[Ident] # Positional Arguments 
    params: List[ParamDecl] # Parameter Declarations & Defaults


@dataclass
class ModelFamily:
    name: Ident # Model Family Name 
    mtype: Ident # Model Type
    variants: List[ModelVariant] # Variants 


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


@dataclass
class End:
    """ Empty class represents `.end` Statements """

    ...


@dataclass
class StatisticsBlock:
    """ Statistical Descriptions. Largely un-parsed for now. """

    txt: str


@dataclass
class Unknown:
    """ Unknown Netlist Statement. Stored as an un-parsed string. """

    txt: str


@dataclass
class Comment:
    txt: str


@dataclass
class DialectChange:
    """ Netlist Dialect Changes, e.g. `simulator lang=xyz` """

    dialect: str


# The big union-type of everything that makes a valid single-line statement
Statement = Union[
    Comment,
    ParamDecls,
    Instance,
    Primitive,
    Options,
    StartSubckt,
    EndSubckt,
    ModelDef,
    ModelVariant,
    ModelFamily,
    Include,
    StartLib,
    EndLib,
    UseLib,
    End,
    DialectChange,
    StatisticsBlock,
    Unknown,
]


@dataclass
class Entry:
    content: Statement
    source_info: Optional[SourceInfo] = None


@dataclass
class SourceFile:
    path: Path  # Source File Path
    contents: List[Entry]  # Statements and their associated SourceInfo


@dataclass
class Program:
    files: List[SourceFile]  # List of Source-File Contents


@dataclass
class Int:
    val: int


@dataclass
class Float:
    val: float


@dataclass
class MetricNum:
    val: str  # No conversion, just stored as string for now


@dataclass
class Call:
    """ Function Call Node 
    All valid parameter-generating function calls return a single value, 
    usable in a mathematical expression (`Expr`) context. 
    All arguments are provided by position and stored in a List. 
    All arguments must also be resolvable as mathematical expressions. 
    Function-names are left as identifiers (`Ident`). 
    No checking is performed as to their availability, 
    nor are they checked for membership of any keyword-list. 
    
    Examples:
    `sqrt(2)` => Call(func=Ident("sqrt"), args=([Int(2)]),)
    """

    func: Ident  # Function Name
    args: List["Expr"]  # Arguments List


Expr = Union["UnOp", "BinOp", Int, Float, MetricNum, Ident, Call]


@dataclass
class UnOp:
    tp: str
    targ: Expr


@dataclass
class BinOp:
    tp: str
    left: Expr
    right: Expr


# Update all the forward-type-references
Call.__pydantic_model__.update_forward_refs()
Primitive.__pydantic_model__.update_forward_refs()
ParamVal.__pydantic_model__.update_forward_refs()
ParamDecl.__pydantic_model__.update_forward_refs()
ParamDecls.__pydantic_model__.update_forward_refs()
SourceInfo.__pydantic_model__.update_forward_refs()
Entry.__pydantic_model__.update_forward_refs()
SourceFile.__pydantic_model__.update_forward_refs()
Program.__pydantic_model__.update_forward_refs()
