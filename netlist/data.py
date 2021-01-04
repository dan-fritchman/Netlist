import os
from pathlib import Path
import re

from pydantic import ValidationError
from pydantic.dataclasses import dataclass
from typing import Optional, Any, Dict, Union, List, Tuple

from enum import Enum


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
    name: Ident
    default: "Expr"
    unit: Optional[str] = None
    distr: Optional[str] = None


@dataclass(eq=True, frozen=True)
class ParamDecls:
    params: List[ParamDecl]


@dataclass
class Instance:
    """ Subckt/Module Instance """

    name: Ident
    module: Ident
    conns: Union[List[Ident], List[Tuple[Ident, Ident]]]
    params: ParamDecls  # FIXME: will be just List[(Ident, "Expr")], without the parameter declaration annotations


@dataclass
class Primitive:
    """ Simulator-Defined Primitive Instance 
    Note at parsing-time, before models are sorted out, 
    it is not always clear what is a port, model name, and parameter value. 
    Primitives instead store positional and keyword arguments `args` and `kwargs`. """

    name: Ident
    args: List["Expr"]
    kwargs: ParamDecls


@dataclass
class Options:
    vals: ParamDecls


@dataclass
class StartSubckt:
    name: Ident
    ports: List[Ident]
    params: ParamDecls


@dataclass
class EndSubckt:
    name: Optional[Ident]


@dataclass
class ModelDef:
    name: HierPath  # FIXME: may need to be specialized
    args: List[Ident]
    params: ParamDecls


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
    """ Unknown Netlist Statement. Stored as an un-parsed string. """

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
ParamDecl.__pydantic_model__.update_forward_refs()
SourceInfo.__pydantic_model__.update_forward_refs()
