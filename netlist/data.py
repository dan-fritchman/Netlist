""" 

# Netlist Data Model 

Primarily in the form of pydantic dataclasses. 

"""

import os
import re
from enum import Enum
from pathlib import Path
from typing import Optional, Any, Dict, Union, List, Tuple
from dataclasses import is_dataclass

from pydantic import ValidationError
from pydantic.dataclasses import dataclass


# _dataclasses = []


# def dataclass(_arg):
#     """ pydantic.dataclass wrapper to register each into our list, for later updates """
#     from pydantic.dataclasses import dataclass

#     def inner(_arg):
#         global _dataclasses
#         _dataclasses.append(_arg)
#         return dataclass(_arg)

#     return inner


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
    """ Parser Source Information """

    line: int
    dialect: "NetlistDialects"


@dataclass
class Ident:
    name: str


@dataclass
class HierPath:
    """ Hierarchical Path Identifier """

    path: List[Ident]


@dataclass
class ParamDecl:
    """ Parameter Declaration 
    Includes Optional Distribution Information """

    name: Ident
    default: Optional["Expr"]
    distr: Optional[str] = None


@dataclass
class ParamDecls:
    """ Parameter Declarations, 
    as via the `param` keywords. """

    params: List[ParamDecl]


@dataclass
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
    name: Optional[Ident]
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
class SubcktDef:
    """ Sub-Circuit / Module Definition """

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations
    entries: List["TreeEntry"]


@dataclass
class ModelDef:
    name: Ident  # Model Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@dataclass
class ModelVariant:
    model: Ident  # Model Family Name
    variant: Ident  # Variant Name
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@dataclass
class ModelFamily:
    name: Ident  # Model Family Name
    mtype: Ident  # Model Type
    variants: List[ModelVariant]  # Variants


@dataclass
class Include:
    path: Path


@dataclass
class AhdlInclude:
    path: Path


@dataclass
class StartLib:
    name: Ident


@dataclass
class EndLib:
    name: Optional[Ident]


@dataclass
class StartLibSection:
    name: Ident


@dataclass
class EndLibSection:
    name: Ident


@dataclass
class LibSection:
    name: Ident
    entries: List["TreeEntry"]


@dataclass
class Library:
    name: Ident
    sections: List[LibSection]


@dataclass
class UseLib:
    path: Path
    section: Ident


@dataclass
class End:
    """ Empty class represents `.end` Statements """

    ...


@dataclass
class Variation:
    """ Single-Parameter Variation Declaration """

    name: Ident
    dist: str
    std: "Expr"


@dataclass
class StatisticsBlock:
    """ Statistical Descriptions """

    process: Optional[List[Variation]]
    mismatch: Optional[List[Variation]]


@dataclass
class Unknown:
    """ Unknown Netlist Statement. Stored as an un-parsed string. """

    txt: str


@dataclass
class DialectChange:
    """ Netlist Dialect Changes, e.g. `simulator lang=xyz` """

    dialect: str


# The big union-type(s) of everything that makes a valid single-line statement
DelimStatement = Union[
    StartSubckt, EndSubckt, StartLib, EndLib, StartLibSection, EndLibSection,
]
FlatStatement = Union[
    Instance,
    Primitive,
    ParamDecls,
    ModelDef,
    ModelVariant,
    ModelFamily,
    DialectChange,
    Unknown,
    Options,
    Include,
    AhdlInclude,
    UseLib,
    StatisticsBlock,
    End,
    "FunctionDef",
]

# All valid single "line" statements
Statement = Union[FlatStatement, DelimStatement]

# All hierarchical nodes
Node = Union[Library, LibSection, SubcktDef, FlatStatement]


@dataclass
class FileEntry:
    content: Statement
    source_info: Optional[SourceInfo] = None


@dataclass
class TreeEntry:
    content: Node
    source_info: Optional[SourceInfo] = None


@dataclass
class SourceFile:
    path: Path  # Source File Path
    contents: List[TreeEntry]  # Statements and their associated SourceInfo


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


@dataclass
class TypedArg:
    tp: Ident
    name: Ident


@dataclass
class Return:
    val: "Expr"


# Types which can be used inside a function definition.
# Will of course grow, in time.
FuncStatement = Union[Return]


@dataclass
class FunctionDef:
    name: Ident
    rtype: Ident
    args: List[TypedArg]
    stmts: List[FuncStatement]


Expr = Union["UnOp", "BinOp", "TernOp", Int, Float, MetricNum, Ident, Call]


@dataclass
class UnOp:
    """ Unary Operation """

    tp: str
    targ: Expr


@dataclass
class BinOp:
    """ Binary Operation """

    tp: str
    left: Expr
    right: Expr


@dataclass
class TernOp:
    """ Ternary Operation """

    cond: Expr
    if_true: Expr
    if_false: Expr


# Update all the forward type-references 
Variation.__pydantic_model__.update_forward_refs()
StatisticsBlock.__pydantic_model__.update_forward_refs()
Call.__pydantic_model__.update_forward_refs()
Return.__pydantic_model__.update_forward_refs()
Primitive.__pydantic_model__.update_forward_refs()
ParamVal.__pydantic_model__.update_forward_refs()
ParamDecl.__pydantic_model__.update_forward_refs()
ParamDecls.__pydantic_model__.update_forward_refs()
SourceInfo.__pydantic_model__.update_forward_refs()
FileEntry.__pydantic_model__.update_forward_refs()
TreeEntry.__pydantic_model__.update_forward_refs()
SubcktDef.__pydantic_model__.update_forward_refs()
LibSection.__pydantic_model__.update_forward_refs()
SourceFile.__pydantic_model__.update_forward_refs()
Program.__pydantic_model__.update_forward_refs()
