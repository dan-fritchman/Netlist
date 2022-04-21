""" 

# Netlist Abstract Syntax Tree

All elements in the netlist-internal "IR", primarily in the form of dataclasses. 
AST-level content captures a netlist as understood just after parsing, 
as a set of definitions and references to other definitions. 
It *does not* include resolution of what these definitions ultimately mean or refer to, 
or whether dependencies between them are valid. 

"""

# Std-Lib Imports
from enum import Enum, auto
from pathlib import Path
from dataclasses import field 
from typing import Optional, Union, List, Tuple

# PyPi Imports
from pydantic.dataclasses import dataclass

# Local Imports
from .shared import (
    SourceInfo,
    Ident,
    Int,
    Float,
    MetricNum,
    UnaryOperator,
    BinaryOperator,
)

# Keep a list of datatypes defined here,
# primarily so that we can update their forward-references at the end of this module.
datatypes = []
unions = []


def datatype(cls: type) -> type:
    """ Register a class as a datatype. """

    # Add an `Optional[SourceInfo]` field to the class, with a default value of `None`.
    # Creates the `__annotations__` field if it does not already exist.
    anno = getattr(cls, "__annotations__", {})
    anno["source_info"] = Optional[SourceInfo]
    cls.__annotations__ = anno
    cls.source_info = field(default=None, repr=False)

    # Convert it to a `pydantic.dataclasses.dataclass`
    cls = dataclass(cls)

    # And add it to the list of datatypes
    datatypes.append(cls)
    return cls


@datatype
class Ident:
    """ Identifier """

    name: str


@datatype
class HierPath:
    """ Hierarchical Path Identifier """

    path: List[Ident]


@datatype
class ParamDecl:
    """ Parameter Declaration 
    Includes Optional Distribution Information """

    name: Ident
    default: Optional["Expr"]
    distr: Optional[str] = None


@datatype
class ParamDecls:
    """ Parameter Declarations, 
    as via the `param` keywords. """

    params: List[ParamDecl]


@datatype
class ParamVal:
    """ Parameter Value-Set """

    name: Ident
    val: "Expr"


@datatype
class Instance:
    """ Subckt / Module Instance """

    name: Ident  # Instance Name
    module: Ident  # Module / Subcircuit Name

    # Connections, either by-position or by-name
    conns: Union[List[Ident], List[Tuple[Ident, Ident]]]
    params: List[ParamVal]  # Parameter Values


@datatype
class Primitive:
    """ 
    Primitive Instance 
    
    Note at parsing-time, before models are sorted out, 
    it is not always clear what is a port, model name, and parameter value. 
    Primitives instead store positional and keyword arguments `args` and `kwargs`. 
    """

    name: Ident
    args: List["Expr"]
    kwargs: List[ParamVal]


@datatype
class QuotedString:
    """ Quoted String Value """

    txt: str


# Simulation Options can take on the values of expressions, e.g. parameter combinations,
# and those of quoted strings, often for file-path.
OptionVal = Union[QuotedString, "Expr"]
unions.append(OptionVal)


@datatype
class Option:
    """ Simulation Option """

    name: Ident  # Option Name
    val: OptionVal  # Option Value


@datatype
class Options:
    """ Simulation Options """

    name: Optional[Ident]  # Name of the Option-set. Used by some dialects.
    vals: List[Option]  # List of [`Option`]s


@datatype
class StartSubckt:
    """ Start of a Subckt / Module Definition """

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations


@datatype
class EndSubckt:
    """ End of a Subckt / Module Definition """

    name: Optional[Ident]


@datatype
class SubcktDef:
    """ Sub-Circuit / Module Definition """

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations
    entries: List["Entry"]


@datatype
class ModelDef:
    """ Model Definition """

    name: Ident  # Model Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
class ModelVariant:
    """ Model Variant within a `ModelFamily` """

    model: Ident  # Model Family Name
    variant: Ident  # Variant Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
class ModelFamily:
    """ Model Family 
    A set of related, identically named models, generally separated by limiting parameters such as {lmin, lmax} or {wmin, wmax}. """

    name: Ident  # Model Family Name
    mtype: Ident  # Model Type
    variants: List[ModelVariant]  # Variants


@datatype
class Include:
    """ Include (a File) Statement """

    path: Path


@datatype
class AhdlInclude:
    """ Analog HDL Include (a File) Statement """

    path: Path


@datatype
class StartLib:
    """ Start of a `Library`"""

    name: Ident


@datatype
class EndLib:
    """ End of a `Library`"""

    name: Optional[Ident]


@datatype
class StartLibSection:
    """ Start of a `LibrarySection` """

    name: Ident


@datatype
class EndLibSection:
    """ End of a `LibrarySection` """

    name: Ident


@datatype
class LibSection:
    """ Library Section 
    A named section of a library, commonly incorporated with a `UseLib` or similar. """

    name: Ident  # Section Name
    entries: List["Entry"]  # Entry List


@datatype
class StartProtectedSection:
    """ Start of a `ProtectedSection` """

    ...  # Empty


@datatype
class EndProtectedSection:
    """ End of a `ProtectedSection` """

    ...  # Empty


@datatype
class ProtectedSection:
    """ Protected Section """

    entries: List["Entry"]  # Entry List


@datatype
class Library:
    """ Library, as Generated by the Spice `.lib` Definition Card
    Includes a list of named `LibSection`s which can be included by their string-name, 
    as common for "corner" inclusions e.g. `.inc "mylib.sp" "tt"`  """

    name: Ident  # Library Name
    sections: List[LibSection]  # Library Sections


@datatype
class UseLib:
    """ Use a Library """

    path: Path  # Library File Path
    section: Ident  # Section Name


@datatype
class End:
    """ Empty class represents `.end` Statements """

    ...  # Empty


@datatype
class Variation:
    """ Single-Parameter Variation Declaration """

    name: Ident  # Parameter Name
    dist: str  # Distribution Name/Type
    std: "Expr"  # Standard Deviation


@datatype
class StatisticsBlock:
    """ Statistical Descriptions """

    process: Optional[List[Variation]]
    mismatch: Optional[List[Variation]]


@datatype
class Unknown:
    """ Unknown Netlist Statement. Stored as an un-parsed string. """

    txt: str


@datatype
class DialectChange:
    """ Netlist Dialect Changes, e.g. `simulator lang=xyz` """

    dialect: str


@datatype
class Int:
    """ Integer Number """

    val: int


@datatype
class Float:
    """ Floating Point Number """

    val: float


@datatype
class MetricNum:
    """ Number with Metric Suffix """

    val: str  # No conversion, just stored as string for now


@datatype
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


class ArgType(Enum):
    """ Enumerated Supported Types for Function Arguments and Return Values """

    REAL = auto()
    UNKNOWN = auto()


@datatype
class TypedArg:
    """ Typed Function Argument """

    tp: ArgType  # Argument Type
    name: Ident  # Argument Name


@datatype
class Return:
    """ Function Return Node """

    val: "Expr"


# Types which can be used inside a function definition.
# Will of course grow, in time.
FuncStatement = Union[Return]
unions.append(FuncStatement)


@datatype
class FunctionDef:
    """ Function Definition """

    name: Ident  # Function Name
    rtype: ArgType  # Return Type
    args: List[TypedArg]  # Argument List
    stmts: List[FuncStatement]  # Function Body/ Statements


# Expression Union
# Everything which can be used as a mathematical expression,
# and ultimately resolves to a scalar value at runtime.
Expr = Union["UnaryOp", "BinaryOp", "TernOp", Int, Float, MetricNum, Ident, Call]
unions.append(Expr)


class UnaryOperator(Enum):
    """ Enumerated, Supported Unary Operators 
    Values generally equal their string-format equivalents. """

    PLUS = "+"
    NEG = "-"


@datatype
class UnaryOp:
    """ Unary Operation """

    tp: UnaryOperator  # Operator Type
    targ: Expr  # Target Expression


@datatype
class BinaryOp:
    """ Binary Operation """

    tp: BinaryOperator  # Enumerated Operator Type
    left: Expr  # Left Operand Expression
    right: Expr  # Right Operand Expression


@datatype
class TernOp:
    """ Ternary Operation """

    cond: Expr  # Condition Expression
    if_true: Expr  # Value if `cond` is True
    if_false: Expr  # Value if `cond` is False


# Union of "flat" statements lacking (substantial) hierarchy
FlatStatement = Union[
    Instance,
    Primitive,
    ParamDecls,
    ModelDef,
    ModelVariant,
    ModelFamily,
    DialectChange,
    FunctionDef,
    Unknown,
    Options,
    Include,
    AhdlInclude,
    UseLib,
    StatisticsBlock,
]
unions.append(FlatStatement)

# Statements which indicate the beginning and end of hierarchical elements,
# and ultimately disappear into the structured AST
DelimStatement = Union[
    StartLib,
    EndLib,
    StartLibSection,
    EndLibSection,
    StartProtectedSection,
    EndProtectedSection,
    End,
]
unions.append(DelimStatement)

# Statements
# The union of types which can appear in first-pass parsing netlist
Statement = Union[FlatStatement, DelimStatement]
unions.append(Statement)

# Entries - the union of types which serve as "high-level" AST nodes,
# i.e. those which can be the direct children of a `SourceFile`.
Entry = Union[FlatStatement, SubcktDef, Library, LibSection, ProtectedSection, End]
unions.append(Entry)


@datatype
class SourceFile:
    path: Path  # Source File Path
    contents: List[Entry]  # Statements and their associated SourceInfo


@datatype
class Program:
    """ 
    # Multi-File "Netlist Program" 
    The name of this type is a bit misleading, but borrowed from more typical compiler-parsers. 
    Spice-culture generally lacks a term for "the totality of a simulator invocation input", 
    or even "a pile of source-files to be used together". 
    So, `Program` it is. 
    """

    files: List[SourceFile]  # List of Source-File Contents


# Update all the forward type-references
for tp in datatypes:
    tp.__pydantic_model__.update_forward_refs()

# And solely export the defined datatypes
# (at least with star-imports, which are hard to avoid using with all these types)
__all__ = [tp.__name__ for tp in datatypes] + [
    "Int",
    "Float",
    "MetricNum",
    "Ident",
    "BinaryOperator",
    "UnaryOperator",
    "Expr",
    "Entry",
    "Statement",
]
