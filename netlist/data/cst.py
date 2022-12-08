""" 

# Netlist Concrete Syntax Tree

The netlist data model after general interpretation of what AST content "means". 
This includes scope-based name resolution, parameter-value resolution, and the like. 

"""

# Std-Lib Imports
from dataclasses import field
from enum import Enum, auto
from pathlib import Path
from typing import Optional, Union, List, Tuple, Dict, Any

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
datatypes = [SourceInfo]


def datatype(cls: type) -> type:
    """Register a class as a datatype."""

    # Add an `Optional[SourceInfo]` field to the class, with a default value of `None`.
    # Creates the `__annotations__` field if it does not already exist.
    anno = getattr(cls, "__annotations__", {})
    anno["source_info"] = Optional[SourceInfo]
    cls.__annotations__ = anno
    cls.source_info = None

    # Convert it to a `pydantic.dataclasses.dataclass`
    cls = dataclass(cls)

    # And add it to the list of datatypes
    datatypes.append(cls)
    return cls


@datatype
class HierPath:
    """Hierarchical Path Identifier"""

    path: List[Ident]


@datatype
class ParamDecl:
    """Parameter Declaration
    Includes Optional Distribution Information"""

    name: Ident
    default: Optional["Expr"]
    distr: Optional[str] = None


@datatype
class ParamDecls:
    """Parameter Declarations,
    as via the `param` keywords."""

    params: List[ParamDecl]


@datatype
class ParamVal:
    """Parameter Value-Set"""

    name: Ident
    val: "Expr"


class RefType(Enum):
    """External Reference Types Enumeration
    Store on each `ExternalRef` to note which types would be valid in context."""

    SUBCKT = auto()
    PARAM = auto()
    FUNCTION = auto()
    MODEL = auto()


@datatype
class ExternalRef:
    """Typed External Reference"""

    name: Ident
    valid_types: List[RefType]


@datatype
class SubcktInstance:
    """Subckt / Module Instance"""

    name: Ident  # Instance Name
    module: Union["SubcktDef", ExternalRef]  # Module Definition or External Reference

    # Connections, either by-position or by-name
    conns: Union[List[Ident], List[Tuple[Ident, Ident]]]
    params: List[ParamVal]  # Parameter Values


class PrimitiveType(Enum):
    """Primitive Types Enumeration"""

    MOS = auto()
    BJT = auto()
    DIODE = auto()
    RESISTOR = auto()
    CAPACITOR = auto()
    INDUCTOR = auto()
    VSOURCE = auto()
    ISOURCE = auto()
    VCVS = auto()
    CCVS = auto()
    VCCS = auto()
    CCCS = auto()


@datatype
class PrimitiveInstance:
    """
    Primitive Instance

    Note at parsing-time, before models are sorted out,
    it is not always clear what is a port, model name, and parameter value.
    Primitives instead store positional and keyword arguments `args` and `kwargs`.
    """

    name: Ident  # Instance Name
    tp: PrimitiveType  # Primitive Element Type

    # FIXME: should args/ kwargs resolve to params and ports by here?
    args: List["Expr"]  # Positional Arguments
    kwargs: List[ParamVal]  # Keyword Arguments


@datatype
class Options:
    """Simulation Options"""

    vals: List[ParamVal]  # List of {name: value} pairs


@datatype
class SubcktDef:
    """Sub-Circuit / Module Definition"""

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List. FIXME: should this be part of our `Scope`?
    scope: "Scope"  # Internal definitions


@datatype
class ModelDef:
    """Model Definition"""

    name: Ident  # Model Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


class ModelType(Enum):
    MOS = auto()
    BJT = auto()
    DIODE = auto()
    RESISTOR = auto()
    CAPACITOR = auto()


@datatype
class ModelVariant:
    """Model Variant within a `ModelFamily`"""

    model: Ident  # Model Family Name
    variant: Ident  # Variant Name
    mtype: ModelType  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
class ModelFamily:
    """Model Family
    A set of related, identically named models, generally separated by limiting parameters such as {lmin, lmax} or {wmin, wmax}."""

    name: Ident  # Model Family Name
    mtype: Ident  # Model Type
    variants: List[ModelVariant]  # Variants


@datatype
class Include:
    """Include (a File) Statement"""

    path: Path


@datatype
class AhdlInclude:
    """Analog HDL Include (a File) Statement"""

    path: Path


@datatype
class LibSection:
    """Library Section
    A named section of a library, commonly incorporated with a `UseLib` or similar."""

    name: Ident  # Section Name
    scope: "Scope"  # Scoped Definitions


@datatype
class Library:
    """Library, as Generated by the Spice `.lib` Definition Card
    Includes a list of named `LibSection`s which can be included by their string-name,
    as common for "corner" inclusions e.g. `.inc "mylib.sp" "tt"`"""

    name: Ident  # Library Name
    sections: List[LibSection]  # Library Sections


@datatype
class UseLib:
    """Use a Library"""

    path: Path  # Library File Path
    section: Ident  # Section Name


@datatype
class End:
    """Empty class represents `.end` Statements"""

    ...


@datatype
class Variation:
    """Single-Parameter Variation Declaration"""

    name: Ident  # Parameter Name
    dist: str  # Distribution Name/Type
    std: "Expr"  # Standard Deviation


@datatype
class StatisticsBlock:
    """Statistical Descriptions"""

    process: Optional[List[Variation]]
    mismatch: Optional[List[Variation]]


@datatype
class Unknown:
    """Unknown Netlist Statement. Stored as an un-parsed string."""

    txt: str


@datatype
class SourceFile:
    path: Path  # Source File Path
    scope: "Scope"  # Scoped Definitions


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

    func: Union["FunctionDef", "ExternalRef"]  # Function Name
    args: List["Expr"]  # Arguments List


class ArgType(Enum):
    """Function Argument (and Return) Types Enumeration"""

    REAL = "real"
    # (that's it for now)


@datatype
class TypedArg:
    """Typed Function Argument"""

    name: Ident  # Argument Name
    tp: Ident  # Argument Type


@datatype
class Return:
    """Function Return Node"""

    val: "Expr"


# Types which can be used inside a function definition.
# Will of course grow, in time.
FuncStatement = Union[Return]


@datatype
class FunctionDef:
    """Function Definition"""

    name: Ident  # Function Name
    rtype: ArgType  # Return Type
    args: List[TypedArg]  # Argument List
    stmts: List[FuncStatement]  # Function Body/ Statements


# Expression Union
# Everything which can be used as a mathematical expression,
# and ultimately resolves to a scalar value at runtime.
#
# CST Expressions differ from their AST counterparts primarily by the inclusion of `ExternalRef`
# and the exclusion of `Ident`.
#
Expr = Union[
    "UnaryOp", "BinaryOp", "TernOp", Int, Float, MetricNum, Call, "ExternalRef"
]


@datatype
class UnaryOp:
    """Unary Operation"""

    tp: UnaryOperator  # Operator Type
    targ: Expr  # Target Expression


@datatype
class BinaryOp:
    """Binary Operation"""

    tp: BinaryOperator  # Enumerated Operator Type
    left: Expr  # Left Operand Expression
    right: Expr  # Right Operand Expression


@datatype
class TernOp:
    """Ternary Operation"""

    cond: Expr  # Condition Expression
    if_true: Expr  # Value if `cond` is True
    if_false: Expr  # Value if `cond` is False


@datatype
class Scope:
    """Hierarchical Scope
    Collection of named, typed definitions"""

    parent: Optional["Scope"]  # Parent Scope
    children: List["Scope"] = field(default_factory=list)  # Child Scopes

    # Contents defined in the source of this scope
    # Parameters
    params: Dict[str, ParamDecl] = field(default_factory=dict)
    # Subcircuit Definitions
    subckt_defs: Dict[str, SubcktDef] = field(default_factory=dict)
    # Model Definitions
    models: Dict[str, ModelDef] = field(default_factory=dict)
    # Model Family Definitions
    model_families: Dict[str, ModelFamily] = field(default_factory=dict)
    # Function Definitions
    functions: Dict[str, FunctionDef] = field(default_factory=dict)
    # Subcircuit Instances
    subckt_instances: Dict[str, SubcktInstance] = field(default_factory=dict)
    # Primitive Instances
    primitive_instances: Dict[str, PrimitiveInstance] = field(default_factory=dict)
    # Unidentified references to identifiers, and valid types which would fulfill them
    external_refs: Dict[str, ExternalRef] = field(default_factory=dict)
    # Other unnamed attributes such as `Option`s
    other: List[Any] = field(default_factory=list)  # FIXME: update type

    @classmethod
    def root(cls) -> "Scope":
        """Create a root scope, i.e. one with no parent."""
        return Scope(parent=None)


@datatype
class Program:
    """
    # Multi-File "Netlist Program"
    The name of this type is a bit misleading, but borrowed from more typical compiler-parsers.
    Spice-culture generally lacks a term for "the totality of a simulator invocation input",
    or even "a pile of source-files to be used together".
    So, `Program` it is.
    """

    scope: Scope = field(default_factory=Scope.root)


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
]
