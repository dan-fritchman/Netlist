""" 

# Netlist Data Model 

All elements in the netlist-internal "IR", 
primarily in the form of dataclasses. 

"""

# Std-Lib Imports
from enum import Enum
from pathlib import Path
from dataclasses import field
from typing import Optional, Union, List, Tuple, Generic, TypeVar

# PyPi Imports
from pydantic.dataclasses import dataclass
from pydantic.generics import GenericModel


class NetlistParseError(Exception):
    """Netlist Parse Error"""

    @staticmethod
    def throw(*args, **kwargs):
        """Exception-raising debug wrapper. Breakpoint to catch `NetlistParseError`s."""
        raise NetlistParseError(*args, **kwargs)


class NetlistDialects(Enum):
    """Enumerated, Supported Netlist Dialects"""

    SPECTRE = "spectre"
    SPECTRE_SPICE = "spectre_spice"
    SPICE = "spice"
    HSPICE = "hspice"
    NGSPICE = "ngspice"
    XYCE = "xyce"
    CDL = "cdl"

    @staticmethod
    def get(spec: "NetlistFormatSpec") -> "NetlistDialects":
        """Get the format specified by `spec`, in either enum or string terms.
        Only does real work in the case when `spec` is a string, otherwise returns it unchanged."""
        if isinstance(spec, (NetlistDialects, str)):
            return NetlistDialects(spec)
        raise TypeError


# Type-alias for specifying format, either in enum or string terms
NetlistFormatSpec = Union[NetlistDialects, str]


def to_json(arg) -> str:
    """Dump any `pydantic.dataclass` or simple combination thereof to JSON string."""
    import json
    from pydantic.json import pydantic_encoder

    return json.dumps(arg, indent=2, default=pydantic_encoder)


@dataclass
class SourceInfo:
    """Parser Source Information"""

    line: int  # Source-File Line Number
    dialect: "NetlistDialects"  # Netlist Dialect


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
class Ident:
    """Identifier"""

    name: str


class RefType(Enum):
    """External Reference Types Enumeration
    Store on each `ExternalRef` to note which types would be valid in context."""

    SUBCKT = "SUBCKT"  # Sub-circuit definition
    MODEL = "MODEL"  # Model definition
    PARAM = "PARAM"  # Parameter reference
    FUNCTION = "FUNCTION"  # Function definition
    FILEPATH = "FILEPATH"  # File-system path, e.g. in `Include`s not parsed


@datatype
class ExternalRef:
    """# External Reference
    Name-based reference to something outside the netlist-program."""

    # Referred-to identifier
    ident: Ident
    # List of types which this can validly refer to
    types: List[RefType] = field(default_factory=list)


# Generic type of "the thing that a `Ref` refers to"
Referent = TypeVar("Referent")


class Ref(GenericModel, Generic[Referent]):
    """# Reference to another Netlist object
    Intially an identifier, then in later stages resolved to a generic `Referent`."""

    # Referred-to identifier
    ident: Ident
    # Resolved referent, or `None` if unresolved.
    resolved: Optional[Union[Referent, ExternalRef]] = None


@datatype
class HierPath:
    """Hierarchical Path Identifier"""

    path: List[Ident]  # FIXME: Ref?


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


@datatype
class Options:
    """Simulation Options"""

    name: Optional[Ident]  # Option Name. FIXME: could this be removed
    vals: List[ParamVal]  # List of name: value pairs


@datatype
class StartSubckt:
    """Start of a Subckt / Module Definition"""

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations


@datatype
class EndSubckt:
    """End of a Subckt / Module Definition"""

    name: Optional[Ident]


@datatype
class SubcktDef:
    """Sub-Circuit / Module Definition"""

    name: Ident  # Module/ Subcircuit Name
    ports: List[Ident]  # Port List
    params: List[ParamDecl]  # Parameter Declarations
    entries: List["Entry"]


@datatype
class ModelDef:
    """Model Definition"""

    name: Ident  # Model Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
class ModelVariant:
    """Model Variant within a `ModelFamily`"""

    model: Ident  # Model Family Name
    variant: Ident  # Variant Name
    mtype: Ident  # Model Type
    args: List[Ident]  # Positional Arguments
    params: List[ParamDecl]  # Parameter Declarations & Defaults


@datatype
class ModelFamily:
    """Model Family
    A set of related, identically named models, generally separated by limiting parameters such as {lmin, lmax} or {wmin, wmax}."""

    name: Ident  # Model Family Name
    mtype: Ident  # Model Type
    variants: List[ModelVariant]  # Variants


# The primary `Model` type-union includes both single-variant and multi-variant versions
Model = Union[ModelDef, ModelFamily]


@datatype
class Instance:
    """Subckt / Module Instance"""

    name: Ident  # Instance Name
    module: Ref[Union[SubcktDef, Model]]  # Module/ Subcircuit Reference

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
class Include:
    """Include (a File) Statement"""

    path: Path


@datatype
class AhdlInclude:
    """Analog HDL Include (a File) Statement"""

    path: Path


@datatype
class StartLib:
    """Start of a `Library`"""

    name: Ident


@datatype
class EndLib:
    """End of a `Library`"""

    name: Optional[Ident]


@datatype
class StartLibSection:
    """Start of a `LibrarySection`"""

    name: Ident


@datatype
class EndLibSection:
    """End of a `LibrarySection`"""

    name: Ident


@datatype
class LibSection:
    """Library Section
    A named section of a library, commonly incorporated with a `UseLib` or similar."""

    name: Ident  # Section Name
    entries: List["Entry"]  # Entry List


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
class DialectChange:
    """Netlist Dialect Changes, e.g. `simulator lang=xyz`"""

    dialect: str


# Union of "flat" statements lacking (substantial) hierarchy
FlatStatement = Union[
    Instance,
    Primitive,
    ParamDecls,
    ModelDef,
    ModelVariant,
    ModelFamily,
    DialectChange,
    "FunctionDef",
    Unknown,
    Options,
    Include,
    AhdlInclude,
    UseLib,
    StatisticsBlock,
]

# Statements which indicate the beginning and end of hierarchical elements,
# and ultimately disappear into the structured AST
DelimStatement = Union[
    StartLib,
    EndLib,
    StartLibSection,
    EndLibSection,
    End,
]

# Statements
# The union of types which can appear in first-pass parsing netlist
Statement = Union[FlatStatement, DelimStatement]

# Entries - the union of types which serve as "high-level" AST nodes,
# i.e. those which can be the direct children of a `SourceFile`.
Entry = Union[FlatStatement, SubcktDef, Library, LibSection, End]


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


@datatype
class Int:
    """Integer Number"""

    val: int


@datatype
class Float:
    """Floating Point Number"""

    val: float


@datatype
class MetricNum:
    """Number with Metric Suffix"""

    val: str  # No conversion, just stored as string for now


@datatype
class TypedArg:
    """Typed Function Argument"""

    tp: Ident  # Argument Type
    name: Ident  # Argument Name


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
    rtype: Ident  # Return Type
    args: List[TypedArg]  # Argument List
    stmts: List[FuncStatement]  # Function Body/ Statements


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

    func: Ref[FunctionDef]  # Function Name
    args: List["Expr"]  # Arguments List


class UnaryOperator(Enum):
    """Enumerated, Supported Unary Operators
    Values generally equal their string-format equivalents."""

    PLUS = "+"
    NEG = "-"


@datatype
class UnaryOp:
    """Unary Operation"""

    tp: UnaryOperator  # Operator Type
    targ: "Expr"  # Target Expression


class BinaryOperator(Enum):
    """Enumerated, Supported Binary Operators
    Values generally equal their string-format equivalents."""

    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"
    POW = "^"  # Note there is some divergence between caret and double-star here.
    GT = ">"
    LT = "<"
    GE = ">="
    LE = "<="


@datatype
class BinaryOp:
    """Binary Operation"""

    tp: BinaryOperator  # Enumerated Operator Type
    left: "Expr"  # Left Operand Expression
    right: "Expr"  # Right Operand Expression


@datatype
class TernOp:
    """Ternary Operation"""

    cond: "Expr"  # Condition Expression
    if_true: "Expr"  # Value if `cond` is True
    if_false: "Expr"  # Value if `cond` is False


# Expression Union
# Everything which can be used as a mathematical expression,
# and ultimately resolves to a scalar value at runtime.
Expr = Union[UnaryOp, BinaryOp, TernOp, Int, Float, MetricNum, Ref, Call]


# Update all the forward type-references
for tp in datatypes:
    tp.__pydantic_model__.update_forward_refs()

# And solely export the defined datatypes
# (at least with star-imports, which are hard to avoid using with all these types)
__all__ = [tp.__name__ for tp in datatypes] + [
    "NetlistDialects",
    "NetlistParseError",
    "BinaryOperator",
    "UnaryOperator",
    "Expr",
    "Entry",
    "Ref",
    "RefType",
    "ExternalRef",
    "Model",
    "Statement",
    "to_json",
]
