""" 
# Netlist Shared Data

Core types used by several layers of netlist representations 
"""

# Std-Lib Imports
from enum import Enum

# PyPi Imports
from pydantic.dataclasses import dataclass


class NetlistDialects(Enum):
    """ Enumerated, Supported Netlist Dialects """

    SPECTRE = "spectre"
    SPECTRE_SPICE = "spectre_spice"
    SPICE = "spice"
    HSPICE = "hspice"
    NGSPICE = "ngspice"
    XYCE = "xyce"
    CDL = "cdl"


def to_json(arg) -> str:
    """ Dump any `pydantic.dataclass` or simple combination thereof to JSON string. """
    import json
    from pydantic.json import pydantic_encoder

    return json.dumps(arg, indent=2, default=pydantic_encoder)


@dataclass
class SourceInfo:
    """ Parser Source Information """

    line: int  # Source-File Line Number
    dialect: NetlistDialects  # Netlist Dialect


@dataclass
class Ident:
    """ Identifier """

    name: str


@dataclass
class Int:
    """ Integer Number """

    val: int


@dataclass
class Float:
    """ Floating Point Number """

    val: float


@dataclass
class MetricNum:
    """ Number with Metric Suffix """

    val: str  # No conversion, just stored as string for now


class UnaryOperator(Enum):
    """ Enumerated, Supported Unary Operators 
    Values generally equal their string-format equivalents. """

    PLUS = "+"
    NEG = "-"


class BinaryOperator(Enum):
    """ Enumerated, Supported Binary Operators 
    Values generally equal their string-format equivalents. """

    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"
    POW = "^"  # Note there is some divergence between caret and double-star here.
    GT = ">"
    LT = "<"
    GE = ">="
    LE = "<="
