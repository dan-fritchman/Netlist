""" 
# Netlist Shared Data

Core types used by several layers of netlist representations 
"""

# Std-Lib Imports
from enum import Enum
from typing import Union

# PyPi Imports
from pydantic.dataclasses import dataclass


class NetlistParseError(Exception):
    """ Netlist Parse Error """

    @staticmethod
    def throw(*args, **kwargs):
        """ Exception-raising debug wrapper. Breakpoint to catch `NetlistParseError`s. """
        raise NetlistParseError(*args, **kwargs)


class NetlistDialects(Enum):
    """ Enumerated, Supported Netlist Dialects """

    SPECTRE = "spectre"
    SPECTRE_SPICE = "spectre_spice"
    SPICE = "spice"
    HSPICE = "hspice"
    NGSPICE = "ngspice"
    XYCE = "xyce"
    CDL = "cdl"

    @staticmethod
    def get(spec: "NetlistFormatSpec") -> "NetlistDialects":
        """ Get the format specified by `spec`, in either enum or string terms. 
        Only does real work in the case when `spec` is a string, otherwise returns it unchanged. """
        if isinstance(spec, (NetlistDialects, str)):
            return NetlistDialects(spec)
        raise TypeError


# Type-alias for specifying format, either in enum or string terms
NetlistFormatSpec = Union[NetlistDialects, str]


def to_json(arg) -> str:
    """ Dump any `pydantic.dataclass` or simple combination thereof to JSON string. """
    import json
    from pydantic.json import pydantic_encoder

    return json.dumps(arg, indent=2, default=pydantic_encoder)


@dataclass
class SourceInfo:
    """ Parser Source Information """

    line: int  # Source-File Line Number
    dialect: "NetlistDialects"  # Netlist Dialect
