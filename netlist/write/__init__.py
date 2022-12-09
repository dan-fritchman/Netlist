""" 
# Netlist Writing 

Exports a netlist `Program` to a netlist format.
"""

# Std-Lib Imports
from typing import Union, IO
from enum import Enum

# Local Imports
from ..data import NetlistDialects, NetlistFormatSpec, Program
from .base import ErrorMode
from .spice import (
    SpiceNetlister,
    HspiceNetlister,
    CdlNetlister,
    XyceNetlister,
    NgspiceNetlister,
)


def writer(fmt: NetlistDialects = NetlistDialects.XYCE) -> type:
    """Get the writer-class paired with the netlist-format."""
    if fmt == NetlistDialects.XYCE:
        return XyceNetlister
    raise ValueError

    # FIXME: anything and everything else!
    if fmt == NetlistDialects.SPECTRE:

        return SpectreNetlister
    if fmt == NetlistDialects.VERILOG:
        return VerilogNetlister
    if fmt == NetlistDialects.SPICE:
        return SpiceNetlister
    if fmt == NetlistDialects.HSPICE:
        return HspiceNetlister
    if fmt == NetlistDialects.NGSPICE:
        return NgspiceNetlister
    if fmt == NetlistDialects.CDL:
        return CdlNetlister


def netlist(
    src: Program,
    dest: IO,
    fmt: NetlistFormatSpec = "xyce",
    errormode: ErrorMode = ErrorMode.RAISE,
) -> None:
    """Write netlist-`Program` `src` to destination `dest`.

    Example usages:
    ```python
    netlist(pkg, dest=open('mynetlist.v', 'w'), fmt='verilog')
    ```
    ```python
    s = StringIO()
    netlist(pkg, dest=s, fmt='spectre')
    ```
    ```python
    import sys
    netlist(pkg, dest=sys.stdout, fmt='spice')
    ```

    Primary argument `src` must be a `Program`.
    Destination `dest` may be anything that supports the `typing.IO` bundle,
    commonly including open file-handles. `StringIO` is particularly helpful
    for producing a netlist in an in-memory string.
    Format-specifier `fmt` may be any of the `NetlistDialectsSpec` enumerated values
    or their string equivalents.
    """
    fmt_enum = NetlistDialects.get(fmt)
    netlister_cls = writer(fmt_enum)
    netlister = netlister_cls(src=src, dest=dest, errormode=errormode)
    netlister.netlist()


# Set our exported content for star-imports
__all__ = ["netlist"]
