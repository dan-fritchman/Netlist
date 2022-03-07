""" 
# Netlist Writing 

Exports a netlist `Program` to a netlist format.
"""

# Std-Lib Imports
from typing import Union, IO
from enum import Enum

# Local Imports
from ..data import NetlistDialects, NetlistFormatSpec, Program
from .spice import (
    SpiceNetlister,
    HspiceNetlister,
    CdlNetlister,
    XyceNetlister,
    NgspiceNetlister,
)


def writer(fmt) -> type:
    """ Get the writer-class paired with the netlist-format. """
    if fmt == NetlistFormat.XYCE:
        return XyceNetlister
    raise ValueError

    # FIXME: anything and everything else!
    
    if fmt == NetlistFormat.SPECTRE:
        return SpectreNetlister
    if fmt == NetlistFormat.VERILOG:
        return VerilogNetlister
    if fmt == NetlistFormat.SPICE:
        return SpiceNetlister
    if fmt == NetlistFormat.HSPICE:
        return HspiceNetlister
    if fmt == NetlistFormat.NGSPICE:
        return NgspiceNetlister
    if fmt == NetlistFormat.CDL:
        return CdlNetlister


def netlist(src: Program, dest: IO, fmt: NetlistFormatSpec = "spectre") -> None:
    """ Write netlist-`Program` `src` to destination `dest`. 

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
    Format-specifier `fmt` may be any of the `NetlistFormatSpec` enumerated values 
    or their string equivalents.
    """
    fmt_enum = NetlistFormat.get(fmt)
    netlister_cls = fmt_enum.netlister()
    netlister = netlister_cls(pkg, dest)
    netlister.netlist()


# Set our exported content for star-imports
__all__ = ["netlist"]
