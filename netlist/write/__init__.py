""" 
# Netlist Writing 

Exports a netlist `Program` to a netlist format.
"""

# Std-Lib Imports
from typing import IO, Optional

# PyPi Imports
from pydantic.dataclasses import dataclass

# Local Imports
from ..data import NetlistDialects, Program
from .base import ErrorMode
from .spice import (
    SpiceNetlister,
    HspiceNetlister,
    CdlNetlister,
    XyceNetlister,
    NgspiceNetlister,
)


def writer(fmt: NetlistDialects = NetlistDialects.XYCE) -> type:
    """ Get the writer-class paired with the netlist-format. """
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


@dataclass
class WriteOptions:
    """ Netlist Writing Options """

    fmt: NetlistDialects = NetlistDialects.XYCE  # Target format, in enumerated or string form
    errormode: ErrorMode = ErrorMode.RAISE  # Error-handling mode, enumerated in `ErrorMode`


def netlist(src: Program, dest: IO, options: Optional[WriteOptions] = None,) -> None:
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
    
    Optional `WriteOptions` argument `options` sets the target format, error-handling strategies, 
    and any further optional settings. 
    """
    if options is None:
        # Create the default options
        options = WriteOptions()

    netlister_cls = writer(options.fmt)
    netlister = netlister_cls(src=src, dest=dest, errormode=options.errormode)
    netlister.netlist()


# Set our exported content for star-imports
__all__ = ["netlist", "NetlistDialects", "WriteOptions"]
