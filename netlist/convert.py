"""
# Netlist Conversion 

Interleaved parsing and writing of netlists, designed to speed up large inputs. 
"""

# Std-Lib Imports
import os
from io import StringIO
from pathlib import Path
from typing import Optional

# PyPi
from pydantic.dataclasses import dataclass

# Local Imports
from .data import NetlistDialects, Program


@dataclass
class ConversionIO:
    """Input and Output Datatype for Nelist-Format Conversion."""

    path: Path
    base_dir: Path
    dialect: Optional[NetlistDialects]


def convert(src: ConversionIO, dest: ConversionIO) -> None:
    """Convert a potentially multi-file netlist-program between dialects."""
    from .parse import Parser, ErrorMode as ParserErrorMode
    from .write import writer, ErrorMode as WriterErrorMode

    # Create the writer, and write initial header content
    netlister_cls = writer(dest.dialect)
    netlister = netlister_cls(
        src=Program([]), dest=StringIO(), errormode=WriterErrorMode.COMMENT
    )
    netlister.netlist()

    # Create the Parser
    parser = Parser(
        src=[src.path], dialect=src.dialect, errormode=ParserErrorMode.STORE
    )

    # For each source-file parsed, write and drop it from memory
    while parser.pending:
        sourcefile = parser.parse_one()

        if src.base_dir not in sourcefile.path.parents:
            raise TabError

        #
        dest_path = Path(
            str(sourcefile.path).replace(str(src.base_dir), str(dest.base_dir))
        )
        if not dest_path.parent.exists():
            os.makedirs(dest_path.parent)

        # Update the destination/target file
        netlister.dest = open(dest_path, "w")
        # And write up
        netlister.write_source_file(sourcefile)
        netlister.dest.flush()

        print(f"Converted {sourcefile.path} to {dest_path}")

    ...
