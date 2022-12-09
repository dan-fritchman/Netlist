"""
# Netlist "Compilation" 

Parse and convert a netlist-program into flattened, simulatable form. 
Occurs in three primary steps:

1. Parsing produces an AST `Program`
2. Concretizing converts this to a nested set of definition `Scope`s 
3. TODO: Elaboration flattens this into a single list of de-parameterized `Primitive` instances
"""
import os
from typing import Union, Sequence, Optional

# Local Imports
from .data import Program
from .ast_to_cst import Scope, ast_to_cst
from .parse import ParseOptions, parse_files, parse_str


def compile(
    src: Union[str, os.PathLike, Sequence[os.PathLike]],
    *,
    options: Optional[ParseOptions] = None
) -> Scope:
    if isinstance(src, str):
        ast_program: Program = parse_str(src=src, options=options)
    else:
        ast_program: Program = parse_files(src=src, options=options)
    return ast_to_cst(ast_program)
