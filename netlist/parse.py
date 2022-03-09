"""
# Netlist Parsing 
"""

import os
import codecs
from enum import Enum
from pathlib import Path
from warnings import warn
from typing import List, Tuple, Optional, Sequence, Union, Set

# Local Imports
from .dialects import DialectParser
from .data import *


class ErrorMode(Enum):
    """ Enumerated Error-Response Strategies """

    RAISE = 0  # Raise any generated exceptions
    STORE = 1  # Store error-generating content in `Unknown` elements


def parse(
    src: Union[os.PathLike, Sequence[os.PathLike]],
    *,
    dialect: Optional[NetlistDialects] = None,
    errormode: ErrorMode = ErrorMode.RAISE,
) -> Program:
    """ 
    Primary netist-parsing entry point.  
    Parse a multi-file netlist-`Program` starting at file or files `src`. 
    
    Recurses into `src`'s dependencies as indicated by `Include` and `Library` directives.

    Optional argument `dialect` dictates the initial netlist-dialect. 
    If not provided, the default is inferred from the file-extension of the first entry in `src`.
    """

    # Cover the cases of a single file
    if not isinstance(src, list):
        src = [src]

    # If an initial dialect has not been provided, infer it from the first file
    dialect = dialect or default_dialect(src[0])

    # Create the parser, and parse each file
    p = Parser(src, dialect, errormode)
    p.parse()

    for e in p.entries():
        if isinstance(e, Unknown):
            warn(f"Unknown Netlist Entry {e}")

    # Return the parsed `Program`
    return p.program


def default_dialect(path: os.PathLike) -> NetlistDialects:
    """ 
    Infer a default dialect from a file name, particularly its suffix. 
    
    For files of suffix `scs` this is straightforwardly set to SPECTRE. 
    All other suffixes are less clear without knowing more context. 
    They are set to the most flexible SPECTRE_SPICE, 
    which generally uses SPICE-style syntax, but also includes dialect-changes. 
    """

    p = Path(path).absolute()
    if not p.exists() or not p.is_file():
        raise FileNotFoundError(p)
    if p.suffix == ".scs":
        return NetlistDialects.SPECTRE
    return NetlistDialects.SPECTRE_SPICE


class Parser:
    """ Multi-File "Netlist Program" Parser """

    def __init__(self, src: List[Path], dialect: NetlistDialects, errormode: ErrorMode):
        self.pending: List[Path] = [Path(s).absolute() for s in src]

        # Initialize the dialects for each file
        self.pending_dialects = {s: dialect for s in self.pending}

        self.done: Set[Path] = set()
        self.program = Program([])
        self.errormode = errormode
        self.dialect = dialect
        self.file_parser = None  ##FileParser(dialect, errormode)

    def parse(self) -> None:
        """ Parse all specified input, including (potentially recursive) dependencies. """

        while self.pending:
            sourcefile = self.parse_one()
            self.program.files.append(sourcefile)

    def parse_one(self) -> SourceFile:
        """ Parse a single file, returning its resultant `SourceFile` tree. 
        Note the result-value *is not* stored in `self.program`. """

        path = self.pending.pop()

        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)

        # Source found; start parsing, to a list of Statements
        self.dialect = self.pending_dialects[p]
        self.file_parser = FileParser(self.dialect, self.errormode)
        path, stmts = self.file_parser.parse(p)
        self.done.add(p)

        # Filter out a few things
        stmts = [e for e in stmts if not isinstance(e, DialectChange)]

        # Recursively descend into files it depends upon
        file_stmts = [e for e in stmts if isinstance(e, (Include, UseLib))]
        self.recurse(path, file_stmts)

        # Form into a scope-hierarchical tree, and add it to our result-`Program`
        sourcefile = hierarchicalize(path, stmts)
        return sourcefile

    def recurse(self, path: Path, contents: List[Statement]):
        """ Parse included-files in SourceFile `f` """
        for s in contents:
            if isinstance(s, (Include, UseLib)):
                # Differentiate absolute vs relative paths, relative to active source-file
                incp = s.path if s.path.is_absolute() else path.parent / s.path
                incp = incp.resolve()
                if not incp.exists() or not incp.is_file():
                    raise FileNotFoundError(incp)
                if incp in self.done:
                    continue

                # Sort out the initial dialect for the new file.
                # Spectre-family dialects expect included files to start in the dialect indicated by their file-extension.
                # All other dialects (that we know of) include files in their own dialect.
                if self.dialect in (
                    NetlistDialects.SPECTRE,
                    NetlistDialects.SPECTRE_SPICE,
                ):
                    # Update our dialect based on the file-extension we load
                    dialect_enum = default_dialect(incp)
                    # self.file_parser = FileParser(dialect_enum)
                else:  # Keep our existing dialect
                    dialect_enum = self.dialect
                    # self.file_parser = FileParser(dialect_enum)

                # Add the file and its dialect to the pending-queue
                self.pending.append(incp)
                self.pending_dialects[incp] = dialect_enum

                # # And kick off parsing of the included file
                # self.parse(incp)

    def entries(self):
        """ Iterator of all parsed Entries """
        for f in self.program.files:
            for e in f.contents:
                yield e


class FileParser:
    """ Single-File Parser 
    Produces a flat list of Statement-Entries """

    def __init__(
        self, dialect: NetlistDialects, errormode: ErrorMode = ErrorMode.RAISE
    ):
        self.dialect = dialect
        self.dialect_parser = None
        self.errormode = errormode
        self.line_num = 1

    def notify(self, reason: DialectChange):
        """ Notification from dialect-parser that something's up. 
        E.g. for dialect changes via `simulator lang` statements. """

        s = reason.dialect.lower().strip()
        if s == "spectre":
            dialect = NetlistDialects.SPECTRE
        elif s == "spice":
            dialect = NetlistDialects.SPECTRE_SPICE
        else:
            self.fail(f"Invalid `simulator lang` {s}")

        dcls = DialectParser.from_enum(dialect)
        self.dialect_parser = dcls.from_parser(self.dialect_parser)

    def _parse_file(self) -> List[Statement]:
        """ Parse the (open) file-pointer at `self.fp` """

        # Create our iterator over file-lines, and core dialect-parser
        lines = iter(self.fp.readline, None)
        dcls = DialectParser.from_enum(self.dialect)
        self.dialect_parser = dcls.from_lines(lines=lines, parent=self)
        self.dialect_parser.start()

        stmts = []
        s = True
        while s:  # Main loop over statements
            si = SourceInfo(
                line=self.dialect_parser.line_num, dialect=self.dialect_parser.enum,
            )

            try:  # Catch errors in primary parsing routines
                s = self.dialect_parser.parse_statement()
            except NetlistParseError as e:
                # Error Handling. Can either include `Unknown` statements, or re-raise.
                if self.errormode == ErrorMode.RAISE:
                    raise e
                # Otherwise, we include `Unknown` statements
                warn(e)  # Issue a warning
                s = Unknown("")  # FIXME: include the line-content in Unknowns
                self.dialect_parser.eat_rest_of_statement()

            if s is not None:  # None-value indicates EOF
                s.source_info = si
                stmts.append(s)

        return stmts

    def parse(self, path: Path) -> Tuple[Path, List[Statement]]:
        """ Parse the netlist `SourceFile` at `path`. """
        with codecs.open(path, "r", encoding="utf-8", errors="replace") as f:
            self.path = path
            self.fp = f
            stmts = self._parse_file()
        return (path, stmts)


def hierarchicalize(p: Path, stmts: List[Statement]) -> SourceFile:
    """ Convert a flat list of Statements into a hierarchical tree """
    nodes = HierarchyCollector(stmts).collect()
    return SourceFile(p, nodes)


class HierarchyCollector:
    """ 
    Hierarchy-collector to create multi-statement tree structure, e.g.:
    * Libraries and Sections
    * (Potentially nested) sub-circuit definitions. 
    """

    def __init__(self, stmts: List[Statement]):
        self.stmts = iter(stmts)

    def nxt(self) -> Optional[Statement]:
        """ Get our next `Statement`, or `None` if exhausted """
        return next(self.stmts, None)

    def collect(self) -> List[Entry]:
        """ Primary Entry Point. 
        Transform a list of flat file-entries to a hierarchical tree of file-nodes. """
        nodes = []
        while True:
            stmt = self.nxt()
            if stmt is None:
                break  # library-end on file-end

            if isinstance(stmt, StartSubckt):
                s = self.collect_subckt(start=stmt)
            elif isinstance(stmt, StartLib):
                s = self.collect_lib(start=stmt)
            elif isinstance(stmt, StartLibSection):
                s = self.collect_lib_section(start=stmt)
            else:
                s = stmt
            nodes.append(s)
        return nodes

    def collect_subckt(self, start: StartSubckt) -> SubcktDef:
        """ Collect a sub-circuit definition """

        nodes = []
        while True:
            stmt = self.nxt()
            if stmt is None:
                break  # library-end on file-end

            if isinstance(stmt, EndSubckt):
                break  # done with this module

            # Parameter statements in subckt scope are "promoted" to be subckt/module-parameters.
            # We do so by appending them to any existing `start` parameters,
            # and by *not* repeating the param-declarations in the resultant AST tree.
            if isinstance(stmt, ParamDecl):
                start.params.append(stmt)
                continue
            elif isinstance(stmt, ParamDecls):
                start.params.extend(stmt.params)
                continue

            if isinstance(stmt, StartSubckt):
                # Collect nested sub-circuit definitions
                s = self.collect_subckt(start=stmt)
            else:  # Anything else, copy along
                s = stmt
            nodes.append(s)

        return SubcktDef(
            name=start.name, ports=start.ports, params=start.params, entries=nodes
        )

    def collect_lib_section(self, start: StartLibSection) -> LibSection:
        """ Collect a library section """

        nodes = []
        while True:
            stmt = self.nxt()
            if stmt is None:
                break  # library-end on file-end

            if isinstance(stmt, EndLibSection):
                break  # done with this section
            elif isinstance(stmt, StartLibSection):
                self.fail()  # these don't nest; something's wrong
            elif isinstance(stmt, StartSubckt):
                s = self.collect_subckt(start=stmt)
            else:  # Anything else, copy along
                s = stmt
            nodes.append(s)

        return LibSection(name=start.name, entries=nodes)

    def collect_lib(self, start: StartLib) -> Library:
        """ Collect a library definition """

        sections = []
        while True:
            stmt = self.nxt()
            if stmt is None:
                break  # library-end on file-end

            if isinstance(stmt, EndLib):
                break  # done with this library
            elif isinstance(stmt, EndLibSection):
                # something went wrong; lib section ends without beginning
                msg = "Invalid `EndLibSection` without `StartLibSection`"
                self.fail(msg)
            elif isinstance(stmt, StartLibSection):
                s = self.collect_lib_section(start=stmt)
                sections.append(s)
            else:
                msg = f"Invalid statement in library: {stmt}"
                self.fail(msg)  # invalid type

        return Library(name=start.name, sections=sections)

