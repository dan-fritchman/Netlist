"""
# Netlist Parsing 
"""

import os
import codecs
from enum import Enum
from pathlib import Path
from warnings import warn
from typing import List, Tuple, Optional

# PyPi Imports
from pydantic import ValidationError

# Local Imports
from .dialects import DialectParser
from .data import *


class ErrorMode(Enum):
    """ Enumerated Responses to Parsing Errors """

    RAISE = 0  # Raise any generated exceptions
    STORE = 1  # Store error-generating content in `Unknown` elements


def parse(
    path: os.PathLike,
    *,
    dialect: Optional[NetlistDialects] = None,
    errormode: ErrorMode = ErrorMode.RAISE,
) -> Program:
    """ 
    Primary netist-parsing entry point.  
    Parse a multi-file netlist-`Program` starting at file `path`. 
    
    Recurses into `path`'s dependencies as indicated by `Include` and `Library` directives.

    Optional argument `dialect` dictates the initial netlist-dialect. 
    If not provided, the default is inferred from the file-extension of `path`.
    """

    dialect = dialect or default_dialect(path)
    p = Parser(dialect, errormode)
    p.parse(path)
    for e in p.entries():
        if isinstance(e, Unknown):
            warn(f"Unknown Netlist Entry {e}")
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
    if p.suffix == "scs":
        return NetlistDialects.SPECTRE
    return NetlistDialects.SPECTRE_SPICE


class Parser:
    """ Multi-File "Netlist Program" Parser """

    def __init__(
        self, dialect: NetlistDialects, errormode: ErrorMode = ErrorMode.RAISE
    ):
        self.deps: List[Path] = []
        self.program = Program([])
        self.errormode = errormode
        self.dialect = dialect
        self.file_parser = FileParser(dialect, errormode)

    def parse_file(self, p: Path):
        """ Parse a single file, including any follow-on steps """

        # Perform our primary file-parsing, to a list of Entries
        path, stmts = self.file_parser.parse(p)
        # Filter out a few things
        stmts = [e for e in stmts if not isinstance(e, DialectChange)]
        # And form into a scope-hierarchical tree
        return hierarchicalize(path, stmts)

    def parse(self, path: os.PathLike):
        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)

        # Source Found; Start Parsing
        self.deps.append(p)
        f = self.parse_file(p)
        # Add it to our parsed result
        self.program.files.append(f)
        # And recursively descend into files it depends upon
        self.recurse(f)

    def recurse(self, f: SourceFile):
        """ Parse included-files in SourceFile `f` """
        for s in f.contents:
            if isinstance(s, (Include, UseLib)):
                # Differentiate absolute vs relative paths, relative to active source-file
                incp = s.path if s.path.is_absolute() else f.path.parent / s.path
                incp = incp.resolve()
                if not incp.exists() or not incp.is_file():
                    raise FileNotFoundError(incp)
                if incp in self.deps:
                    continue

                from .dialects.spectre import SpectreMixin

                if isinstance(self.dialect, SpectreMixin):
                    # Update our dialect based on the file-extension we load
                    d_enum = default_dialect(incp)
                    self.file_parser = FileParser(d_enum)
                else:
                    self.file_parser = FileParser(self.dialect)
                self.parse(incp)

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
        """ Get our next `Statement`, or None if exhausted """
        return next(self.stmts, None)
        try:
            return self.stmts.pop(0)
        except IndexError:
            return None

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
                s = self.collect_lib_section(start=e)
                sections.append(s)
            else:
                msg = f"Invalid statement in library: {stmt}"
                self.fail(msg)  # invalid type

        return Library(name=start.name, sections=sections)

