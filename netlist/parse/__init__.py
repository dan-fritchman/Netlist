"""
# Netlist Parsing 
"""

import os, copy, codecs
from enum import Enum
from pathlib import Path
from warnings import warn
from typing import List, Tuple, Optional, Sequence, Union, Set, get_args
from pydantic.dataclasses import dataclass

# Local Imports
from .dialects import DialectParser, NetlistParseError
from ..data import ast, NetlistDialects


class ErrorMode(Enum):
    """ Enumerated Error-Response Strategies """

    RAISE = "raise"  # Raise any generated exceptions
    STORE = "store"  # Store error-generating content in `Unknown` elements


@dataclass
class ParseOptions:
    """ Parse Options """

    dialect: Optional[NetlistDialects] = None  # Initial netlist dialect
    errormode: ErrorMode = ErrorMode.RAISE  # Error-handling mode, enumerated in `ErrorMode`
    recurse: bool = True  # Whether to recurse into dependent & included files


def parse(
    src: Union[os.PathLike, Sequence[os.PathLike]],
    *,
    options: Optional[ParseOptions] = None,
) -> ast.Program:
    """ 
    Primary netist-parsing entry point.  
    Parse a multi-file netlist-`Program` starting at file or files `src`. 
    Optional argument `options` sets all behavior laid out by the `ParseOptions` class. 
    """

    if options is None:  # If not provided, create the default `ParseOptions`.
        options = ParseOptions()

    # Cover the cases of a single file
    if not isinstance(src, list):
        src = [src]

    # If an initial dialect has not been provided, infer it from the first file
    if options.dialect is None:
        options.dialect = default_dialect(src[0])

    # Create the parser, and parse each file
    p = Parser(src, options)
    p.parse()

    for e in p.entries():
        if isinstance(e, ast.Unknown):
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

    def __init__(
        self, src: List[Path], options: ParseOptions,
    ):
        self.options = options
        self.pending: List[Path] = [Path(s).absolute() for s in src]
        # Initialize the dialects for each file
        self.pending_dialects = {s: options.dialect for s in self.pending}
        self.done: Set[Path] = set()
        self.program = ast.Program([])
        self.dialect = options.dialect
        self.file_parser = None

    def parse(self) -> None:
        """ Parse all specified input, including (potentially recursive) dependencies. """

        while self.pending:
            sourcefile = self.parse_one()
            self.program.files.append(sourcefile)

    def parse_one(self) -> ast.SourceFile:
        """ Parse a single file, returning its resultant `SourceFile` tree. 
        Note the result-value *is not* stored in `self.program`. """

        path = self.pending.pop()

        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)

        # Source found; start parsing, to a list of Statements
        self.dialect = self.pending_dialects[p]
        self.file_parser = FileParser(self.dialect, self.options)
        path, stmts = self.file_parser.parse(p)
        self.done.add(p)

        # Filter out a few things
        stmts = [e for e in stmts if not isinstance(e, ast.DialectChange)]

        # Recursively descend into files it depends upon
        if self.options.recurse:
            file_stmts = [e for e in stmts if isinstance(e, (ast.Include, ast.UseLib))]
            self.recurse(path, file_stmts)

        # Form into a scope-hierarchical tree, and add it to our result-`Program`
        sourcefile = hierarchicalize(path, stmts)
        return sourcefile

    def recurse(self, path: Path, contents: List[ast.Statement]):
        """ Parse included-files in SourceFile `f` """
        for s in contents:
            if isinstance(s, (ast.Include, ast.UseLib)):
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
        self, dialect: NetlistDialects, options: ParseOptions,
    ):
        self.dialect = dialect
        self.dialect_parser = None
        self.options = options
        self.line_num = 1

    def notify(self, reason: ast.DialectChange):
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

    def _parse_file(self) -> List[ast.Statement]:
        """ Parse the (open) file-pointer at `self.fp` """

        # Create our iterator over file-lines, and core dialect-parser
        lines = iter(self.fp.readline, None)
        dcls = DialectParser.from_enum(self.dialect)
        self.dialect_parser = dcls.from_lines(lines=lines, parent=self)
        self.dialect_parser.start()

        stmts = []
        s = True
        while s:  # Main loop over statements
            si = ast.SourceInfo(
                line=self.dialect_parser.line_num, dialect=self.dialect_parser.enum,
            )

            try:  # Catch errors in primary parsing routines
                s = self.dialect_parser.parse_statement()
            except NetlistParseError as e:
                # Error Handling. Can either include `Unknown` statements, or re-raise.
                if self.options.errormode == ErrorMode.RAISE:
                    raise e
                # Otherwise, we include `Unknown` statements
                warn(e)  # Issue a warning
                s = ast.Unknown("")  # FIXME: include the line-content in Unknowns
                self.dialect_parser.eat_rest_of_statement()

            if s is not None:  # None-value indicates EOF
                s.source_info = si
                stmts.append(s)

        return stmts

    def parse(self, path: Path) -> Tuple[Path, List[ast.Statement]]:
        """ Parse the netlist `SourceFile` at `path`. """
        with codecs.open(path, "r", encoding="utf-8", errors="replace") as f:
            self.path = path
            self.fp = f
            stmts = self._parse_file()
        return (path, stmts)


def hierarchicalize(p: Path, stmts: List[ast.Statement]) -> ast.SourceFile:
    """ Convert a flat list of Statements into a hierarchical tree """
    nodes = HierarchyCollector(stmts).collect_source_file()
    return ast.SourceFile(p, nodes)


class HierarchyCollector:
    """ 
    Hierarchy-collector to create multi-statement tree structure, e.g.:
    * Libraries and Sections
    * (Potentially nested) sub-circuit definitions. 
    """

    def __init__(self, stmts: List[ast.Statement]):
        # Statements are stored in an iterator, which maintains a single state/ index.
        # So all these nested `collect_*` calls `for` loops are incrementing the same index through it.
        self.stmts = iter(stmts)

    def collect_source_file(self) -> List[ast.Entry]:
        """ Primary Entry Point. 
        Transform a list of flat file-entries to a hierarchical tree of file-nodes. 
        While the return-type is `List[Entry]`, these lists are designed as the content of a `SourceFile`. """

        nodes = []
        params = []  # Collect parameter-declarations into one

        for stmt in self.stmts:

            # Check for structured types, and if we find one, delegate to its collection-function
            if isinstance(stmt, ast.StartSubckt):
                s = self.collect_subckt(start=stmt)
            elif isinstance(stmt, ast.StartLib):
                s = self.collect_lib(start=stmt)
            elif isinstance(stmt, ast.StartLibSection):
                s = self.collect_lib_section(start=stmt)
            elif isinstance(stmt, ast.StartProtectedSection):
                s = self.collect_protected_section(start=stmt)

            # Append any parameter-declarations to our running list
            elif isinstance(stmt, ast.ParamDecl):
                params.append(stmt)
                continue
            elif isinstance(stmt, ast.ParamDecls):
                params.extend(stmt.params)
                continue

            else:  # Everything else, copy along
                s = stmt

            if not isinstance(s, get_args(ast.Entry)):
                raise TypeError(str(s))
            nodes.append(s)

        if params:
            # If we found any parameters, stick them at the beginning of the nodes-list.
            # This tends to be the most interpretable for eventual netlist formats.
            nodes = [ast.ParamDecls(params)] + nodes

        return nodes

    def collect_subckt(self, start: ast.StartSubckt) -> ast.SubcktDef:
        """ Collect a sub-circuit definition """

        nodes = []
        params = copy.copy(start.params)
        end: Optional[ast.EndSubckt] = None

        for stmt in self.stmts:

            if isinstance(stmt, ast.EndSubckt):
                end = stmt
                break  # done with this module

            # Parameter statements in subckt scope are "promoted" to be subckt/module-parameters.
            # We do so by appending them to any existing `start` parameters,
            # and by *not* repeating the param-declarations in the resultant AST tree.
            elif isinstance(stmt, ast.ParamDecl):
                params.append(stmt)
                continue
            elif isinstance(stmt, ast.ParamDecls):
                params.extend(stmt.params)
                continue

            elif isinstance(stmt, ast.StartSubckt):
                # Collect nested sub-circuit definitions
                s = self.collect_subckt(start=stmt)

            elif isinstance(
                stmt, (ast.StartLib, ast.StartLibSection, ast.StartProtectedSection)
            ):
                # Other structured types are not allowed here.
                self.fail()

            else:  # Anything else, copy along
                s = stmt

            if not isinstance(s, get_args(ast.Entry)):
                raise TypeError(str(s))
            nodes.append(s)

        if end is None:
            msg = f"SubcktDef lacking EndSubckt: {start}"
            self.fail(msg)

        return ast.SubcktDef(
            name=start.name, ports=start.ports, params=params, entries=nodes
        )

    def collect_lib_section(self, start: ast.StartLibSection) -> ast.LibSection:
        """ Collect a library section """

        nodes = []
        params = []  # Collect parameter-declarations into one
        end: Optional[ast.EndLibSection] = None

        for stmt in self.stmts:

            if isinstance(stmt, ast.EndLibSection):
                end = stmt
                break  # done with this section

            elif isinstance(stmt, (ast.StartLib, ast.StartLibSection)):
                self.fail()  # these don't nest; something's wrong

            elif isinstance(stmt, ast.StartSubckt):
                s = self.collect_subckt(start=stmt)

            elif isinstance(stmt, ast.StartProtectedSection):
                s = self.collect_protected_section(start=stmt)

            # Append any parameter-declarations to our running list
            elif isinstance(stmt, ast.ParamDecl):
                params.append(stmt)
                continue
            elif isinstance(stmt, ast.ParamDecls):
                params.extend(stmt.params)
                continue

            else:  # Anything else, copy along
                s = stmt

            if not isinstance(s, get_args(ast.Entry)):
                raise TypeError(str(s))
            nodes.append(s)

        # FIXME: not finding an `end` implies we reached end-of-input. Is that a valid or errant case? Error for now.
        if end is None:
            msg = f"LibrarySection lacking ending delimiter: {start}"
            self.fail(msg)

        if params:
            # If we found any parameters, stick them at the beginning of the nodes-list.
            # This tends to be the most interpretable for eventual netlist formats.
            nodes = [ast.ParamDecls(params)] + nodes

        return ast.LibSection(name=start.name, entries=nodes)

    def collect_lib(self, start: ast.StartLib) -> ast.Library:
        """ Collect a library definition """

        sections = []
        end: Optional[ast.EndLib] = None

        for stmt in self.stmts:

            # `StartLibSection` and `EndLib` are the *only* valid statements at the library-level.

            if isinstance(stmt, ast.EndLib):
                end = stmt
                break  # done with this library

            elif isinstance(stmt, ast.StartLibSection):
                s = self.collect_lib_section(start=stmt)
                sections.append(s)

            else:  # invalid type
                msg = f"Invalid statement in library: {stmt}"
                self.fail(msg)

        # FIXME: not finding an `end` implies we reached end-of-input. Is that a valid or errant case? Error for now.
        if end is None:
            msg = f"`Library` lacking `EndLib`: {start}"
            self.fail(msg)

        return ast.Library(name=start.name, sections=sections)

    def collect_protected_section(
        self, start: ast.StartProtectedSection
    ) -> ast.ProtectedSection:
        """ Collect a `ProtectedSection`. 
        Protected sections only have very basic support here, 
        for containing scalar elements such as `option`s.
        Any attempts to define nested elements such as `SubcktDef`, `LibSection`, or nested `ProtectedSection`s fail. """

        entries = []
        end: Optional[ast.EndProtectedSection] = None

        for stmt in self.stmts:

            if isinstance(stmt, ast.EndProtectedSection):
                end = stmt
                break  # done defining this section

            # Ban essentially all delimiter and nested elements
            banned = (
                ast.StartProtectedSection,
                ast.StartLib,
                ast.EndLib,
                ast.StartLibSection,
                ast.EndLibSection,
                ast.End,
                ast.SubcktDef,
                ast.Library,
                ast.LibSection,
                ast.ProtectedSection,
                ast.End,
            )
            if isinstance(stmt, banned):
                msg = "Invalid statement in ProtectedSection: {stmt}"
                self.fail(msg)

            # Copy everything else along
            if not isinstance(stmt, get_args(ast.Entry)):
                raise TypeError(str(stmt))
            entries.append(stmt)

        # FIXME: not finding an `end` implies we reached end-of-input. Is that a valid or errant case? Error for now.
        if end is None:
            msg = f"Protected Section lacking `unprotect`: {start}"
            self.fail(msg)

        return ast.ProtectedSection(entries)

    def fail(self, *args, **kwargs):
        raise NotImplementedError # FIXME! Some real content here