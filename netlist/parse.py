import os
import codecs

from pathlib import Path
from warnings import warn
from typing import List, Tuple

from .dialects import DialectParser
from .data import *


class Node:
    """ Helper for catching `pydantic.ValidationError`s. 
    Usage: Replace 
    `MyDataCls(*args, **kwargs)` with 
    `Node.new(MyDataCls, args, kwargs)`
    Validation errors can be analyzed by breaking or otherwise debugging 
    from the `except`-clause below. """

    @staticmethod
    def new(cls, args=None, kwargs=None):
        try:
            if not args:
                args = tuple()
            if not kwargs:
                kwargs = dict()
            return cls(*args, **kwargs)
        except ValidationError as e:
            raise e


def parse(path: os.PathLike, *, dialect=None) -> Program:
    """ Parse a Multi-File Netlist-Program """
    d = dialect or default_dialect(path)
    p = Parser(d)
    p.parse(path)
    for e in p.entries():
        if isinstance(e.content, Unknown):
            warn(f"Unknown Netlist Entry {e}")
    return p.program


def default_dialect(path: os.PathLike) -> NetlistDialects:
    """ Infer a default dialect from a file name, particularly its suffix. 
    For files of suffix `scs` this is straightforwardly set to SPECTRE. 
    All other suffixes are less clear without knowing more context. 
    They are set to the most flexible SPECTRE_SPICE, which includes dialect-changes. """

    p = Path(path).absolute()
    if not p.exists() or not p.is_file():
        raise FileNotFoundError(p)
    if p.suffix == ".scs":
        return NetlistDialects.SPECTRE
    return NetlistDialects.SPECTRE_SPICE


class Parser:
    """ Multi-File "Netlist Program" Parser """

    def __init__(self, dialect: NetlistDialects, recursive: bool = True):
        self.deps: List[Path] = []
        self.pending: List[Tuple[Path, NetlistDialects]] = []
        self.program = Program([])
        self.recursive = recursive
        self.dialect = dialect
        self.file_parser = FileParser(dialect)

    def parse_file(self, p: Path):
        """ Parse a single file, including any follow-on steps """

        # Perform our primary file-parsing, to a list of Entries
        path, stmts = self.file_parser.parse(p)
        # Recursively descend into files it depends upon
        if self.recursive:
            self.recurse(path, stmts)
        # Filter out a few things
        stmts = [e for e in stmts if not isinstance(e.content, DialectChange)]
        # And form into a scope-hierarchical tree
        rv = hierarchicalize(path, stmts)
        return rv

    def parse(self, path: os.PathLike):
        """ Parse a potentially multi-file netlist-Program, starting from `path`. """

        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)

        # Source Found; Start Parsing
        self.deps.append(p)
        f = self.parse_file(p)
        # Add it to our parsed result
        self.program.files.append(f)

        # If we have any pending dependencies, parse them too
        if self.pending:
            path, dialect = self.pending.pop()
            self.file_parser = FileParser(dialect)
            self.parse(path)

        # Return `self` to allow nested calls, e.g. p.parse(path1).parse(path2)
        return self

    def recurse(self, path: Path, entries: List[FileEntry]):
        """ Add included-files in `entries` to our pending-list """
        from .dialects.spectre import SpectreMixin

        for e in entries:
            s = e.content
            if isinstance(s, (Include, UseLib)):
                # Differentiate absolute vs relative paths, relative to active source-file
                incp = s.path if s.path.is_absolute() else path.parent / s.path
                incp = incp.resolve()
                if not incp.exists() or not incp.is_file():
                    raise FileNotFoundError(incp)
                if incp in self.deps or incp in [p[0] for p in self.pending]:
                    continue

                # Sort out which initial dialect to parse, based on current dialect and file-extension
                dialect = (
                    default_dialect(incp)
                    if isinstance(self.file_parser.dialect, SpectreMixin)
                    else self.dialect
                )
                self.pending.append((incp, dialect))

    def entries(self):
        """ Iterator of all parsed Entries """
        for f in self.program.files:
            for e in f.contents:
                yield e


class FileParser:
    """ Single-File Parser 
    Produces a flat list of Statement-Entries """

    def __init__(self, dialect: NetlistDialects):
        self.dialect = dialect
        self.dialect_parser = None
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
            NetlistParseError.throw()

        dcls = DialectParser.from_enum(dialect)
        self.dialect_parser = dcls.from_parser(self.dialect_parser)

    def _parse_file(self) -> List[FileEntry]:
        """ Parse the (open) file-pointer at `self.fp` """

        # Create our iterator over file-lines, and core dialect-parser
        lines = iter(self.fp.readline, None)
        dcls = DialectParser.from_enum(self.dialect)
        self.dialect_parser = dcls.from_lines(lines=lines, parent=self)
        self.dialect_parser.start()

        entries = []
        s = True
        while s:  # Main loop over statements
            si = SourceInfo(
                line=self.dialect_parser.line_num, dialect=self.dialect_parser.enum,
            )
            try:  # Catch errors in primary parsing routines
                s = self.dialect_parser.parse_statement()
            except (NetlistParseError, ValidationError) as e:
                # Error Handling. Can either include `Unknown` statements, or re-raise.
                warn(e)
                s = Unknown("")  # FIXME: include the line-content in Unknowns
                self.dialect_parser.eat_rest_of_statement()
                # NetlistParseError.throw(
                #     f"{str(e)} Error Parsing {self.path} Line {start_line_num}: \n{lines} "
                # )
            if s is not None:  # None-value indicates EOF
                e = FileEntry(s, si)
                entries.append(e)

        return entries

    def parse(self, path: Path) -> Tuple[Path, List[FileEntry]]:
        """ Parse the netlist `SourceFile` at `path`. """
        with codecs.open(path, "r", encoding="utf-8", errors="replace") as f:
            self.path = path
            self.fp = f
            entries = self._parse_file()
        return (path, entries)


def hierarchicalize(p: Path, entries: List[FileEntry]) -> SourceFile:
    nodes = HierarchyCollector(entries).collect()
    return SourceFile(p, nodes)


class HierarchyCollector:
    """ Second-pass hierarchicy-collector to create multi-statement tree structure, e.g.:
    * Libraries and Sections
    * (Potentially nested) Sub-Circuit definitions. """

    def __init__(self, entries: List[FileEntry]):
        self.entries = entries

    def nxt(self) -> FileEntry:
        try:
            return self.entries.pop(0)
        except IndexError:
            return None

    def collect(self) -> List[TreeEntry]:
        """ Primary Entry Point. 
        Transform a list of flat file-entries to a hierarchical tree. """
        nodes = []
        while True:
            e = self.nxt()
            if e is None:  # EOF
                break
            stmt = e.content
            if isinstance(stmt, StartSubckt):
                s = self.collect_subckt(start=e)
                nodes.append(Node.new(TreeEntry, (s, e.source_info)))
            elif isinstance(stmt, StartLib):
                s = self.collect_lib(start=e)
                nodes.append(Node.new(TreeEntry, (s, e.source_info)))
            elif isinstance(stmt, StartLibSection):
                s = self.collect_lib_section(start=e)
                nodes.append(Node.new(TreeEntry, (s, e.source_info)))
            else:
                nodes.append(Node.new(TreeEntry, (e.content, e.source_info)))
        return nodes

    def collect_subckt(self, start: StartSubckt) -> SubcktDef:
        nodes = []
        while True:
            e = self.nxt()
            if e is None:
                NetlistParseError.throw()
            stmt = e.content
            if isinstance(stmt, EndSubckt):
                break  # done with this module
            elif isinstance(stmt, StartSubckt):
                s = self.collect_subckt(start=e)
                nodes.append(Node.new(TreeEntry, (s, e.source_info)))
            else:  # Anything else, copy along
                nodes.append(Node.new(TreeEntry, (e.content, e.source_info)))
        st = start.content
        return SubcktDef(name=st.name, ports=st.ports, params=st.params, entries=nodes)

    def collect_lib_section(self, start: StartLibSection) -> LibSection:
        nodes = []
        while True:
            e = self.nxt()
            if e is None:
                break  # section-end on file-end
            stmt = e.content
            if isinstance(stmt, EndLibSection):
                break  # done with this section
            elif isinstance(stmt, StartLibSection):
                NetlistParseError.throw()  # these don't nest; something's wrong
            elif isinstance(stmt, StartSubckt):
                s = self.collect_subckt(start=e)
                nodes.append(Node.new(TreeEntry, (s, e.source_info)))
            else:  # Anything else, copy along
                nodes.append(Node.new(TreeEntry, (e.content, e.source_info)))
        start = start.content
        return LibSection(name=start.name, entries=nodes)

    def collect_lib(self, start: StartLib) -> Library:
        sections = []
        while True:
            e = self.nxt()
            if e is None:
                break  # library-end on file-end
            stmt = e.content
            if isinstance(stmt, EndLib):
                break  # done with this library
            elif isinstance(stmt, EndLibSection):
                NetlistParseError.throw()  # something went wrong; lib section ends without beginning
            elif isinstance(stmt, StartLibSection):
                s = self.collect_lib_section(start=e)
                sections.append(s)
            else:
                NetlistParseError.throw()  # invalid type
        st = start.content
        return Library(name=st.name, sections=sections)

