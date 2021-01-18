import os
from pathlib import Path
from warnings import warn
from typing import List

from .dialects import DialectParser
from .data import (
    NetlistDialects,
    NetlistParseError,
    ValidationError,
    Entry,
    DialectChange,
    Unknown,
    SourceInfo,
    SourceFile,
    Program,
    Include,
    UseLib,
)


def default_dialect(path: os.PathLike) -> NetlistDialects:
    """ Infer a default dialect from a file name, particularly its suffix. 
    For files of suffix `scs` this is straightforwardly set to SPECTRE. 
    All other suffixes are less clear without knowing more context. 
    They are set to the most flexible SPECTRE_SPICE, which includes dialect-changes. """

    p = Path(path).absolute()
    if not p.exists() or not p.is_file():
        raise FileNotFoundError(p)
    if p.suffix == "scs":
        return NetlistDialects.SPECTRE
    return NetlistDialects.SPECTRE_SPICE


class FileParser:
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

    def _parse_file(self) -> List[Entry]:
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
                # Error Handling.
                # Can either include `Unknown` statements, or re-raise.
                warn(e)
                s = Unknown("")
                self.dialect_parser.eat_rest_of_statement()
                # NetlistParseError.throw(
                #     f"{str(e)} Error Parsing {self.path} Line {start_line_num}: \n{lines} "
                # )
            if s is not None:  # None-value indicates EOF
                e = Entry(s, si)
                entries.append(e)

        return entries

    def parse(self, path: Path) -> SourceFile:
        """ Parse the netlist `SourceFile` at `path`. """
        with open(path, "r") as f:
            self.path = path
            self.fp = f
            entries = self._parse_file()
        return SourceFile(path, entries)


class Parser:
    """ Multi-File "Netlist Program" Parser """

    def __init__(self, dialect: NetlistDialects):
        self.deps: List[Path] = []
        self.program = Program([])
        self.dialect = dialect
        self.file_parser = FileParser(dialect)

    def parse(self, path: os.PathLike):
        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)

        # Source Found; Start Parsing
        self.deps.append(p)
        f = self.file_parser.parse(p)
        # Add it to our parsed result
        self.program.files.append(f)

        # 2nd pass, parse included-files
        for e in f.contents:
            s = e.content
            if isinstance(s, Include):
                # Differentiate absolute vs relative paths, relative to active source-file
                incp = s.path if s.path.is_absolute() else p.parent / s.path
                incp = incp.resolve()
                if not incp.exists() or not incp.is_file():
                    raise FileNotFoundError(incp)

                from .dialects.spectre import SpectreMixin

                if isinstance(self.dialect, SpectreMixin):
                    # Update our dialect based on the file-extension we load
                    d_enum = default_dialect(path)
                    self.file_parser = FileParser(d_enum)
                else:
                    self.file_parser = FileParser(self.dialect)
                self.parse(incp)
            if isinstance(s, UseLib):
                # Not supported, yet
                NetlistParseError.throw()

    def entries(self):
        """ Iterator of all parsed entries """
        for f in self.program.files:
            for e in f.contents:
                yield e


def parse(path: os.PathLike, *, dialect=None) -> Program:
    """ Parse a Multi-File Netlist-Program """
    d = dialect or default_dialect(path)
    p = Parser(d)
    p.parse(path)
    for e in p.entries():
        if isinstance(e.content, Unknown):
            print(e)
    return p.program

