from ..data import *


class Dialect:

    enum = None
    INSTANCE_RE = None

    @classmethod
    def from_enum(cls, dialect: Optional["NetlistDialects"] = None):
        """ Return a Dialect sub-class based on the `NetlistDialects` enum. 
        Returns the default class if argument `dialect` is not provided or `None`. """
        from .spice import SpiceDialect, NgSpiceDialect
        from .spectre import SpectreDialect, SpectreSpiceDialect

        if dialect is None:
            return SpectreDialect
        if dialect == NetlistDialects.SPECTRE:
            return SpectreDialect
        if dialect == NetlistDialects.SPECTRE_SPICE:
            return SpectreSpiceDialect
        if dialect == NetlistDialects.NGSPICE:
            return NgSpiceDialect
        raise ValueError

    def __init__(self, parent: Optional["Parser"] = None):
        self.parent = parent

    @classmethod
    def is_continuation(cls, txt: str) -> bool:
        # Continuation-characters, comments, and blank-lines all manifest "continuation lines "
        s = txt.lstrip()
        return (
            s.startswith(cls.CONTINUATION_CHAR)
            or cls.is_comment(s)
            or not len(s.strip())
        )

    @classmethod
    def is_comment(cls, s: str) -> bool:
        return s.startswith(cls.COMMENT_CHAR)

    def parse_instance(self, txt: str) -> Optional[Instance]:
        """ Parse a Subckt/Module Instance """
        m = re.match(self.INSTANCE_RE, txt.lstrip(), re.M)
        if m is None:
            return None
        from .. import LineParser

        p = LineParser(txt, self)
        return p.parse(f=p.parse_instance)

    def parse_param_declarations(self, line: str) -> List[ParamDecl]:
        from .. import LineParser

        p = LineParser(line, self)
        return p.parse(f=p.parse_param_declarations)

    def parse_param_values(self, line: str) -> List[ParamVal]:
        from .. import LineParser

        p = LineParser(line, self)
        return p.parse(f=p.parse_param_values)

