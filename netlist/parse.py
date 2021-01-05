from .data import *
from .dialects import *


class Parser:
    def __init__(self, dialect: Optional[NetlistDialects] = None):
        self.dialect = Dialect.from_enum(dialect)(self)
        self.deps: List[Path] = []
        self.entries: List[Entry] = []
        self.line = self.nxt = "*"  # Initialize our two lines worth of input strings
        self.line_num = 1
        self.program = Program([])

    @classmethod
    def default_dialect(cls, path: os.PathLike) -> "NetlistDialects":
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

    def notify(self, reason: DialectChange):
        """ Notification from dialect-parser that something's up. 
        E.g. for dialect changes via `simulator lang` statements. """

        s = reason.dialect.lower().strip()
        if s == "spectre":
            dialect = NetlistDialects.SPECTRE
        elif s == "spice":
            dialect = NetlistDialects.SPECTRE_SPICE
        else:
            raise NetlistParseError
        self.dialect = Dialect.from_enum(dialect)(self)

    def peek(self) -> str:
        return self.nxt

    def advance(self) -> str:
        self.line = self.nxt
        self.nxt = self.fp.readline()
        self.line_num += 1
        return self.line

    def _parse_file(self):
        """ Parse the (open) file-pointer at `self.fp` """
        entries = []
        self.line_num = start_line_num = 1
        self.advance()

        while self.line:
            # Iterate over multi-line statements
            line = self.line[:]  # Copies

            while self.dialect.is_continuation(self.nxt):
                if not self.dialect.is_comment(self.nxt):
                    line += self.nxt.lstrip()[1:]
                if not self.nxt:
                    break  # Break on end-of-file
                self.advance()

            if line.strip():  # Filter out empties
                try:  # Collected a statement, parse it
                    s = self.dialect.parse_stmt(line.lstrip())
                    e = Entry(
                        content=s,
                        source_info=SourceInfo(
                            path=self.path,
                            line=start_line_num,
                            dialect=self.dialect.enum,
                        ),
                    )
                    entries.append(e)
                except (NetlistParseError, ValidationError) as e:
                    NetlistParseError.throw(
                        f"{str(e)} Error Parsing {self.path} Line {start_line_num}: \n{line} "
                    )

            # Move to the next statement
            start_line_num = self.line_num
            self.advance()

        # Append everything to our running total
        self.entries.extend(entries)
        return entries

    def parse(self, path: os.PathLike):
        # Check for validity of source file
        p = Path(path).absolute()
        if not p.exists() or not p.is_file():
            raise FileNotFoundError(p)
        # Source Found; Start Parsing
        self.deps.append(p)
        with open(p, "r") as f:
            self.path = p
            self.fp = f
            entries = self._parse_file()

        # Add it to our parsed result
        self.program.files.append(SourceFile(p, entries))

        # 2nd pass, parse include-files
        for e in entries:
            s = e.content
            if isinstance(s, Include):
                # Differentiate absolute vs relative paths, relative to active source-file
                incp = s.path if s.path.is_absolute() else p.parent / s.path
                if not incp.exists() or not incp.is_file():
                    raise FileNotFoundError(incp)

                from .dialects.spectre import SpectreMixin

                if isinstance(self.dialect, SpectreMixin):
                    # Update our dialect based on the file-extension we load
                    d_enum = Parser.default_dialect(path)
                    self.dialect = Dialect.from_enum(d_enum)(self)
                self.parse(incp)
            if isinstance(s, UseLib):
                # Not supported, yet
                NetlistParseError.throw()


def parse(path: os.PathLike, *, dialect=None) -> Program:
    """ Parse a Multi-File Netlist-Program """
    if dialect is None:
        dialect = Parser.default_dialect(path)
    p = Parser(dialect)
    p.parse(path)
    return p.program


# Numeric-value suffixes
suffixes = dict(
    T=1.0e12,
    G=1.0e9,
    MEG=1.0e6,
    X=1.0e6,
    K=1.0e3,
    M=1.0e-3,
    MIL=2.54e-5,  # (1/1000 inch)
    U=1.0e-6,
    N=1.0e-9,
    P=1.0e-12,
    F=1.0e-15,
    A=1.0e-18,
)
# Include both upper and lower-case versions.
# (Python `re` complains about inserting this as an inline flag.)
suffix_pattern = "|".join(list(suffixes.keys()) + [k.lower() for k in suffixes.keys()])

# Master mapping of tokens <=> patterns
tokens = dict(
    LPAREN=r"\(",
    RPAREN=r"\)",
    LBRACKET=r"\{",
    RBRACKET=r"\}",
    WHITE=r"\s",
    NEWLINE=r"\n",
    PLUS=r"\+",
    MINUS=r"\-",
    SLASH=r"\/",
    CARET=r"\^",
    DUBSTAR=r"\*\*",
    STAR=r"\*",
    TICK=r"\'",
    COMMA=r"\,",
    EQUALS=r"\=",
    DOLLAR=r"\$",
    METRIC_NUM=rf"(\d+(\.\d+)?|\.\d+)({suffix_pattern})",  # 1M or 1.0f or .1k
    FLOAT=r"(\d+[eE][+-]?\d+|(\d+\.\d*|\.\d+)([eE][+-]?\d+)?)",  # 1e3 or 1.0 or .1 (optional e-3)
    INT=r"\d+",
    INLINE=r"inline",
    SUBCKT=r"subckt",
    DEV_GAUSS=r"dev\/gauss",  # Perhaps there are more "dev/{x}" to be added; gauss is the known one for now.
    IDENT=r"[A-Za-z_][A-Za-z0-9_]*",
)
# Given each token its name as a key in the overall regex
tokens = {key: rf"(?P<{key}>{val})" for key, val in tokens.items()}
# Build our overall regex pattern, a union of all
pat = re.compile("|".join(tokens.values()))
# Create an enum-ish class of these token-types
Tokens = type("Tokens", (object,), {k: k for k in tokens.keys()})


@dataclass
class Token:
    tp: str
    val: Any


class Lexer:
    def __init__(self, txt: str):
        self.txt = txt
        self.parser = None

    def lex(self):
        """ Create an iterator over pattern-matches """
        sc = pat.scanner(self.txt)
        for m in iter(sc.match, None):  # Iterate over token-matches
            token = Token(m.lastgroup, m.group())
            if token.tp != Tokens.WHITE:  # Filter out whitespace
                yield token


class LineParser:
    def __init__(self, s: str, dialect: Optional[SpiceDialect] = None):
        self.dialect = dialect or Dialect.from_enum(NetlistDialects.SPECTRE_SPICE)
        self.root = None
        # Initialize our state
        self.cur = None
        self.nxt = None  # It is (was) LL(1)
        self.nxt1 = None  # OK, it's LL(2)
        # Initialize our lexer and its token-generator
        self.lex = Lexer(s)
        self.lex.parser = self
        self.tokens = self.lex.lex()

    def start(self) -> None:
        # Queue up our lookahead tokens
        self.advance()
        self.advance()

    def advance(self) -> None:
        self.cur = self.nxt
        self.nxt = self.nxt1
        self.nxt1 = next(self.tokens, None)

    def match(self, tp: str) -> bool:
        """ Boolean indication of whether our next token matches `tp` """
        if self.nxt and tp == self.nxt.tp:
            self.advance()
            return True
        return False

    def match_any(self, *tp: List[str]) -> bool:
        """ Boolean indication of whether our next token matche *any* provided `tp` """
        if self.nxt and any([self.nxt.tp == t for t in tp]):
            self.advance()
            return True
        return False

    def expect(self, *tp: List[str]) -> None:
        """ Assertion that our next token matches `tp`. 
        Note this advances if successful, effectively discarding `self.cur`. """
        if not self.match_any(*tp):
            NetlistParseError.throw()

    def parse(self, f=None) -> Any:
        """ Perform parsing. Succeeds if top-level is parsable by function `f`.
        Defaults to parsing `Expr`. """
        self.start()
        func = f if f else self.parse_expr
        self.root = func()
        if self.nxt is not None:  # Check whether there's more stuff
            NetlistParseError.throw()
        return self.root

    def parse_subckt_start(self) -> StartSubckt:
        """ module_name ( port1 port2 port2 ) p1=param1 p2=param2 ... 
        FIXME: spectre-only for now! 
        Note predecessor keywords "suckt" and the optional "inline" 
        have been removed by calling-time """

        # Boolean indication of the `inline`-ness
        _inline = self.match(Tokens.INLINE)
        self.expect(Tokens.SUBCKT)
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.LPAREN)

        # Parse port-names
        ports = []
        MAX_PORTS = 10_000  # "Time-out"
        for i in range(MAX_PORTS, -1, -1):
            if not self.nxt or self.match(Tokens.RPAREN):
                break
            self.expect(Tokens.IDENT, Tokens.INT)
            pname = Ident(
                str(self.cur.val)
            )  # Note integer-valued node-names are stored as Ident(str)
            ports.append(pname)
        if i <= 0:  # Check whether the time-out triggered
            NetlistParseError.throw()

        # Parse parameters
        params = self.parse_param_declarations()
        # And create & return our instance
        return StartSubckt(name=name, ports=ports, params=params)

    def parse_instance(self) -> Instance:
        """ iname (? port1 port2 port2 )? mname p1=param1 p2=param2 ... """
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        conns = []

        # Parse the parens-optional port-list
        if self.match(Tokens.LPAREN):
            MAX_PORTS = 10_000  # "Time-out"
            for i in range(MAX_PORTS, -1, -1):
                if not self.nxt or self.match(Tokens.RPAREN):
                    break
                self.expect(Tokens.IDENT, Tokens.INT)
                pname = Ident(
                    str(self.cur.val)
                )  # Note integer-valued node-names are stored as Ident(str)
                conns.append(pname)
            if i <= 0:  # Check whether the time-out triggered
                NetlistParseError.throw()
            # Grab the module name
            self.expect(Tokens.IDENT)
            module = Ident(self.cur.val)

        else:  # No-parens case
            MAX_PORTS = 10_000  # "Time-out"
            for i in range(MAX_PORTS, -1, -1):
                if not self.nxt or not self.nxt1 or self.nxt1.tp == Tokens.EQUALS:
                    break
                self.expect(Tokens.IDENT, Tokens.INT)
                name = Ident(
                    str(self.cur.val)
                )  # Note integer-valued node-names are stored as Ident(str)
                conns.append(name)
            if i <= 0:  # Check whether the time-out triggered
                NetlistParseError.throw()
            # Grab the module name, at this point errantly in the `conns` list
            module = conns.pop()  # FIXME: check this matched `Ident` and not `Int`

        # Parse parameters
        params = self.parse_param_values()
        # And create & return our instance
        return Instance(name=name, module=module, conns=conns, params=params)

    def parse_param_declarations(self) -> List[ParamDecl]:
        """ ( ident = expr ( dev/gauss = expr )? ( $ units/commentary )? )* """
        rv = []
        # Set a (fairly artificial) "time-out" so we don't get stuck here
        MAX_ARGS = 1000
        for i in range(MAX_ARGS, -1, -1):
            if self.nxt is None:
                break

            self.expect(Tokens.IDENT)
            name = Ident(self.cur.val)
            self.expect(Tokens.EQUALS)
            e = self.parse_expr()

            if self.match_any(Tokens.DEV_GAUSS, Tokens.DOLLAR):
                # FIXME: Skipping this auxiliary stuff for now
                while self.nxt and not self.match(Tokens.NEWLINE):
                    self.advance()
            rv.append(ParamDecl(name, e))

        if i <= 0:  # Check whether the time-out triggered
            NetlistParseError.throw()
        return rv

    def parse_param_values(self) -> List[ParamVal]:
        """ ( ident = expr )* """
        rv = []
        # Set a (fairly artificial) "time-out" so we don't get stuck here
        MAX_ARGS = 10_000
        for i in range(MAX_ARGS, -1, -1):
            if self.nxt is None or self.match(Tokens.NEWLINE):
                break

            self.expect(Tokens.IDENT)
            name = Ident(self.cur.val)
            self.expect(Tokens.EQUALS)
            e = self.parse_expr()
            rv.append(ParamVal(name, e))

        if i <= 0:  # Check whether the time-out triggered
            NetlistParseError.throw()
        return rv

    def parse_expr(self) -> Expr:
        """ expr0 | 'expr0' | {expr0} """
        # FIXME: the ticks vs brackets syntax will become a Dialect-specific thing
        if self.match(Tokens.TICK):
            e = self.parse_expr0()
            self.expect(Tokens.TICK)
            return e
        if self.match(Tokens.LBRACKET):
            e = self.parse_expr0()
            self.expect(Tokens.RBRACKET)
            return e
        return self.parse_expr0()

    def parse_expr0(self) -> Expr:
        """ expr1 ( (+|-) expr0 )? """
        e = self.parse_expr1()
        if self.match_any(Tokens.PLUS, Tokens.MINUS):
            return BinOp(tp=self.cur.tp, left=e, right=self.parse_expr0())
        return e

    def parse_expr1(self) -> Expr:
        """ expr2 ( (*|/) expr1 )? """
        e = self.parse_expr2()
        if self.match_any(Tokens.STAR, Tokens.SLASH):
            return BinOp(tp=self.cur.tp, left=e, right=self.parse_expr1())
        return e

    def parse_expr2(self) -> Expr:
        """ expr3 ( (**|^) expr2 )? """
        e = self.parse_expr3()
        if self.match_any(Tokens.DUBSTAR, Tokens.CARET):
            return BinOp(tp=self.cur.tp, left=e, right=self.parse_expr2())
        return e

    def parse_expr3(self) -> Expr:
        """ ( expr ) or term """
        if self.match(Tokens.LPAREN):
            e = self.parse_expr()
            self.expect(Tokens.RPAREN)
            return e
        return self.parse_term()

    def parse_term(self) -> Union[Int, Float, Ident]:
        """ Parse a terminal value, or raise an Exception 
        ( number | ident | unary(term) | call ) """
        if self.match(Tokens.METRIC_NUM):
            return MetricNum(self.cur.val)
        if self.match(Tokens.FLOAT):
            return Float(float(self.cur.val))
        if self.match(Tokens.INT):
            return Int(int(self.cur.val))
        if self.match_any(Tokens.PLUS, Tokens.MINUS):
            return UnOp(tp=self.cur.tp, targ=self.parse_term())
        if self.match(Tokens.IDENT):
            name = Ident(self.cur.val)
            if self.match(Tokens.LPAREN):  # Function-call syntax
                # Parse arguments
                args = []
                MAX_ARGS = 100  # Set a "time-out" so that we don't get stuck here.
                for i in range(MAX_ARGS, -1, -1):
                    if self.match(Tokens.RPAREN):
                        break
                    a = self.parse_expr0()  # Grab an argument-expression
                    args.append(a)
                    if self.match(Tokens.RPAREN):
                        break
                    self.expect(Tokens.COMMA)
                if i <= 0:  # Check the time-out
                    NetlistParseError.throw()
                return Call(func=name, args=args)
            return name
        NetlistParseError.throw()

