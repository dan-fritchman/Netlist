from ..data import *
from typing import Iterable


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
_patterns1 = dict(
    DUBSLASH=r"\/\/",
    DUBSTAR=r"\*\*",
    LPAREN=r"\(",
    RPAREN=r"\)",
    LBRACKET=r"\{",
    RBRACKET=r"\}",
    NEWLINE=r"\n",
    WHITE=r"\s",
    SLASH=r"\/",
    CARET=r"\^",
    STAR=r"\*",
    TICK=r"\'",
    COMMA=r"\,",
    SEMICOLON=r"\;",
    COLON=r"\:",
    GE=r"\>\=",
    LE=r"\<\=",
    GT=r"\>",
    LT=r"\<",
    EQUALS=r"\=",
    DOLLAR=r"\$",
    QUESTION=r"\?",
    QUOTESTR=r"\".*\"",
    MODEL_VARIANT=r"[A-Za-z_][A-Za-z0-9_]*\.\d+", # nmos.0, mymodel.3, etc
    METRIC_NUM=rf"(\d+(\.\d+)?|\.\d+)({suffix_pattern})",  # 1M or 1.0f or .1k
    FLOAT=r"(\d+[eE][+-]?\d+|(\d+\.\d*|\.\d+)([eE][+-]?\d+)?)",  # 1e3 or 1.0 or .1 (optional e-3)
    INT=r"\d+",
    PLUS=r"\+",
    MINUS=r"\-",
    DOT=r"\.",
)
_keywords = dict(
    ENDSECTION=r"endsection",
    SECTION=r"section",
    AHDL=r"ahdl_include",
    INCLUDE=r"include",
    INC=r"inc",
    INLINE=r"inline",
    SUBCKT=r"subckt",
    ENDS=r"ends",
    LIBRARY=r"library",
    LIB=r"lib",
    ENDL=r"endl",
    MODEL=r"model",
    STATS=r"statistics",
    SIMULATOR=r"simulator",
    LANG=r"lang",
    PARAMETERS=r"parameters",
    PARAM=r"param",
    OPTIONS=r"options",
    OPTION=r"option",
    REAL=r"real",
    RETURN=r"return",
    DEV_GAUSS=r"dev\/gauss",  # Perhaps there are more "dev/{x}" to be added; gauss is the known one for now.
)
_patterns2 = dict(
    IDENT=r"[A-Za-z_][A-Za-z0-9_]*",
    ERROR=r"[\s\S]",
)
# Given each token its name as a key in the overall regex
tokens = {key: rf"(?P<{key}>{val})" for key, val in _patterns1.items()}
for key, val in _keywords.items():
    # Insert \b word-boundaries around keywords 
    tokens[key] = rf"(?P<{key}>\b{val}\b)"
for key, val in _patterns2.items():
    # Add the lower-priority patterns last 
    tokens[key] = rf"(?P<{key}>{val})"
# Build our overall regex pattern, a union of all
pat = re.compile("|".join(tokens.values()))
# Create an enum-ish class of these token-types
Tokens = type("Tokens", (object,), {k: k for k in tokens.keys()})


@dataclass
class Token:
    tp: str
    val: Any


class Lexer:
    def __init__(self, lines: Iterable[str]):
        self.parser = None
        self.lines = lines
        self.line = next(self.lines, None)
        self.line_num = 1
        self.toks = iter(pat.scanner(self.line).match, None)

    def nxt(self) -> Optional[Token]:
        """ Get our next Token, pulling a new line if necessary. """
        m = next(self.toks, None)
        if m is None:  # Grab a new line
            self.line = next(self.lines, None)
            if self.line is None:  # End of input
                return None
            self.line_num += 1
            self.toks = iter(pat.scanner(self.line).match, None)
            m = next(self.toks, None)
            if m is None:
                return None
        return Token(m.lastgroup, m.group())

    def eat_idle(self, token) -> Optional[Token]:
        """ Consume whitespace and comments, returning the next (potential) action-token. 
        Does not handle line-continuations. """
        while token and token.tp == Tokens.WHITE:
            token = self.nxt()
        if token and self.parser.is_comment(token):
            while token and token.tp != Tokens.NEWLINE:
                token = self.nxt()
        return token

    def lex(self):
        """ Create an iterator over pattern-matches """
        token = self.nxt()
        while token:  # Iterate over token-matches

            # Skip whitespace & comments
            token = self.eat_idle(token)

            # Handle continuation-lines
            if token and token.tp == Tokens.NEWLINE:

                # Loop until a non-blank-line, non-comment, non-continuation token
                token = self.eat_idle(self.nxt())
                while token and token.tp in (Tokens.NEWLINE, Tokens.WHITE,):
                    token = self.eat_idle(self.nxt())

                if token and token.tp == Tokens.PLUS:
                    # Cancelled newline; skip to next token
                    token = self.nxt()
                else:
                    # Non-cancelled newline; yield the (already-passed) NEWLINE
                    # Next loop-pass will get the first token on the next line
                    yield Token(Tokens.NEWLINE, "\n")
                continue  # Either way, restart this loop body

            yield token
            token = self.nxt()


class ParserState(Enum):
    # States of the parser, as they need be understood by the lexer
    PROGRAM = 0  # Typical program content
    EXPR = 1  # High-priority expressions


class DialectParser:
    """ Netlist Dialect-Parsing Base-Class """

    enum = None

    def __init__(self, lex: Lexer, parent: Optional["FileParser"] = None):
        self.parent = parent
        # Initialize our state
        self.state = ParserState.PROGRAM
        self.tokens = None
        self.prev = None
        self.cur = None
        self.nxt = None
        self.nxt0 = None
        self.rewinding = False
        # Initialize our lexer and its token-generator
        self.lex = lex
        self.lex.parser = self

    @property
    def line_num(self):
        return self.lex.line_num

    @classmethod
    def from_parser(cls, p: "DialectParser") -> "DialectParser":
        """ Create from another DialectParser, 
        as during a `simulator lang` DialectChange.  
        Includes copying its private internal state. """
        rv = cls(lex=p.lex, parent=p.parent)
        rv.state = p.state
        rv.tokens = p.tokens
        rv.cur = p.cur
        rv.nxt = p.nxt
        rv.nxt0 = p.nxt0
        rv.rewinding = p.rewinding
        return rv

    @classmethod
    def from_lines(cls, lines: Iterable[str], **kwargs) -> "DialectParser":
        """ Create from a line iterator """
        return cls(lex=Lexer(lines), **kwargs)

    @classmethod
    def from_str(cls, txt: str, **kwargs) -> "DialectParser":
        """ Create from a multi-line input string """
        ls = [line + "\n" for line in txt.split("\n")[:-1]]
        ls += [txt.split("\n")[-1]]
        return cls.from_lines(lines=iter(ls), **kwargs)

    @classmethod
    def from_enum(cls, dialect: Optional["NetlistDialects"] = None):
        """ Return a Dialect sub-class based on the `NetlistDialects` enum. 
        Returns the default class if argument `dialect` is not provided or `None`. """
        from .spice import SpiceDialectParser, NgSpiceDialectParser
        from .spectre import SpectreDialectParser, SpectreSpiceDialectParser

        if dialect is None:
            return SpectreDialectParser
        if dialect == NetlistDialects.SPECTRE:
            return SpectreDialectParser
        if dialect == NetlistDialects.SPECTRE_SPICE:
            return SpectreSpiceDialectParser
        if dialect == NetlistDialects.NGSPICE:
            return NgSpiceDialectParser
        raise ValueError

    def eat_blanks(self):
        """ Pass over blank-lines, generally created by full-line comments. """
        while self.nxt and self.nxt.tp == Tokens.NEWLINE:
            self.advance()

    def eat_rest_of_statement(self):
        """ Ignore Tokens from `self.cur` to the end of the current statement. 
        Largely used for error purposes. """
        while self.nxt and not self.match(Tokens.NEWLINE):
            self.advance()

    def start(self) -> None:
        # Initialize our token generator
        self.tokens = self.lex.lex()
        # And queue up our lookahead tokens
        self.advance()

    def advance(self) -> None:
        self.prev = self.cur
        self.cur = self.nxt
        if self.rewinding:
            self.rewinding = False
            self.nxt = self.nxt0
        else:
            self.nxt = next(self.tokens, None)
        self.nxt0 = None

    def rewind(self):
        """ Rewind by one Token. Error if already in rewinding state. """
        if self.rewinding:
            NetlistParseError.throw()
        self.rewinding = True
        self.nxt0 = self.nxt
        self.nxt = self.cur
        self.cur = self.prev
        self.prev = None

    def peek(self) -> Optional[Token]:
        """ Peek at the next two Tokens """
        return self.nxt

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
        """ module_name ( port1 port2 port2 ) p1=param1 p2=param2 ... """

        # Boolean indication of the `inline`-ness
        _inline = self.match(Tokens.INLINE)
        self.expect(Tokens.SUBCKT)

        # Grab the module/subckt name
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)

        # Parse the parens-optional port-list
        if self.match(Tokens.LPAREN):  # Parens case
            term = lambda s: not s.nxt or s.nxt.tp in (Tokens.RPAREN, Tokens.NEWLINE)
            ports = self.parse_node_list(term)
            self.expect(Tokens.RPAREN)

        else:  # No-parens case
            ports = self.parse_node_list(_endargs_startkwargs)
            # If we landed on a key-value param key, rewind it
            if self.nxt and self.nxt.tp == Tokens.EQUALS:
                self.rewind()
                ports.pop()

        # Parse parameters
        params = self.parse_param_declarations()

        # And create & return our instance
        return StartSubckt(name=name, ports=ports, params=params)

    def parse_ident(self) -> Ident:
        """ Parse an Identifier """
        self.expect(Tokens.IDENT)
        return Ident(self.cur.val)

    def parse_node_ident(self) -> Ident:
        """ Parse a Node-Identifier - either a var-name-style Ident or an Int. """
        self.expect(Tokens.IDENT, Tokens.INT)
        return Ident(str(self.cur.val))

    def parse_list(self, parse_item, term, *, MAXN=10_000) -> List[Any]:
        """ Parse a whitespace-separated list of entries possible by function `parse_item`. 
        Terminated in the condition `term(self)`. """
        rv = []
        for i in range(MAXN, -1, -1):
            if term(self):
                break
            rv.append(parse_item())
        if i <= 0:  # Check whether the time-out triggered
            NetlistParseError.throw()
        return rv

    def parse_node_list(self, term, *, MAXN=10_000) -> List[Ident]:
        """ Parse a Node-Identifier (Ident or Int) list, 
        terminated in the condition `term(self)`. """
        return self.parse_list(self.parse_node_ident, term=term, MAXN=MAXN)

    def parse_ident_list(self, term, *, MAXN=10_000) -> List[Ident]:
        """ Parse list of Identifiers """
        return self.parse_list(self.parse_ident, term=term, MAXN=MAXN)

    def parse_expr_list(self, term, *, MAXN=10_000) -> List[Expr]:
        """ Parse list of Expressions """
        return self.parse_list(self.parse_expr, term=term, MAXN=MAXN)

    def parse_primitive(self) -> Primitive:
        """ Parse a Spice-format primitive instance """
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        args = []

        # For Primitives it is not necessarily clear at parse-time which arguments
        # are ports vs parameters. All are parsed as `Expr` and sorted out later.
        if self.match(Tokens.LPAREN):

            # Parse ports in parentheses
            # In this case, we actually do know what's a port vs param.
            # But both are still stored in `args`, for consistency with the alternate case.
            term = lambda s: not s.nxt or s.nxt.tp in (Tokens.RPAREN, Tokens.NEWLINE)
            args = self.parse_expr_list(term)
            self.expect(Tokens.RPAREN)

            # Now parse the positional parameters
            args = self.parse_expr_list(_endargs_startkwargs)

        else:  # No-parens case
            args = self.parse_expr_list(_endargs_startkwargs)
            # If we landed on a key-value param key, rewind it
            if self.nxt and self.nxt.tp == Tokens.EQUALS:
                self.rewind()
                args.pop()

        # Parse parameters
        params = self.parse_param_values()
        # And create & return our instance
        return Primitive(name=name, args=args, kwargs=params)

    def parse_instance(self) -> Instance:
        """ iname (? port1 port2 port2 )? mname p1=param1 p2=param2 ... """
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        conns = []

        # Parse the parens-optional port-list
        if self.match(Tokens.LPAREN):  # Parens case
            term = lambda s: not s.nxt or s.nxt.tp in (Tokens.RPAREN, Tokens.NEWLINE)
            conns = self.parse_node_list(term)
            self.expect(Tokens.RPAREN)

            # Grab the module name
            self.expect(Tokens.IDENT)
            module = Ident(self.cur.val)

        else:  # No-parens case
            conns = self.parse_node_list(_endargs_startkwargs)
            # If we landed on a key-value param key, rewind it
            if self.nxt and self.nxt.tp == Tokens.EQUALS:
                self.rewind()
                if not len(conns): # Something went wrong!
                    NetlistParseError.throw()
                conns.pop()
            # Grab the module name, at this point errantly in the `conns` list
            module = conns.pop()  # FIXME: check this matched `Ident` and not `Int`

        # Parse parameters
        params = self.parse_instance_param_values()
        # And create & return our instance
        return Instance(name=name, module=module, conns=conns, params=params)

    def parse_instance_param_values(self) -> List[ParamVal]:
        # Base class parses instance-params as any other params
        return self.parse_param_values()

    def parse_param_val(self) -> ParamVal:
        self.expect(Tokens.IDENT)
        name = Ident(self.cur.val)
        self.expect(Tokens.EQUALS)
        e = self.parse_expr()
        return ParamVal(name, e)

    def parse_param_declaration(self):
        val = self.parse_param_val()
        # FIXME: Skipping this auxiliary stuff for now
        if self.match(Tokens.DEV_GAUSS):
            self.expect(Tokens.EQUALS)
            _e = self.parse_expr()
        return ParamDecl(val.name, val.val)

    def parse_param_declarations(self) -> List[ParamDecl]:
        """ Parse a set of parameter declarations """
        term = lambda s: s.nxt is None or s.match(Tokens.NEWLINE)
        return self.parse_list(self.parse_param_declaration, term=term)

    def parse_param_values(self) -> List[ParamVal]:
        """ ( ident = expr )* """
        term = lambda s: s.nxt is None or s.match(Tokens.NEWLINE)
        return self.parse_list(self.parse_param_val, term=term)

    def parse_end_sub(self):
        self.expect(Tokens.ENDS)
        if self.match(Tokens.IDENT):
            name = Ident(self.cur.val)
        else:
            name = None
        self.expect(Tokens.NEWLINE)
        return EndSubckt(name)

    def parse_expr0(self) -> Expr:
        """ expr0b ( (<|>|<=|>=) expr0b )? """
        e = self.parse_expr0b()
        if self.match_any(Tokens.GT, Tokens.LT, Tokens.GE, Tokens.LE):
            tp = self.cur.tp
            right = self.parse_expr0b()
            return BinOp(tp=tp, left=e, right=right)
        return e

    def parse_expr0b(self) -> Expr:
        """ expr1 ( (+|-) expr0 )? """
        e = self.parse_expr1()
        if self.match_any(Tokens.PLUS, Tokens.MINUS):
            tp = self.cur.tp
            right = self.parse_expr0b()
            return BinOp(tp=tp, left=e, right=right)
        return e

    def parse_expr1(self) -> Expr:
        """ expr2 ( (*|/) expr1 )? """
        e = self.parse_expr2()
        if self.match_any(Tokens.STAR, Tokens.SLASH):
            tp = self.cur.tp
            right = self.parse_expr1()
            return BinOp(tp=tp, left=e, right=right)
        return e

    def parse_expr2(self) -> Expr:
        """ expr3 ( (**|^) expr2 )? """
        e = self.parse_expr2b()
        if self.match_any(Tokens.DUBSTAR, Tokens.CARET):
            return BinOp(tp=self.cur.tp, left=e, right=self.parse_expr2())
        return e

    def parse_expr2b(self) -> Expr:
        """ expr3 ( ? expr0 : expr0 )? """
        e = self.parse_expr3()
        if self.match(Tokens.QUESTION):
            if_true = self.parse_expr0()
            self.expect(Tokens.COLON)
            if_false = self.parse_expr0()
            return TernOp(e, if_true, if_false)
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

    def is_comment(self, tok: Token) -> bool:
        """ Boolean indication of whether `tok` begins a Comment """
        return tok.tp in (Tokens.DUBSLASH, Tokens.DOLLAR,) or (
            self.are_stars_comments_now() and tok.tp in (Tokens.DUBSTAR, Tokens.STAR)
        )

    """ Abstract Methods """

    def are_stars_comments_now(self) -> bool:
        """ Boolean indication of whether Tokens.STAR and DUBSTAR should 
        currently be lexed as a comment. """
        raise NotImplementedError

    def parse_statement(self) -> Optional[Statement]:
        """ Statement Parser 
        Dispatches to type-specific parsers based on prioritized set of matching rules. 
        Returns `None` at end. """
        raise NotImplementedError

    def parse_expr(self) -> Expr:
        """ Parse an Expression """
        raise NotImplementedError

    def parse_model(self) -> Expr:
        """ Parse a Model Declaration """
        raise NotImplementedError


def _endargs_startkwargs(s):
    """ A fairly intractible test of where argument-lists end and key-valued keyword args being. 
    e.g. 
    a b c d=1 e=2 ... => d
    a b c \n  => \n
    a b c EOF => EOF
    """
    return s.nxt is None or s.nxt.tp == Tokens.NEWLINE or s.nxt.tp == Tokens.EQUALS

