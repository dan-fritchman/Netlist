""" 
# Netlist Lexing
"""

# Std-Lib Imports
import re
from typing import Iterable, Optional

# PyPi Imports
from pydantic.dataclasses import dataclass

# Local Imports
from .data import *


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

# Pattern for a string identifier
# An initial alpha character, followed by any number of chars, numbers, and underscores.
# Note some Spice "identifiers" - particularly nets - are often numerically-valued.
# "Node zero" is a prominent example.
# Those are not covered here.
ident_pattern = r"[A-Za-z_][A-Za-z0-9_]*"

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
    BACKSLASH=r"\\",
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
    DUBQUOTE=r"\"",  # Double-quote. Surrounds file-paths, and in some cases, expressions.
    MODEL_VARIANT=rf"{ident_pattern}\.(\d|{ident_pattern})+",  # nmos.0, mymodel.global, etc
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
    INCLUDE=r"(include|INCLUDE)",
    INC=r"(inc|INC)",
    INLINE=r"inline",
    SUBCKT=r"(subckt|SUBCKT)",
    ENDS=r"(ends|ENDS)",
    LIBRARY=r"library",
    LIB=r"(lib|LIB)",
    ENDL=r"(endl|ENDL)",
    MODEL=r"(model|MODEL)",
    STATS=r"statistics",
    SIMULATOR=r"simulator",
    LANG=r"lang",
    PARAMETERS=r"parameters",
    PARAM=r"(param|PARAM)",
    OPTIONS=r"(options|OPTIONS)",
    OPTION=r"(option|OPTION)",
    REAL=r"real",
    RETURN=r"return",
    DEV_GAUSS=r"dev\/gauss",  # Perhaps there are more "dev/{x}" to be added; gauss is the known one for now.
    # Note the order of these `protect` tokens is important, as `prot` is a subset of all of them! It must come last and lowest priority.
    UNPROTECT=r"(unprotect|UNPROTECT)",
    UNPROT=r"(unprot|UNPROT)",
    PROTECT=r"(protect|PROTECT)",
    PROT=r"(prot|PROT)",
)
_patterns2 = dict(IDENT=ident_pattern, ERROR=r"[\s\S]",)
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
    """ Lexer Token 
    Includes type-annotation (as a string), and the token's text value. """

    tp: str  # Type Annotation. A value from `Tokens`, which should be enumerated, some day.
    val: str  # Text Content Value


class Lexer:
    """ # Netlist Lexer """

    def __init__(self, lines: Iterable[str]):
        self.parser = None
        self.lines = lines
        self.line = next(self.lines, None)
        self.line_num = 1
        self.lexed_nonwhite_on_this_line = False
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
                self.lexed_nonwhite_on_this_line = False

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

            self.lexed_nonwhite_on_this_line = True
            yield token
            token = self.nxt()
