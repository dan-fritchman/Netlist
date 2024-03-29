"""
# Netlist Unit Tests 
 
"""

from io import StringIO
from pathlib import Path
from textwrap import dedent

# DUT Imports
import netlist
from netlist import (
    __version__,
    parse_files,
    parse_str,
    netlist as write_netlist,
    ParseOptions,
    Program,
    SourceFile,
    ParamDecl,
    ModelDef,
    ModelVariant,
    ModelFamily,
    BinaryOperator,
    UnaryOperator,
    Options,
    Option,
    ParamVal,
    Ident,
    MetricNum,
    SourceInfo,
    NetlistDialects,
    SpiceDialectParser,
    BinaryOp,
    SpectreDialectParser,
    Int,
    Float,
    MetricNum,
    UnaryOp,
    Call,
    Ref,
    StartProtectedSection,
    EndProtectedSection,
)


def test_version():
    assert __version__ == "0.1.0"


def test_spice_exprs():
    """Test parsing spice-format expressions"""

    def parse_expression(s: str) -> SpiceDialectParser:
        """Parse a string expression, including placing the parser in EXPR mode first,
        particularly so that elements like "*" are interpreted as multiplication."""
        from netlist.dialects.base import ParserState

        parser = SpiceDialectParser.from_str(s)
        parser.state = ParserState.EXPR
        return parser.parse(parser.parse_expr)

    p = parse_expression(" ' a + b ' ")  # SPICE-style ticked-expression
    assert p == BinaryOp(
        tp=BinaryOperator.ADD,
        left=Ref(ident=Ident(name="a")),
        right=Ref(ident=Ident(name="b")),
    )


def test_spectre_exprs():
    def parse_expression(s: str) -> SpectreDialectParser:
        """Parse a string expression"""
        from netlist.dialects.base import ParserState

        parser = SpectreDialectParser.from_str(s)
        parser.state = ParserState.EXPR
        return parser.parse(parser.parse_expr)

    p = parse_expression("1")
    assert p == Int(1)
    p = parse_expression("1+2")
    assert p == BinaryOp(BinaryOperator.ADD, Int(1), Int(2))
    p = parse_expression("1+2*3")
    assert p == BinaryOp(
        BinaryOperator.ADD, Int(1), BinaryOp(BinaryOperator.MUL, Int(2), Int(3))
    )
    p = parse_expression("1*2+3")
    assert p == BinaryOp(
        BinaryOperator.ADD,
        left=BinaryOp(BinaryOperator.MUL, Int(1), Int(2)),
        right=Int(3),
    )

    p = parse_expression("(1+2)*(3+4)")
    assert p == BinaryOp(
        tp=BinaryOperator.MUL,
        left=BinaryOp(tp=BinaryOperator.ADD, left=Int(val=1), right=Int(val=2)),
        right=BinaryOp(tp=BinaryOperator.ADD, left=Int(val=3), right=Int(val=4)),
    )

    p = parse_expression("a     ")
    assert p == Ref(ident=Ident("a"))
    p = parse_expression("   b + 1     ")
    assert p == BinaryOp(BinaryOperator.ADD, Ref(ident=Ident("b")), Int(1))

    p = parse_expression("1e-3")
    assert p == Float(1e-3)
    p = parse_expression("1.0")
    assert p == Float(1.0)
    p = parse_expression("1.")
    assert p == Float(1.0)
    p = parse_expression(".1")
    assert p == Float(0.1)
    p = parse_expression("1e-3 + 2. * .3")
    assert p == BinaryOp(
        BinaryOperator.ADD,
        Float(1e-3),
        BinaryOp(BinaryOperator.MUL, Float(2.0), Float(0.3)),
    )

    p = parse_expression("r*l/w")
    assert p == BinaryOp(
        tp=BinaryOperator.MUL,
        left=Ref(ident=Ident(name="r")),
        right=BinaryOp(
            tp=BinaryOperator.DIV,
            left=Ref(ident=Ident(name="l")),
            right=Ref(ident=Ident(name="w")),
        ),
    )

    p = parse_expression("(0.5f * p)")  # SPICE metric-suffixed number
    assert p == BinaryOp(
        tp=BinaryOperator.MUL,
        left=MetricNum(val="0.5f"),
        right=Ref(ident=Ident(name="p")),
    )

    p = parse_expression(" a + func(b, c) ")  # Function call
    assert p == BinaryOp(
        tp=BinaryOperator.ADD,
        left=Ref(ident=Ident(name="a")),
        right=Call(
            func=Ref(ident=Ident(name="func")),
            args=[Ref(ident=Ident(name="b")), Ref(ident=Ident(name="c"))],
        ),
    )

    p = parse_expression(" - a ")  # Unary operator
    assert p == UnaryOp(tp=UnaryOperator.NEG, targ=Ref(ident=Ident(name="a")))

    p = parse_expression(" - + + - a ")  # Unary operator(s!)
    assert p == UnaryOp(
        tp=UnaryOperator.NEG,
        targ=UnaryOp(
            tp=UnaryOperator.PLUS,
            targ=UnaryOp(
                tp=UnaryOperator.PLUS,
                targ=UnaryOp(tp=UnaryOperator.NEG, targ=Ref(ident=Ident(name="a"))),
            ),
        ),
    )

    p = parse_expression(" -5 * -3 ")  # Mixture of unary & binary ops
    assert p == BinaryOp(
        tp=BinaryOperator.MUL,
        left=UnaryOp(tp=UnaryOperator.NEG, targ=Int(val=5)),
        right=UnaryOp(tp=UnaryOperator.NEG, targ=Int(val=3)),
    )

    p = parse_expression(" 3 ** 4 * 2  ")  # Mixture of unary & binary ops
    assert p == BinaryOp(
        tp=BinaryOperator.MUL,
        left=BinaryOp(tp=BinaryOperator.POW, left=Int(val=3), right=Int(val=4)),
        right=Int(val=2),
    )
    p = parse_expression(" 2 * 3 ** 4 ")  # Mixture of unary & binary ops
    assert p == BinaryOp(
        tp=BinaryOperator.MUL,
        left=Int(val=2),
        right=BinaryOp(tp=BinaryOperator.POW, left=Int(val=3), right=Int(val=4)),
    )


def test_param_values():
    from netlist import Ident, ParamDecl, Float, Expr, BinaryOp

    p = ParamDecl(Ident("a"), Float(5))
    p = ParamDecl(
        Ident("b"),
        BinaryOp(
            BinaryOperator.ADD,
            Float(1e-3),
            BinaryOp(BinaryOperator.MUL, Float(2.0), Float(0.3)),
        ),
    )


def test_primitive():
    from netlist import SpiceDialectParser
    from netlist.data import Ident, BinaryOp, Primitive, Float, Int, ParamVal

    txt = dedent(
        """ r1 1 0
        + fun_param='((0.5*(x-2*y))+z)/(2*(a-2*b))'
        * A mid-stream line comment
        + funner_param=11e-21
        """
    )
    p = SpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_primitive)
    assert i == Primitive(
        name=Ident(name="r1"),
        args=[Int(val=1), Int(val=0)],
        kwargs=[
            ParamVal(
                name=Ident(name="fun_param"),
                val=BinaryOp(
                    tp=BinaryOperator.DIV,
                    left=BinaryOp(
                        tp=BinaryOperator.ADD,
                        left=BinaryOp(
                            tp=BinaryOperator.MUL,
                            left=Float(val=0.5),
                            right=BinaryOp(
                                tp=BinaryOperator.SUB,
                                left=Ref(ident=Ident(name="x")),
                                right=BinaryOp(
                                    tp=BinaryOperator.MUL,
                                    left=Int(val=2),
                                    right=Ref(ident=Ident(name="y")),
                                ),
                            ),
                        ),
                        right=Ref(ident=Ident(name="z")),
                    ),
                    right=BinaryOp(
                        tp=BinaryOperator.MUL,
                        left=Int(val=2),
                        right=BinaryOp(
                            tp=BinaryOperator.SUB,
                            left=Ref(ident=Ident(name="a")),
                            right=BinaryOp(
                                tp=BinaryOperator.MUL,
                                left=Int(val=2),
                                right=Ref(ident=Ident(name="b")),
                            ),
                        ),
                    ),
                ),
            ),
            ParamVal(name=Ident(name="funner_param"), val=Float(val=1.1e-20)),
        ],
    )


def test_instance():
    from netlist import SpectreDialectParser
    from netlist import Ident, ParamVal, Int, Instance, Ref

    p = SpectreDialectParser.from_str(
        "xxx (d g s b) mymos l=11 w=global_w",
    )
    i = p.parse(p.parse_instance)
    assert i == Instance(
        name=Ident(name="xxx"),
        module=Ref(ident=Ident(name="mymos")),
        conns=[Ident(name="d"), Ident(name="g"), Ident(name="s"), Ident(name="b")],
        params=[
            ParamVal(name=Ident(name="l"), val=Int(val=11)),
            ParamVal(name=Ident(name="w"), val=Ref(ident=Ident(name="global_w"))),
        ],
    )

    txt = """rend  (r1 ra) resistor r=rend *(1 + vc1_raw_end*(1 - exp(-abs(v(r2,r1))))
        +                            + vc2_raw_end*(1 - exp(-abs(v(r2,r1)))) * (1 - exp(-abs(v(r2,r1))))        )
        +     + vc3_raw_end*(1 - exp(-abs(v(r2,r1)))) * (1 - exp(-abs(v(r2,r1)))) * (1 - exp(-abs(v(r2,r1))))       """  # The question: adding these (((
    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_instance)


def test_instance_parens():
    """
    Spectre has a fun behavior with dangling close-parens at the end of instance statements -
    it accepts as many as you care to provide.

    So it will accept this is a valid instance:
    ```
    rsad 1 0 resistor r=1  )))))) // really, with all those parentheses
    ```

    The same close-paren behavior does not apply to parameter-declaration statements.
    It may apply to other types.

    You may ask, why should `netlist` inherit what is almost certainly a Spectre bug?
    Because, sadly, notable popular commercial netlists and models include some of these errant parentheses,
    and therefore only work *because* of the Spectre-bug. So, if we want to parse them, we need that bug too.
    """

    txt = "rsad 1 0 resistor r=1  ))))))"
    from netlist import SpectreDialectParser
    from netlist import Ident, ParamVal, Int, Instance, Ref

    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_instance)
    assert i == Instance(
        name=Ident(name="rsad"),
        module=Ref(ident=Ident(name="resistor")),
        conns=[Ident(name="1"), Ident(name="0")],
        params=[ParamVal(name=Ident(name="r"), val=Int(val=1))],
    )


def test_subckt_def():
    from netlist import SpectreDialectParser
    from netlist import Ident, ParamDecl, Int, StartSubckt, Ref

    p = SpectreDialectParser.from_str("subckt mymos (d g s b) l=11 w=global_w")
    i = p.parse(p.parse_subckt_start)
    assert i == StartSubckt(
        name=Ident(name="mymos"),
        ports=[Ident(name="d"), Ident(name="g"), Ident(name="s"), Ident(name="b")],
        params=[
            ParamDecl(name=Ident(name="l"), default=Int(val=11), distr=None),
            ParamDecl(
                name=Ident(name="w"),
                default=Ref(ident=Ident(name="global_w")),
                distr=None,
            ),
        ],
    )


def test_model_family():

    txt = dedent(
        """model npd_model bsim3 {
        0: type=n
        //
        + lmin = 1.0 lmax = 2.0 wmin = 1.2 wmax = 1.4
        + level = 999
        + // some commentary


        // plus some blank lines

        + tnom = 30
        1: type=n
        + version = 3.2
        + xj = 1.2e-7
        + lln = 1
        //
        //  Plus More Commentary
        //
        + lwn = 1
        }
        """
    )

    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_model)
    assert i == ModelFamily(
        name=Ident(name="npd_model"),
        mtype=Ident(name="bsim3"),
        variants=[
            ModelVariant(
                model=Ident(name="npd_model"),
                variant=Ident(name="0"),
                mtype=Ident(name="bsim3"),
                args=[],
                params=[
                    ParamDecl(
                        name=Ident(name="type"),
                        default=Ref(ident=Ident(name="n")),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="lmin"),
                        default=Float(val=1.0),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="lmax"),
                        default=Float(val=2.0),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="wmin"),
                        default=Float(val=1.2),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="wmax"),
                        default=Float(val=1.4),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="level"),
                        default=Int(val=999),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="tnom"),
                        default=Int(val=30),
                        distr=None,
                    ),
                ],
            ),
            ModelVariant(
                model=Ident(name="npd_model"),
                variant=Ident(name="1"),
                mtype=Ident(name="bsim3"),
                args=[],
                params=[
                    ParamDecl(
                        name=Ident(name="type"),
                        default=Ref(
                            ident=Ident(name="n")
                        ),  # FIXME: this is a "ref", but to a kinda behind-the-scenes "thing" `n`
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="version"),
                        default=Float(val=3.2),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="xj"),
                        default=Float(val=1.2e-07),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="lln"),
                        default=Int(val=1),
                        distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="lwn"),
                        default=Int(val=1),
                        distr=None,
                    ),
                ],
            ),
        ],
    )


def test_spectre_midstream_comment():
    """Test for mid-stream full-line comments, which do not break up statements such as `model`
    from being line-continued."""

    txt = dedent(
        """model whatever diode
        + level      =        3
        *
        * This commentary here does not break up the statement. 
        *
        + area       =        1.1e11
        """
    )
    from netlist import SpectreDialectParser

    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_model)

    # Check that parsed to a `ModelDef`
    from netlist.data import ModelDef, Ident, ParamDecl, Int, Float

    assert i == ModelDef(
        name=Ident(name="whatever"),
        mtype=Ident(name="diode"),
        args=[],
        params=[
            ParamDecl(name=Ident(name="level"), default=Int(val=3), distr=None),
            ParamDecl(name=Ident(name="area"), default=Float(val=1.1e11), distr=None),
        ],
    )


def test_parse_capital_param():
    from netlist import SpectreSpiceDialectParser, Ident, ParamDecls, ParamDecl, Int

    txt = ".PARAM a = 3 \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)

    assert i == ParamDecls(
        params=[ParamDecl(name=Ident(name="a"), default=Int(val=3), distr=None)]
    )


def test_spice_include():
    from netlist import SpectreSpiceDialectParser, Include

    txt = '.include "/path/to/file" \n'
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == Include(path=Path("/path/to/file"))


def test_write1():
    """Test writing an empty netlist `Program`"""

    src = Program(files=[SourceFile(path="/", contents=[])])
    write_netlist(src=src, dest=StringIO())


def test_write2():
    """Test writing some actual content"""

    src = Program(
        files=[
            SourceFile(
                path="/",
                contents=[
                    Options(
                        name=None,
                        vals=[
                            Option(
                                name=Ident(name="scale"),
                                val=MetricNum(val="1.0u"),
                            )
                        ],
                        source_info=SourceInfo(
                            line=15, dialect=NetlistDialects.SPECTRE_SPICE
                        ),
                    )
                ],
            )
        ]
    )
    write_netlist(src=src, dest=StringIO())


def test_protection():
    """Test the `protect` / `unprotect` encryption features"""
    from netlist import SpectreDialectParser, SpectreSpiceDialectParser

    txt = ".protect \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == StartProtectedSection()

    txt = ".prot \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == StartProtectedSection()

    txt = ".unprotect \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == EndProtectedSection()

    txt = ".unprot \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == EndProtectedSection()

    txt = "protect \n"
    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == StartProtectedSection()

    txt = "prot \n"
    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == StartProtectedSection()

    txt = "unprotect \n"
    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == EndProtectedSection()

    txt = "unprot \n"
    p = SpectreDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)
    assert i == EndProtectedSection()


def test_names_including_keywords():
    """Test parsing objects whose names include keywords, such as `my_favorite_subckt`."""
    from netlist import SpectreSpiceDialectParser, Ident, ParamDecls, ParamDecl, Int

    txt = ".param my_favorite_model = model_that_works_best \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    i = p.parse(p.parse_statement)

    assert i == ParamDecls(
        params=[
            ParamDecl(
                name=Ident(name="my_favorite_model"),
                default=Ref(ident=Ident(name="model_that_works_best")),
                distr=None,
            )
        ],
    )


def test_model_with_parens():
    from netlist import SpectreSpiceDialectParser

    txt = ".model mymodel mtype arg1 arg2 arg3 (key1=val1 key2=val2) \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    m = p.parse(p.parse_statement)

    golden = ModelDef(
        name=Ident(name="mymodel"),
        mtype=Ident(name="mtype"),
        args=[
            Ident(name="arg1"),
            Ident(name="arg2"),
            Ident(name="arg3"),
        ],
        params=[
            ParamDecl(
                name=Ident(name="key1"),
                default=Ref(ident=Ident(name="val1")),
                distr=None,
            ),
            ParamDecl(
                name=Ident(name="key2"),
                default=Ref(ident=Ident(name="val2")),
                distr=None,
            ),
        ],
    )
    assert m == golden

    # Run the same thing without the parens, check we get the same result
    txt = ".model mymodel mtype arg1 arg2 arg3 key1=val1 key2=val2 \n"
    p = SpectreSpiceDialectParser.from_str(txt)
    m = p.parse(p.parse_statement)
    assert m == golden


def test_spice_function_def():
    """Test parsing a SPICE-syntax function-definition"""
    from netlist import SpectreSpiceDialectParser
    from netlist.data import (
        Ident,
        FunctionDef,
        ArgType,
        TypedArg,
        Return,
        BinaryOp,
        TernOp,
    )

    txt = ".param f1(p1, p2) = 'p1 > p2 ? a : b' \n"

    p = SpectreSpiceDialectParser.from_str(txt)
    m = p.parse(p.parse_statement)

    assert isinstance(m, FunctionDef)
    assert m == FunctionDef(
        name=Ident(name="f1"),
        rtype=ArgType.UNKNOWN,
        args=[
            TypedArg(
                tp=ArgType.UNKNOWN,
                name=Ident(name="p1"),
            ),
            TypedArg(
                tp=ArgType.UNKNOWN,
                name=Ident(name="p2"),
            ),
        ],
        stmts=[
            Return(
                val=BinaryOp(
                    tp=BinaryOperator.GT,
                    left=Ref(ident=Ident(name="p1"), resolved=None),
                    right=TernOp(
                        cond=Ref(ident=Ident(name="p2"), resolved=None),
                        if_true=Ref(ident=Ident(name="a"), resolved=None),
                        if_false=Ref(ident=Ident(name="b"), resolved=None),
                    ),
                ),
            )
        ],
    )


def test_nested_subckt_def():
    """Test parsing nested sub-circuit definitions"""
    from netlist import has_external_refs, get_external_refs, Scope

    txt = dedent(
        """.subckt a 
            .subckt b 
                .subckt c
                .ends
                .subckt d 
                .ends
                xc c * Instance of `c`
                xd d * Instance of `d`
            .ends
        .ends
        """
    )

    scope = netlist.compile(txt)
    assert isinstance(scope, Scope)
    assert get_external_refs(scope) == []
    assert not has_external_refs(scope)


def test_spectre_multiply_starting_continuation():
    """Test the case of a multiply starting a continuation-line
    This can prove confusing to parsing, as the state of whether "*" means "multiply" or "comment"
    changes depending on whether it occurs at the start of a new line."""

    # Spectre format
    txt = dedent(
        """
        r1 ( p n ) resistor r=5
        + * 10
    """
    )
    parse_str(txt, options=ParseOptions(dialect=NetlistDialects.SPECTRE))

    # Spice format
    txt = dedent(
        """
        r1  p n  r=5
        + * 10
    """
    )
    parse_str(txt, options=ParseOptions(dialect=NetlistDialects.SPECTRE_SPICE))
    parse_str(txt, options=ParseOptions(dialect=NetlistDialects.NGSPICE))
