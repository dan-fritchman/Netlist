def test_version():
    from netlist import __version__

    assert __version__ == "0.1.0"


def test_exprs():
    from netlist import (
        LineParser,
        Int,
        Float,
        MetricNum,
        UnOp,
        BinOp,
        Ident,
        Call,
    )

    def parse_expression(s: str) -> LineParser:
        parser = LineParser(s)
        return parser.parse(parser.parse_expr)

    p = parse_expression("1")
    assert p == Int(1)
    p = parse_expression("1+2")
    assert p == BinOp("PLUS", Int(1), Int(2))
    p = parse_expression("1+2*3")
    assert p == BinOp("PLUS", Int(1), BinOp("STAR", Int(2), Int(3)))
    p = parse_expression("1*2+3")
    assert p == BinOp(tp="PLUS", left=BinOp("STAR", Int(1), Int(2)), right=Int(3))

    p = parse_expression("(1+2)*(3+4)")
    assert p == BinOp(
        tp="STAR",
        left=BinOp(tp="PLUS", left=Int(val=1), right=Int(val=2)),
        right=BinOp(tp="PLUS", left=Int(val=3), right=Int(val=4)),
    )

    p = parse_expression("a     ")
    assert p == Ident("a")
    p = parse_expression("   b + 1     ")
    assert p == BinOp("PLUS", Ident("b"), Int(1))

    p = parse_expression("1e-3")
    assert p == Float(1e-3)
    p = parse_expression("1.0")
    assert p == Float(1.0)
    p = parse_expression("1.")
    assert p == Float(1.0)
    p = parse_expression(".1")
    assert p == Float(0.1)
    p = parse_expression("1e-3 + 2. * .3")
    assert p == BinOp("PLUS", Float(1e-3), BinOp("STAR", Float(2.0), Float(0.3)))

    p = parse_expression("r*l/w")
    assert p == BinOp(
        tp="STAR",
        left=Ident(name="r"),
        right=BinOp(tp="SLASH", left=Ident(name="l"), right=Ident(name="w")),
    )

    p = parse_expression("(0.5f * p)")  # SPICE metric-suffixed number
    assert p == BinOp(tp="STAR", left=MetricNum(val="0.5f"), right=Ident(name="p"))

    p = parse_expression(" ' a + b ' ")  # SPICE-style ticked-expression
    assert p == BinOp(tp="PLUS", left=Ident(name="a"), right=Ident(name="b"))

    p = parse_expression(" a + func(b, c) ")  # Function call
    assert p == BinOp(
        tp="PLUS",
        left=Ident(name="a"),
        right=Call(func=Ident(name="func"), args=[Ident(name="b"), Ident(name="c")]),
    )

    p = parse_expression(" - a ")  # Unary operator
    assert p == UnOp(tp="MINUS", targ=Ident(name="a"))

    p = parse_expression(" - + + - a ")  # Unary operator(s!)
    assert p == UnOp(
        tp="MINUS",
        targ=UnOp(
            tp="PLUS", targ=UnOp(tp="PLUS", targ=UnOp(tp="MINUS", targ=Ident(name="a")))
        ),
    )

    p = parse_expression(" -5 * -3 ")  # Mixture of unary & binary ops
    assert p == BinOp(
        tp="STAR",
        left=UnOp(tp="MINUS", targ=Int(val=5)),
        right=UnOp(tp="MINUS", targ=Int(val=3)),
    )

    p = parse_expression(" 3 ** 4 * 2  ")  # Mixture of unary & binary ops
    assert p == BinOp(
        tp="STAR",
        left=BinOp(tp="DUBSTAR", left=Int(val=3), right=Int(val=4)),
        right=Int(val=2),
    )
    p = parse_expression(" 2 * 3 ** 4 ")  # Mixture of unary & binary ops
    assert p == BinOp(
        tp="STAR",
        left=Int(val=2),
        right=BinOp(tp="DUBSTAR", left=Int(val=3), right=Int(val=4)),
    )


def test_param_values():
    from netlist import Ident, ParamDecl, Float, Expr, BinOp

    p = ParamDecl(Ident("a"), Float(5))
    p = ParamDecl(
        Ident("b"), BinOp("PLUS", Float(1e-3), BinOp("STAR", Float(2.0), Float(0.3)))
    )


def test_instance():
    from netlist import LineParser, NetlistDialects, Dialect
    from netlist import Ident, ParamVal, Int, Instance

    p = LineParser(
        "xxx (d g s b) mymos l=11 w=global_w",
        Dialect.from_enum(NetlistDialects.SPECTRE),
    )
    i = p.parse(p.parse_instance)
    assert i == Instance(
        name=Ident(name="xxx"),
        module=Ident(name="mymos"),
        conns=[Ident(name="d"), Ident(name="g"), Ident(name="s"), Ident(name="b")],
        params=[
            ParamVal(name=Ident(name="l"), val=Int(val=11)),
            ParamVal(name=Ident(name="w"), val=Ident(name="global_w")),
        ],
    )

    txt = """rend  (r1 ra) resistor r=rend *(1 + vc1_raw_end*(1 - exp(-abs(v(r2,r1))))   
        +                            + vc2_raw_end*(1 - exp(-abs(v(r2,r1)))) * (1 - exp(-abs(v(r2,r1))))        )
        +     + vc3_raw_end*(1 - exp(-abs(v(r2,r1)))) * (1 - exp(-abs(v(r2,r1)))) * (1 - exp(-abs(v(r2,r1))))       """ # The question: adding these ((( 
    p = LineParser(txt, Dialect.from_enum(NetlistDialects.SPECTRE))
    i = p.parse(p.parse_instance)


def test_subckt_def():
    from netlist import LineParser, NetlistDialects, Dialect
    from netlist import Ident, ParamDecl, Int, StartSubckt

    p = LineParser(
        "subckt mymos (d g s b) l=11 w=global_w",
        Dialect.from_enum(NetlistDialects.SPECTRE),
    )
    i = p.parse(p.parse_subckt_start)
    assert i == StartSubckt(
        name=Ident(name="mymos"),
        ports=[Ident(name="d"), Ident(name="g"), Ident(name="s"), Ident(name="b")],
        params=[
            ParamDecl(name=Ident(name="l"), default=Int(val=11), distr=None),
            ParamDecl(
                name=Ident(name="w"), default=Ident(name="global_w"), distr=None,
            ),
        ],
    )


def test_model_family():

    txt = """model npd_model bsim3 {
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

    from netlist import LineParser, NetlistDialects, Dialect
    from netlist import Ident, ParamDecl, Int, Float, ModelVariant, ModelFamily

    p = LineParser(txt, Dialect.from_enum(NetlistDialects.SPECTRE),)
    i = p.parse(p.parse_model)
    assert i == ModelFamily(
        name=Ident(name="npd_model"),
        mtype=Ident(name="bsim3"),
        variants=[
            ModelVariant(
                model=Ident(name="npd_model"),
                variant=Ident(name="0"),
                args=[],
                params=[
                    ParamDecl(
                        name=Ident(name="type"), default=Ident(name="n"), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="lmin"), default=Float(val=1.0), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="lmax"), default=Float(val=2.0), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="wmin"), default=Float(val=1.2), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="wmax"), default=Float(val=1.4), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="level"), default=Int(val=999), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="tnom"), default=Int(val=30), distr=None,
                    ),
                ],
            ),
            ModelVariant(
                model=Ident(name="npd_model"),
                variant=Ident(name="1"),
                args=[],
                params=[
                    ParamDecl(
                        name=Ident(name="type"), default=Ident(name="n"), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="version"), default=Float(val=3.2), distr=None,
                    ),
                    ParamDecl(
                        name=Ident(name="xj"), default=Float(val=1.2e-07), distr=None,
                    ),
                    ParamDecl(name=Ident(name="lln"), default=Int(val=1), distr=None,),
                    ParamDecl(name=Ident(name="lwn"), default=Int(val=1), distr=None,),
                ],
            ),
        ],
    )

