def test_version():
    from netlist import __version__

    assert __version__ == "0.1.0"


def test_exprs():
    from netlist import parse_expression, Int, Float, MetricNum, BinOp, Ident, Call

    p = parse_expression("1")
    assert p.root == Int(1)
    p = parse_expression("1+2")
    assert p.root == BinOp("PLUS", Int(1), Int(2))
    p = parse_expression("1+2*3")
    assert p.root == BinOp("PLUS", Int(1), BinOp("STAR", Int(2), Int(3)))
    p = parse_expression("1*2+3")
    assert p.root == BinOp(tp="PLUS", left=BinOp("STAR", Int(1), Int(2)), right=Int(3))

    p = parse_expression("(1+2)*(3+4)")
    assert p.root == BinOp(
        tp="STAR",
        left=BinOp(tp="PLUS", left=Int(val=1), right=Int(val=2)),
        right=BinOp(tp="PLUS", left=Int(val=3), right=Int(val=4)),
    )

    p = parse_expression("a     ")
    assert p.root == Ident("a")
    p = parse_expression("   b + 1     ")
    assert p.root == BinOp("PLUS", Ident("b"), Int(1))

    p = parse_expression("1e-3")
    assert p.root == Float(1e-3)
    p = parse_expression("1.0")
    assert p.root == Float(1.0)
    p = parse_expression("1.")
    assert p.root == Float(1.0)
    p = parse_expression(".1")
    assert p.root == Float(0.1)
    p = parse_expression("1e-3 + 2. * .3")
    assert p.root == BinOp("PLUS", Float(1e-3), BinOp("STAR", Float(2.0), Float(0.3)))

    p = parse_expression("r*l/w")
    assert p.root == BinOp(
        tp="STAR",
        left=Ident(name="r"),
        right=BinOp(tp="SLASH", left=Ident(name="l"), right=Ident(name="w")),
    )

    p = parse_expression("(0.5f * p)")  # SPICE metric-suffixed number
    assert p.root == BinOp(tp="STAR", left=MetricNum(val="0.5f"), right=Ident(name="p"))

    p = parse_expression(" ' a + b ' ")  # SPICE-style ticked-expression
    assert p.root == BinOp(tp="PLUS", left=Ident(name="a"), right=Ident(name="b"))

    p = parse_expression(" a + func(b, c) ")  # Function call
    assert p.root == BinOp(
        tp="PLUS",
        left=Ident(name="a"),
        right=Call(func=Ident(name="func"), args=[Ident(name="b"), Ident(name="c")]),
    )


def test_param_values():
    from netlist import ParamValue, Float, Expr, BinOp

    p = ParamValue(Float(5))
    p = ParamValue(BinOp("PLUS", Float(1e-3), BinOp("STAR", Float(2.0), Float(0.3))))

