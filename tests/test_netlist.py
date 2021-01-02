def test_version():
    from netlist import __version__

    assert __version__ == "0.1.0"


def test_exprs():
    from netlist import parse_expression, Int, Float, BinOp, Ident

    p = parse_expression("1")
    assert p.root == Int(1)
    p = parse_expression("1+2")
    assert p.root == BinOp("PLUS", Int(1), Int(2))
    p = parse_expression("1+2*3")
    assert p.root == BinOp("PLUS", Int(1), BinOp("STAR", Int(2), Int(3)))
    p = parse_expression("1*2+3")
    assert p.root == BinOp(tp="PLUS", left=BinOp("STAR", Int(1), Int(2)), right=Int(3))

    p = parse_expression("a     ")
    assert p.root == Ident("a")
    p = parse_expression("   b + 1     ")
    assert p.root == BinOp("PLUS", Ident("b"), Int(1))

    p = parse_expression("1e-3")
    assert p.root == Float(1e-3)
    p = parse_expression("1.0")
    assert p.root == Float(1.0)
    p = parse_expression("1.")
    assert p.root == Float(1.)
    p = parse_expression(".1")
    assert p.root == Float(.1)
    p = parse_expression("1e-3 + 2. * .3")
    assert p.root == BinOp("PLUS", Float(1e-3), BinOp("STAR", Float(2.), Float(.3)))

