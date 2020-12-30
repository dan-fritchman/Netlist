

def test_version():
    from netlist import __version__
    assert __version__ == '0.1.0'

def test_bigtime():
    from netlist import parse
    parse('/tools/B/dan_fritchman/sky130/skywater-pdk/libraries/sky130_fd_pr/latest/models/all.spice')

    