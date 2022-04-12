# Netlist

Circuit netlist generation & parsing

[Netlist](https://github.com/dan-fritchman/Netlist) is a Python package for reading, manipulating, and writing popular circuit netlist formats.
Particularly emphasis is placed on the _device model_ and _technology library_ constructs of netlist formats.

## The Netlist AST

The data model used throughout the `netlist` package is defined in [data.py](netlist/data.py).
The primary top-level entity is a `Program` - a term which Spice-culture generally lacks,
but means "a collection of input `SourceFile`s for simulation".

## Netlist Dialects

"Spice" format is not a terribly well-defined term.
Netlists come in a variety of dialects, each generally specific to a particular simulation program.
These dialects represent the same conceptual content - circuits, instances of sub-circuits, models, parameters, and the like -
but do so with varying syntax and semantics.

The `netlist` package is designed to support the most popular dialects.
Enumerated, supported dialects are listed by the `netlist.NetlistDialects` enumeration.
Support as of this writing includes:

| Dialect              | Parsing | Writing |
| -------------------- | ------- | ------- |
| "Generic" Spice (\*) | [x]     | [ ]     |
| Hspice               | [x]     | [ ]     |
| Ngspice              | [ ]     | [ ]     |
| CDL                  | [ ]     | [ ]     |
| Xyce                 | [ ]     | [x]     |
| Spectre              | [x]     | [ ]     |
| Spectre-Spice (\*)   | [x]     | [ ]     |

Notes:

- While "generic spice" format is something of an oxymoron, a base-case set of rules covers many common constructs, and should cover most syntax for an unlisted simulation program.
- "Spectre-Spice" is the spice-dialect supported by Cadence's Spectre. It is largely identical to "generic spice", plus the capacity for `simulator lang` changes into spectre-syntax.

## Supported Constructs

`Netlist` focuses on the netlist-constructs most commonly used for device models and technology libraries, including:

- Circuit hierarchy via `SubcktDef` sub-circuit-definitions, `Instance`s thereof, and `Primitive` instances
- `ModelDef` statements for defining device models
  - Binned models, generally separated by physical quantities such as {lmin,lmax}, are grouped into `ModelFamily`s and `ModelVariant`s.
- Parameter declarations (`ParamDecl`) and values (`ParamVal`)
- Mathematical expressions (`Expr`) including
  - `UnaryOp`, `BinaryOp`, and `TernaryOp` among parameters
  - Funcion `Call`s
- Limited `FunctionDef` definitions, in supporting dialects
- `Options`

Common unsupported constructs include most used for simulation stimulus-generation: analyses, measurements, probes, and similar.

## Parsing

The primary entry point for parsing an existing netlist is `netlist.parse`.
This function takes a `Path` or `PathLike` input, and returns a netlist `Program`.

```python
from netlist import parse
program = parse("mymodels.sp")
```

The `parse` function accepts an optional `dialect` argument, which sets the initial spice-dialect for parsing.
If not provided

## Writing

The primary entrypoint for writing a netlist `Program` is `netlist.netlist` (as a _verb_). The `netlist` function accepts a source `Program` and an open `IO` object - commonly an open file-handle - as a destination.

```python
from netlist import write
write(program, open("mymodels.scs", "w"), fmt=NetlistDialects.SPECTRE)
```

The `netlist` function accepts an optional `dialect` argument, which sets the dialect for writing.

## Conversions

Real-life netlist-programs can be long - at times millions of text-lines -
and can be broken up into a series of smaller files.
For tasks not requiring any in-memory analysis or manipulation, but strict conversion from one format to another,
`netlist` includes a `convert` function which maps potentially multi-file input to a directory-full of output.

```python
from netlist import convert
convert(...)
```

## Development 

To get started and install all development dependencies: 

```
pip install -e ".[dev]"
```

### Environment Setup 

This project includes bits of collateral for both `setuptools` (`setup.py`) and `poetry` (`pyproject.toml`, by another name).  
It ultimately uses `setuptools`. 

While `poetry` is great and we love it, and especially love TOML instead of Python for configuration, 
it's not quite ready for prime time with respect to a few features we use. 
Especially *editable installs*, i.e. those generated with `pip install -e .`. 
Note generating these editable installs depends on *not* having a `pyproject.toml` file present, 
or `pip` generally chokes. Hence the alternate name of the ostensible `pyproject.toml` file. 

