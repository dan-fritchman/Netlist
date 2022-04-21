"""
# Netlist Writer Base Class 

"""

# Std-Lib Imports
from warnings import warn
from typing import Tuple, Union, IO, Dict, Iterable
from enum import Enum, auto
from dataclasses import dataclass, field


# Local Imports
from ..data import *


class ErrorMode(Enum):
    """ Enumerated Error-Response Strategies """

    RAISE = "raise"  # Raise any generated exceptions
    COMMENT = "comment"  # Write errant entries as commments instead


class ExpressionState(Enum):
    """ Enumerated Expression States 
    Indicates within the writer whether it is amid an arithmetic expression. """

    PROGRAM = auto()
    EXPR = auto()


class SpicePrefix(Enum):
    """ Enumerated Spice Primitives and their Instance-Name Prefixes """

    # Sub-circits, either from `Module`s or `ExternalModule`s
    SUBCKT = "x"
    # Ideal Passives
    RESISTOR = "r"
    CAPACITOR = "c"
    INDUCTOR = "l"
    # Semiconductor Devices
    MOS = "m"
    DIODE = "d"
    BIPOLAR = "q"
    # Independent Sources
    VSOURCE = "v"
    ISOURCE = "i"
    # Dependent Sources
    VCVS = "e"
    VCCS = "g"
    CCCS = "f"
    CCVS = "h"
    # Transmission Lines
    TLINE = "o"


@dataclass
class Indent:
    """ 
    # Indentation Helper 
    
    Supports in-place addition and subtraction of indentation levels, e.g. via 
    ```python
    indent = Indent()
    indent += 1 # Adds one "tab", or indentation level
    indent += 1 # Adds another
    indent -= 1 # Drops back by one
    ```
    The current indentation-string is available via the `state` attribute. 
    Writers using such an indenter will likely be of the form:
    ```python
    dest.write(f"{indent.state}{content}")
    ```
    """

    # Per-"tab" indentation characters. Defaults to two spaces.
    chars: str = 2 * " "
    # Current (integer) indentation-level, in number of "tabs"
    num: int = field(init=False, default=0)
    # Current indentation-string. Always equals `num * chars`.
    state: str = field(init=False, default="")

    def __post_init_post_parse__(self) -> None:
        self.state = self.chars * self.num

    def __iadd__(self, other: int) -> None:
        """ In-place add, i.e. `indent += 1` """
        self.num += other
        self.state = self.chars * self.num
        return self

    def __isub__(self, other: int) -> None:
        """ In-place subtract, i.e. `indent -= 1` """
        self.num = self.num - other
        if self.num < 0:
            raise ValueError("Negative indentation")
        self.state = self.chars * self.num
        return self


class Netlister:
    """ # Abstract Base `Netlister` Class 

    `Netlister` is not directly instantiable, and none of its sub-classes are intended 
    for usage outside the `netlist` package. The primary API method `netlist` is designed to 
    create, use, and drop a `Netlister` instance. 
    Once instantiated a `Netlister`'s primary API method is `netlist`. 
    This writes all content in its `src` field to destination `dest`. 
    
    Internal methods come in two primary flavors:
    * `write_*` methods, which write to `self.dest`. These methods are generally format-specific. 
    * `format_*` methods, which return format-specific strings, but *do not* write to `dest`. 
    * `get_*` methods, which retrieve some internal data, e.g. extracting the type of a `Connection`. 
    """

    def __init__(
        self, src: Program, dest: IO, *, errormode: ErrorMode = ErrorMode.RAISE
    ) -> None:
        self.src = src
        self.dest = dest
        self.errormode = errormode
        self.expr_state = ExpressionState.PROGRAM
        self.indent = Indent(chars="  ")
        self.module_names = set()  # Netlisted Module names
        self.pmodules = dict()  # Visited proto-Modules
        self.ext_modules = dict()  # Visited ExternalModules
        self.ext_module_names = (
            dict()
        )  # Visited ExternalModule names, checked for duplicates

    def netlist(self) -> None:
        """ Primary API Method.
        Convert everything in `self.src` and write to `self.dest`. """

        # Write some header commentary
        self.write_header()

        # Now do the real stuff, writing the content of each source-file
        # FIXME: some SPICE may treat some things different semantically per source-file.
        # We may end up needing to break them up, rather than writing to one big `dest` file.
        for file in self.src.files:
            self.write_source_file(file)

        # And ensure all output makes it to `self.dest`
        self.dest.flush()

    def handle_error(self, entry: Entry, msg: str) -> None:
        """ React to an error, depending on `self.errormode`. """
        if self.errormode is ErrorMode.RAISE:
            raise RuntimeError(msg)
        elif self.errormode is ErrorMode.COMMENT:
            msg = f"Warning: invalid Entry {entry}".replace("\n", " ")
            warn(msg)
            self.write_comment(msg)
        else:
            raise ValueError(f"Unknown error mode {self.errormode}")

    def write_source_file(self, file: SourceFile) -> None:
        """ Write the content of `file` to `self.dest`. """
        self.write_comment(f"Source File: {file.path}")
        for entry in file.contents:
            self.write_entry(entry)

    def write_entry(self, entry: Entry) -> None:
        """ Write an `Entry`. Primarily dispatches across the `Entry` union-types. """

        if isinstance(entry, SubcktDef):
            return self.write_subckt_def(entry)
        if isinstance(entry, LibSection):
            return self.write_library_section(entry)
        if isinstance(entry, End):
            return self.write_end(entry)
        if isinstance(entry, End):
            return self.write_end(entry)
        if isinstance(entry, Instance):
            return self.write_subckt_instance(entry)
        if isinstance(entry, Primitive):
            return self.write_primitive_instance(entry)
        if isinstance(entry, ParamDecls):
            return self.write_param_decls(entry)
        if isinstance(entry, ModelDef):
            return self.write_model_def(entry)
        if isinstance(entry, ModelVariant):
            return self.write_model_variant(entry)
        if isinstance(entry, ModelFamily):
            return self.write_model_family(entry)
        if isinstance(entry, Options):
            return self.write_options(entry)
        if isinstance(entry, Include):
            return self.write_include(entry)
        if isinstance(entry, UseLib):
            return self.write_use_lib(entry)
        if isinstance(entry, StatisticsBlock):
            return self.write_statistics_block(entry)

        # Explicitly note these data-types as unsupported for writing
        unsupported = (DialectChange, FunctionDef, Unknown, AhdlInclude, Library)
        # FIXME: is writing `Library` even really a thing?
        if isinstance(entry, unsupported):
            return self.handle_error(entry, f"Unsupported Entry: {entry}")

        return self.handle_error(entry, f"Invalid Entry: {entry}")

    def write(self, s: str) -> None:
        """ Helper/wrapper, passing to `self.dest` """
        self.dest.write(s)

    def writeln(self, s: str) -> None:
        """ Write `s` as a line, at our current `indent` level. """
        self.write(f"{self.indent.state}{s}\n")

    # @classmethod
    # def get_module_name(cls, module: vlsir.circuit.Module) -> str:
    #     """ Create a netlist-compatible name for proto-Module `module` """

    #     # Create the module name
    #     # Replace all format-invalid characters with underscores
    #     name = module.name.split(".")[-1]
    #     for ch in name:
    #         if not (ch.isalpha() or ch.isdigit() or ch == "_"):
    #             name = name.replace(ch, "_")
    #     return name

    # def resolve_reference(self, ref: vlsir.utils.Reference) -> ResolvedModule:
    #     """ Resolve the `ModuleLike` referent of `ref`. """

    #     if ref.WhichOneof("to") == "local":  # Internally-defined Module
    #         module = self.pmodules.get(ref.local, None)
    #         if module is None:
    #             raise RuntimeError(f"Invalid undefined Module {ref.local} ")
    #         return ResolvedModule(
    #             module=module,
    #             module_name=self.get_module_name(module),
    #             spice_prefix=SpicePrefix.SUBCKT,
    #         )

    #     if ref.WhichOneof("to") == "external":  # Defined outside package

    #         # First check the priviledged/ internally-defined domains
    #         if ref.external.domain == "vlsir.primitives":
    #             # Built-in primitive. Load its definition from the `vlsir.primitives` (python) module.
    #             name = ref.external.name
    #             module = vlsir.primitives.dct.get(ref.external.name, None)
    #             if module is None:
    #                 raise RuntimeError(f"Invalid undefined primitive {ref.external}")

    #             # Mapping from primitive-name to spice-prefix
    #             prefixes = dict(
    #                 resistor=SpicePrefix.RESISTOR,
    #                 capacitor=SpicePrefix.CAPACITOR,
    #                 inductor=SpicePrefix.INDUCTOR,
    #                 vdc=SpicePrefix.VSOURCE,
    #                 vpulse=SpicePrefix.VSOURCE,
    #                 vpwl=SpicePrefix.VSOURCE,
    #                 vsin=SpicePrefix.VSOURCE,
    #                 isource=SpicePrefix.ISOURCE,
    #                 vcvs=SpicePrefix.VCVS,
    #                 vccs=SpicePrefix.VCCS,
    #                 cccs=SpicePrefix.CCCS,
    #                 ccvs=SpicePrefix.CCVS,
    #                 mos=SpicePrefix.MOS,
    #                 bipolar=SpicePrefix.BIPOLAR,
    #                 diode=SpicePrefix.DIODE,
    #             )

    #             if name not in prefixes:
    #                 raise ValueError(f"Unsupported or Invalid Ideal Primitive {ref}")

    #             return ResolvedModule(
    #                 module=module,
    #                 module_name=module.name.name,
    #                 spice_prefix=prefixes[name],
    #             )

    #         if ref.external.domain == "hdl21.primitives":
    #             msg = f"Invalid direct-netlisting of physical `hdl21.Primitive` `{ref.external.name}`. "
    #             msg += "Either compile to a target technology, or replace with an `ExternalModule`. "
    #             raise RuntimeError(msg)

    #         if ref.external.domain == "hdl21.ideal":
    #             # FIXME: complete the deprecation of the dependency on `hdl21`.
    #             import warnings

    #             msg = f"Pending Deprecation: `hdl21.ideal` primitives. Move to `vlsir.primitives"
    #             warnings.warn(msg)

    #             # Ideal elements
    #             name = ref.external.name

    #             # Sort out the spectre-format name
    #             if name == "IdealCapacitor":
    #                 module_name = "capacitor"
    #                 spice_prefix = SpicePrefix.CAPACITOR
    #             elif name == "IdealResistor":
    #                 module_name = "resistor"
    #                 spice_prefix = SpicePrefix.RESISTOR
    #             elif name == "IdealInductor":
    #                 module_name = "inductor"
    #                 spice_prefix = SpicePrefix.INDUCTOR
    #             elif name == "VoltageSource":
    #                 module_name = "vsource"
    #                 spice_prefix = SpicePrefix.VSOURCE
    #             elif name == "CurrentSource":
    #                 module_name = "isource"
    #                 spice_prefix = SpicePrefix.ISOURCE
    #             else:
    #                 raise ValueError(f"Unsupported or Invalid Ideal Primitive {ref}")

    #             # Awkwardly, primitives don't naturally have definitions as
    #             # either `vlsir.circuit.Module` or `vlsir.circuit.ExternalModule`.
    #             # So we create one on the fly.

    #             # FIXME: these two dependencies should be removed!
    #             from hdl21.proto.to_proto import ProtoExporter
    #             from hdl21.proto.from_proto import ProtoImporter

    #             prim = ProtoImporter.import_hdl21_primitive(ref.external)
    #             module = ProtoExporter.export_hdl21_primitive(prim)
    #             return ResolvedModule(
    #                 module=module, module_name=module_name, spice_prefix=spice_prefix,
    #             )

    #         else:  # Externally-Defined, External-Domain `ExternalModule`
    #             key = (ref.external.domain, ref.external.name)
    #             module = self.ext_modules.get(key, None)
    #             if module is None:
    #                 msg = f"Invalid Instance of undefined External Module {key}"
    #                 raise RuntimeError(msg)
    #             # Check for duplicate names which would conflict from other namespaces
    #             module_name = ref.external.name
    #             if (
    #                 module_name in self.ext_module_names
    #                 and self.ext_module_names[module_name] is not module
    #             ):
    #                 msg = f"Conflicting ExternalModule definitions {module} and {self.ext_module_names[module_name]}"
    #                 raise RuntimeError(msg)
    #             self.ext_module_names[module_name] = module
    #             return ResolvedModule(
    #                 module=module,
    #                 module_name=module_name,
    #                 spice_prefix=SpicePrefix.SUBCKT,
    #             )

    #     # Not a Module, not an ExternalModule, not sure what it is
    #     raise ValueError(f"Invalid Module reference {ref}")

    def write_header(self) -> None:
        """ Write header commentary 
        This proves particularly important for many Spice-like formats, 
        which *always* interpret the first line of an input-file as a comment (among many other dumb things). 
        So, always write one there right off the bat. """

        # FIXME!
        # if self.src.domain:
        #     self.write_comment(f"circuit.Package {self.src.domain}")
        # else:
        #     self.write_comment(f"Anonymous circuit.Package")
        self.write_comment(f"Written by {self.__class__.__name__}")
        self.write_comment("")
        self.writeln("")

    def format_ident(self, ident: Ident) -> str:
        """ Format an identifier. Default just returns its string `name`. """
        return ident.name

    def expression_delimiters(self) -> Tuple[str, str]:
        """ Return the starting and closing delimiters for expressions. """
        # Base case: single-ticks
        return ("'", "'")

    def format_number(self, num: Union[Int, Float, MetricNum]) -> str:
        """ Format a numeric literal. """
        return str(num.val)

    def format_expr(self, expr: Expr) -> str:
        """ Format a mathematical Expression. 
        Primarily dispatches across the `Expr` type-union. """

        # Scalar literals. Return their string value.
        if isinstance(expr, (Int, Float, MetricNum)):
            return self.format_number(expr)

        # Everything else is some form of compound expression, e.g. a function call, add, or reference to another parameter.
        # If we are not already in a sub-expression, write the evaluator character.
        if self.expr_state != ExpressionState.EXPR:
            self.expr_state = ExpressionState.EXPR
            rv = self.expression_delimiters()[0]
            close_me_please = True
        else:
            rv = ""
            close_me_please = False

        if isinstance(expr, Ident):
            rv += self.format_ident(expr)  # Identifiers
        elif isinstance(expr, UnaryOp):
            rv += self.format_unary_op(expr)  # Unary Operators
        elif isinstance(expr, BinaryOp):
            rv += self.format_binary_op(expr)
        elif isinstance(expr, Call):
            rv += self.format_function_call(expr)
        elif isinstance(expr, TernOp):
            rv += self.format_ternary_op(expr)
        else:
            raise TypeError(f"Invalid or Unsupported Expression {expr}")

        if close_me_please:
            self.expr_state = ExpressionState.PROGRAM
            rv += self.expression_delimiters()[1]

        return rv

    def format_unary_op(self, op: UnaryOp) -> str:
        """ Format a unary operation . """

        operator = self.format_unary_operator(op.tp)
        targ = self.format_expr(op.targ)

        return f"{operator}{targ}"

    def format_unary_operator(self, tp: UnaryOperator) -> str:
        """ Format a unary operator """
        return tp.value

    def format_binary_op(self, op: BinaryOp) -> str:
        """ Format a binary operation . """

        left = self.format_expr(op.left)
        operator = self.format_binary_operator(op.tp)
        right = self.format_expr(op.right)

        return f"{left}{operator}{right}"

    def format_binary_operator(self, tp: BinaryOperator) -> str:
        """ Format a binary operator """
        return tp.value

    def format_ternary_op(self, op: TernOp) -> str:
        """ Format a ternary operation. """

        cond = self.format_expr(op.cond)
        if_true = self.format_expr(op.if_true)
        if_false = self.format_expr(op.if_false)

        return f"{cond} ? {if_true} : {if_false}"

    def format_function_call(self, call: Call) -> str:
        """ Format a function-call expression. """
        funcname = self.format_ident(call.func)
        args = [self.format_expr(arg) for arg in call.args]
        return f'{funcname}({",".join(args)})'

    """ 
    Virtual `format` Methods 
    """

    @classmethod
    def format_bus_bit(cls, index: Union[int, str]) -> str:
        """ Format bus-bit `index` """
        raise NotImplementedError

    """ 
    Virtual `write` Methods 
    """

    def write_include(self, inc: Include) -> None:
        """ Write a file-Include """
        raise NotImplementedError

    def write_use_lib(self, uselib: UseLib) -> None:
        """ Write a sectioned Library-usage """
        raise NotImplementedError

    def write_library_section(self, section: LibSection) -> None:
        """ Write a Library Section definition """
        raise NotImplementedError

    def write_model_def(self, model: ModelDef) -> None:
        """ Write a model definition """
        raise NotImplementedError

    def write_model_def(self, model: ModelDef) -> None:
        """ Write a model definition """
        raise NotImplementedError

    def write_statistics_block(self, stats: StatisticsBlock) -> None:
        """ Write a StatisticsBlock `stats` """
        raise NotImplementedError

    def write_param_decls(self, params: ParamDecls) -> None:
        """ Write parameter declarations """
        raise NotImplementedError

    def write_param_decl(self, param: ParamDecl) -> None:
        """ Write a parameter declaration """
        raise NotImplementedError

    def write_param_val(self, param: ParamVal) -> None:
        """ Write a parameter value """
        raise NotImplementedError

    def write_comment(self, comment: str) -> None:
        """ Format and string a string comment. 
        "Line comments" are the sole supported variety, which fit within a line, and extend to the end of that line. 
        The `write_comment` method assumes responsibility for closing the line. """
        raise NotImplementedError

    def write_subckt_def(self, subckt: SubcktDef) -> None:
        """ Write Subckt Definition `subckt` """
        raise NotImplementedError

    def write_instance(self, inst: Instance) -> None:
        """ Write Instance `inst` """
        raise NotImplementedError

    def write_options(self, options: Options) -> None:
        """ Write Options `options` """
        raise NotImplementedError

    def write_expr(self, expr: Expr) -> None:
        """ Write a (potentially nested) mathematical expression `expr` """
        raise NotImplementedError

    """ 
    Other Virtual Methods 
    """

    @property
    def enum(self):
        """ Get our entry in the `NetlistFormat` enumeration """
        raise NotImplementedError
