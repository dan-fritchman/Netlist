"""
# Netlist Writer Base Class 

"""

# Std-Lib Imports
from ast import UnaryOp
from typing import Optional, Union, IO, Dict, Iterable
from enum import Enum
from dataclasses import dataclass, field

from numpy import isin

# Local Imports
from ..data import *
import vlsir


# Internal type shorthand
ModuleLike = Union[vlsir.circuit.Module, vlsir.circuit.ExternalModule]


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
class ResolvedModule:
    """ Resolved reference to a `Module` or `ExternalModule`. 
    Includes its spice-language prefix, and if user-defined its netlist-sanitized module-name. """

    module: ModuleLike
    module_name: str
    spice_prefix: SpicePrefix


@dataclass
class ResolvedParams:
    """ Resolved Instance-Parameter Values 
    Factoring in defaults, and converted to strings. 
    Largely a wrapper for `Dict[str, str]`, with accessors `get` and `pop` that raise `RuntimeError` if a key is missing. """

    values: Dict[str, str]

    def set(self, key: str, val: str) -> None:
        """ Set the value of `key` to `val` in the resolved parameters. """
        self.values[key] = val

    def get(self, key: str) -> str:
        """ Get the value of `key` from the resolved parameters. 
        Raises `RuntimeError` if `key` is not present. """
        if key not in self.values:
            raise RuntimeError(f"Missing parameter {key}")
        return self.values[key]

    def pop(self, key: str) -> str:
        """ Get the value of `key` from the resolved parameters, and remove it from the `ResolvedParams`. 
        Raises `RuntimeError` if `key` is not present. """
        if key not in self.values:
            raise RuntimeError(f"Missing parameter {key}")
        return self.values.pop(key)

    def pop_many(self, keys: Iterable[str]) -> Dict[str, str]:
        """ Get the values of `keys` from the resolved parameters, and remove them from the `ResolvedParams`. 
        Raises `RuntimeError` if any `key` is not present. """
        return {key: self.pop(key) for key in keys}

    @property
    def items(self):
        return self.values.items

    def __bool__(self):
        """ Boolean conversions, generally through the `not` keyword or `bool` constructor, 
        are forwarded down to the internal values dictionary. """
        return bool(self.values)


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

    def __init__(self, src: Program, dest: IO):
        self.src = src
        self.dest = dest
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

    def write_source_file(self, file: SourceFile) -> None:
        """ Write the content of `file` to `self.dest`. """
        self.write_comment(f"Source File: {file.path}")
        for entry in file.contents:
            self.write_entry(entry)

    def write_entry(self, entry: Entry) -> None:
        """ Dispatch across the various entry types. """

        if isinstance(entry, SubcktDef):
            return self.write_subckt_def(entry)
        if isinstance(entry, Library):
            return self.write_library(entry)
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
        unsupported = (DialectChange, FunctionDef, Unknown, AhdlInclude)
        if isinstance(entry, unsupported):
            raise ValueError(f"Unsupported Entry: {entry}")

        raise TypeError(f"Invalid or Unsupported Entry: {entry}")

    def write(self, s: str) -> None:
        """ Helper/wrapper, passing to `self.dest` """
        self.dest.write(s)

    def writeln(self, s: str) -> None:
        """ Write `s` as a line, at our current `indent` level. """
        self.write(f"{self.indent.state}{s}\n")

    def get_external_module(self, emod: vlsir.circuit.ExternalModule) -> None:
        """ Visit an ExternalModule definition.
        "Netlisting" these doesn't actually write anything,
        but just stores a reference  in internal dictionary `ext_modules`
        for future references to them. """
        key = (emod.name.domain, emod.name.name)
        if key in self.ext_modules:
            raise RuntimeError(f"Invalid doubly-defined external module {emod}")
        self.ext_modules[key] = emod

    @classmethod
    def get_param_default(cls, pparam: vlsir.circuit.Parameter) -> Optional[str]:
        """ Get the default value of `pparam`. Returns `None` for no default. """
        if pparam.default.WhichOneof("value") is None:
            return None
        return cls.get_param_value(pparam.default)

    @classmethod
    def get_param_value(cls, ppval: vlsir.circuit.ParameterValue) -> str:
        """ Get a string representation of a parameter-value """
        ptype = ppval.WhichOneof("value")
        if ptype == "integer":
            return str(int(ppval.integer))
        if ptype == "double":
            return str(float(ppval.double))
        if ptype == "string":
            return str(ppval.string)
        if ptype == "literal":
            return str(ppval.literal)
        raise ValueError

    @classmethod
    def get_instance_params(
        cls, pinst: vlsir.circuit.Instance, pmodule: ModuleLike
    ) -> ResolvedParams:
        """ Resolve the parameters of `pinst` to their values, including default values provided by `pmodule`. 
        Raises a `RuntimeError` if any required parameter is not defined. 

        Note this method *does not* raise errors for parameters *not specified* in `pmodule`, 
        allowing for "pass-through" parameters not explicitly defined. """

        values = dict()

        # Step through each of `pmodule`'s declared parameters first, applying defaults if necessary
        for mparam in pmodule.parameters:
            if mparam.name in pinst.parameters:  # Specified by the Instance
                inst_pval = pinst.parameters.pop(mparam.name)
                values[mparam.name] = cls.get_param_value(inst_pval)
            else:  # Not specified by the instance. Apply the default, or fail.
                pdefault = cls.get_param_default(mparam)
                if pdefault is None:
                    msg = f"Required parameter `{mparam.name}` not specified for Instance `{pinst}`"
                    raise RuntimeError(msg)
                values[mparam.name] = pdefault

        # Convert the remaining instance-provided parameters to strings
        for (pname, pval) in pinst.parameters.items():
            values[pname] = cls.get_param_value(pval)

        # And wrap the resolved values in a `ResolvedParams` object
        return ResolvedParams(values)

    @classmethod
    def get_module_name(cls, module: vlsir.circuit.Module) -> str:
        """ Create a netlist-compatible name for proto-Module `module` """

        # Create the module name
        # Replace all format-invalid characters with underscores
        name = module.name.split(".")[-1]
        for ch in name:
            if not (ch.isalpha() or ch.isdigit() or ch == "_"):
                name = name.replace(ch, "_")
        return name

    def resolve_reference(self, ref: vlsir.utils.Reference) -> ResolvedModule:
        """ Resolve the `ModuleLike` referent of `ref`. """

        if ref.WhichOneof("to") == "local":  # Internally-defined Module
            module = self.pmodules.get(ref.local, None)
            if module is None:
                raise RuntimeError(f"Invalid undefined Module {ref.local} ")
            return ResolvedModule(
                module=module,
                module_name=self.get_module_name(module),
                spice_prefix=SpicePrefix.SUBCKT,
            )

        if ref.WhichOneof("to") == "external":  # Defined outside package

            # First check the priviledged/ internally-defined domains
            if ref.external.domain == "vlsir.primitives":
                # Built-in primitive. Load its definition from the `vlsir.primitives` (python) module.
                name = ref.external.name
                module = vlsir.primitives.dct.get(ref.external.name, None)
                if module is None:
                    raise RuntimeError(f"Invalid undefined primitive {ref.external}")

                # Mapping from primitive-name to spice-prefix
                prefixes = dict(
                    resistor=SpicePrefix.RESISTOR,
                    capacitor=SpicePrefix.CAPACITOR,
                    inductor=SpicePrefix.INDUCTOR,
                    vdc=SpicePrefix.VSOURCE,
                    vpulse=SpicePrefix.VSOURCE,
                    vpwl=SpicePrefix.VSOURCE,
                    vsin=SpicePrefix.VSOURCE,
                    isource=SpicePrefix.ISOURCE,
                    vcvs=SpicePrefix.VCVS,
                    vccs=SpicePrefix.VCCS,
                    cccs=SpicePrefix.CCCS,
                    ccvs=SpicePrefix.CCVS,
                    mos=SpicePrefix.MOS,
                    bipolar=SpicePrefix.BIPOLAR,
                    diode=SpicePrefix.DIODE,
                )

                if name not in prefixes:
                    raise ValueError(f"Unsupported or Invalid Ideal Primitive {ref}")

                return ResolvedModule(
                    module=module,
                    module_name=module.name.name,
                    spice_prefix=prefixes[name],
                )

            if ref.external.domain == "hdl21.primitives":
                msg = f"Invalid direct-netlisting of physical `hdl21.Primitive` `{ref.external.name}`. "
                msg += "Either compile to a target technology, or replace with an `ExternalModule`. "
                raise RuntimeError(msg)

            if ref.external.domain == "hdl21.ideal":
                # FIXME: complete the deprecation of the dependency on `hdl21`.
                import warnings

                msg = f"Pending Deprecation: `hdl21.ideal` primitives. Move to `vlsir.primitives"
                warnings.warn(msg)

                # Ideal elements
                name = ref.external.name

                # Sort out the spectre-format name
                if name == "IdealCapacitor":
                    module_name = "capacitor"
                    spice_prefix = SpicePrefix.CAPACITOR
                elif name == "IdealResistor":
                    module_name = "resistor"
                    spice_prefix = SpicePrefix.RESISTOR
                elif name == "IdealInductor":
                    module_name = "inductor"
                    spice_prefix = SpicePrefix.INDUCTOR
                elif name == "VoltageSource":
                    module_name = "vsource"
                    spice_prefix = SpicePrefix.VSOURCE
                elif name == "CurrentSource":
                    module_name = "isource"
                    spice_prefix = SpicePrefix.ISOURCE
                else:
                    raise ValueError(f"Unsupported or Invalid Ideal Primitive {ref}")

                # Awkwardly, primitives don't naturally have definitions as
                # either `vlsir.circuit.Module` or `vlsir.circuit.ExternalModule`.
                # So we create one on the fly.

                # FIXME: these two dependencies should be removed!
                from hdl21.proto.to_proto import ProtoExporter
                from hdl21.proto.from_proto import ProtoImporter

                prim = ProtoImporter.import_hdl21_primitive(ref.external)
                module = ProtoExporter.export_hdl21_primitive(prim)
                return ResolvedModule(
                    module=module, module_name=module_name, spice_prefix=spice_prefix,
                )

            else:  # Externally-Defined, External-Domain `ExternalModule`
                key = (ref.external.domain, ref.external.name)
                module = self.ext_modules.get(key, None)
                if module is None:
                    msg = f"Invalid Instance of undefined External Module {key}"
                    raise RuntimeError(msg)
                # Check for duplicate names which would conflict from other namespaces
                module_name = ref.external.name
                if (
                    module_name in self.ext_module_names
                    and self.ext_module_names[module_name] is not module
                ):
                    msg = f"Conflicting ExternalModule definitions {module} and {self.ext_module_names[module_name]}"
                    raise RuntimeError(msg)
                self.ext_module_names[module_name] = module
                return ResolvedModule(
                    module=module,
                    module_name=module_name,
                    spice_prefix=SpicePrefix.SUBCKT,
                )

        # Not a Module, not an ExternalModule, not sure what it is
        raise ValueError(f"Invalid Module reference {ref}")

    def format_connection(self, pconn: vlsir.circuit.Connection) -> str:
        """ Format a `Connection` reference. 
        Does not *declare* any new connection objects, but generates references to existing ones. """
        # Connections are a proto `oneof` union
        # which includes signals, slices, and concatenations.
        # Figure out which to import

        stype = pconn.WhichOneof("stype")
        if stype == "sig":
            return self.format_signal_ref(pconn.sig)
        if stype == "slice":
            return self.format_signal_slice(pconn.slice)
        if stype == "concat":
            return self.format_concat(pconn.concat)
        raise ValueError(f"Invalid Connection Type {stype} for Connection {pconn}")

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

    def format_expr(self, expr: Expr) -> str:
        """ Format a mathematical Expression. """

        # Dispatch across the `Expr` type-union
        if isinstance(expr, (Int, Float, MetricNum)):
            # Scalar literals. Write their string value
            return str(expr.val)
        if isinstance(expr, Ident):
            return self.format_ident(expr)  # Identifiers
        if isinstance(expr, UnaryOp):
            return self.format_unary_op(expr)  # Unary Operators
        if isinstance(expr, BinaryOp):
            return self.format_binary_op(expr)
        if isinstance(expr, Call):
            return self.format_function_call(expr)

        # Carve out these explicitly unsupported (at least for now) cases:
        if isinstance(expr, TernOp):
            raise RuntimeError(f"Unsupported Expression Type {expr}")

        raise TypeError(f"Invalid or Unsupported Expression {expr}")

    def format_unary_op(self, op: UnaryOp) -> str:
        """ Format a unary operation . """

        operator = self.format_unary_operator(op.tp)
        targ = self.format_expr(op.targ)

        return f"{operator}{targ}"

    def format_binary_op(self, op: BinaryOp) -> str:
        """ Format a binary operation . """

        left = self.format_expr(op.left)
        operator = self.format_binary_operator(op.tp)
        right = self.format_expr(op.right)

        return f"{left}{operator}{right}"

    def format_binary_operator(self, tp: str) -> str:
        """ Format a binary operator """
        # FIXME: `tp` will become an enum or similar, and need actual conversion
        return tp

    def format_unary_operator(self, tp: str) -> str:
        """ Format a unary operator """
        # FIXME: `tp` will become an enum or similar, and need actual conversion
        return tp

    def format_function_call(self, call: Call) -> str:
        """ Format a function-call expression. """
        funcname = self.format_ident(call.func)
        args = [self.format_expr(arg) for arg in call.args]
        return f'{funcname}({",".join(args)})'

    """ 
    Virtual `format` Methods 
    """

    @classmethod
    def format_port_decl(cls, pport: vlsir.circuit.Port) -> str:
        """ Format a declaration of a `Port` """
        raise NotImplementedError

    @classmethod
    def format_port_ref(cls, pport: vlsir.circuit.Port) -> str:
        """ Format a reference to a `Port` """
        raise NotImplementedError

    @classmethod
    def format_signal_decl(cls, psig: vlsir.circuit.Signal) -> str:
        """ Format a declaration of Signal `psig` """
        raise NotImplementedError

    @classmethod
    def format_signal_ref(cls, psig: vlsir.circuit.Signal) -> str:
        """ Format a reference to Signal `psig` """
        raise NotImplementedError

    @classmethod
    def format_signal_slice(cls, pslice: vlsir.circuit.Slice) -> str:
        """ Format Signal-Slice `pslice` """
        raise NotImplementedError

    def format_concat(self, pconc: vlsir.circuit.Concat) -> str:
        """ Format the Concatenation of several other Connections """
        raise NotImplementedError

    @classmethod
    def format_bus_bit(cls, index: Union[int, str]) -> str:
        """ Format bus-bit `index` """
        raise NotImplementedError

    """ 
    Virtual `write` Methods 
    """

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

