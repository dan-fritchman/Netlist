""" 
# Spice Format Netlisting

"Spice-format" is a bit of a misnomer in netlist-world. 
Of the countless Spice-class simulators have been designed the past half-century, 
most have a similar general netlist format, including: 

* Simulation input comprises a file-full of: 
  * (a) Circuit elements, arranged in `vlsir.Module`s, and
  * (b) Simulator control "cards", such as analysis statements, global parameters, measurements, probes, and the like.
* Circuit-specification is aided by hierarchy, generally in the form of "sub-circuits", denoted `SUBCKT`. 
  * Sub-circuits can commonly be parameterized, and can use a limited set of "parameter programming" to maniupulate their own parameter-values into those of their child-instances. 
  * For example, an instance might be declared as: `xdiode p n area=`width*length``, where `width` and `length` are parameters or its parent.
* `Signal`s are all scalar nets, which are created "out of thin air" whenever referenced. 
* "Typing" performed by instance-name prefixes, e.g. instances named `r1` being interpreted as resistors. 
* Many other subtleties, such as the typical case-insensitivity of netlist content (e.g. `V1` and `v1` are the same net). 

However each simulator also differs in ways large and small. 
Common differences manifest in the areas of: 

* How sub-circuits parameters are declared, and correspondingly how instance-parameter values are set. 
  * Sandia Lab's *Xyce* differs in a prominent fashion, adding a `PARAMS:` keyword where declarations and values begin. 
* How arithmetic expressions are specified, and what functions and expressions are available.
  * Common methods include back-ticks (Hspice) and squiggly-brackets (NgSpice).
  * Notably the asterisk-character (`*`) is the comment-character in many of these formats, and must be wrapped in an expression to perform multiplication. 
* The types and locations of *comments* that are supported. 
  * Some include the fun behavior that comments beginning mid-line require *different* comment-characters from those starting at the beginning of a line.
* While not an HDL attribute, they often differ even more in how simulation control is specified, particularly in analysis and saving is specified. 

"Spice" netlisting therefore requires a small family of "Spice Dialects", 
heavily re-using a central `SpiceNetlister` class, but requiring simulator-specific implementation details. 

"""

# Std-Lib Imports
import sys
from enum import Enum
from warnings import warn
from typing import Tuple, Union, List

# Local Imports
from ..data import *
from .base import Netlister, ErrorMode


class SpiceNetlister(Netlister):
    """
    # "Generic" Spice Netlister 
    and base-class for Spice dialects.

    Performs nearly all data-model traversal, 
    offloading syntax-specifics to dialect-specific sub-classes. 

    Attempts to write only the "generic" subset of Spice-content, 
    in the "most generic" methods as perceived by the authors. 
    This may not work for *any* particular simulator; see the simulator-specific dialects below, 
    and the module-level commentary above for more on why. 
    """

    @property
    def enum(self):
        """ Get our entry in the `NetlistFormat` enumeration """
        from . import NetlistFormat

        return NetlistFormat.SPICE

    def write_subckt_def(self, module: SubcktDef) -> None:
        """ Write the `SUBCKT` definition for `Module` `module`."""

        # Create the module name
        module_name = self.format_ident(module.name)
        # # Check for double-definition
        # if module_name in self.module_names:
        #     raise RuntimeError(f"Module {module_name} doubly defined")
        # # Add to our visited lists
        # self.module_names.add(module_name)
        # self.pmodules[module.name] = module

        # Create the sub-circuit definition header
        self.write(f".SUBCKT {module_name} \n")

        # Create its ports, if any are defined
        if module.ports:
            self.write_port_declarations(module)
        else:
            self.write("+ ")
            self.write_comment("No ports")

        # Create its parameters, if any are defined
        if module.params:
            self.write_module_params(module.params)
        else:
            self.write("+ ")
            self.write_comment("No parameters")

        # End the `subckt` header-content with a blank line
        self.write("\n")

        # Write its internal content/ entries
        for entry in module.entries:
            self.write_entry(entry)

        # And close up the sub-circuit
        self.write(".ENDS\n\n")

    def write_port_declarations(self, module: SubcktDef) -> None:
        """ Write the port declarations for Module `module`. """
        self.write("+ ")
        for port in module.ports:
            self.write(self.format_ident(port) + " ")
        self.write("\n")

    def write_module_params(self, module: SubcktDef) -> None:
        """ Write the parameter declarations for Module `module`. 
        Parameter declaration format: `name1=val1 name2=val2 name3=val3 \n`"""
        self.write("+ ")
        for name, pparam in module.parameters.items():
            self.write_param_decl(name, pparam)
        self.write("\n")

    def write_subckt_instance(self, pinst: Instance) -> None:
        """ Write sub-circuit-instance `pinst` of `rmodule`. """

        # Write the instance name
        self.write(self.format_ident(pinst.name) + " \n")

        # Write its port-connections
        self.write_instance_conns(pinst)

        # Write the sub-circuit name
        self.write("+ " + self.format_ident(pinst.module) + " \n")

        # Write its parameter values
        self.write_instance_params(pinst.params)

        # Add a blank after each instance
        self.write("\n")

    def write_primitive_instance(self, pinst: Instance) -> None:
        """ Write primitive-instance `pinst` of `rmodule`. 
        Note spice's primitive instances often differn syntactically from sub-circuit instances, 
        in that they can have positional (only) parameters. """

        # Write the instance name
        self.write(self.format_ident(pinst.name) + " \n")

        self.write("+ ")
        for arg in pinst.args:
            # FIXME: at this point, we don't really know which of these arguments
            # is a connection, and which is a positional parameter-value.
            # This matters in some contexts, as parameters can be expressions,
            # which may need some expression-evaluation syntax.
            # For now, if the arguments are not scalars (identifiers or numbers), fail.
            if isinstance(arg, Ident):
                self.write(self.format_ident(arg) + " ")
            elif isinstance(arg, (Int, Float, MetricNum)):
                self.write(self.format_number(arg) + " ")
            else:
                msg = f"Primitive instance {pinst.name} has non-scalar argument {arg}"
                raise RuntimeError(msg)
        self.write(" \n")

        self.write("+ ")
        for kwarg in pinst.kwargs:
            self.write_param_val(kwarg)
            self.write(" ")

        # Add a blank after each instance
        self.write("\n")

    def write_instance_conns(self, pinst: Instance) -> None:
        """ Write the port-connections for Instance `pinst` """

        # Write a quick comment for port-less modules
        if not len(pinst.conns):
            self.write("+ ")
            return self.write_comment("No ports")

        if isinstance(pinst.conns[0], tuple):
            # FIXME: connections by-name are not supported.
            raise RuntimeError(f"Unsupported by-name connections on {pinst}")

        self.write("+ ")
        # And write the Instance ports, in that order
        for pconn in pinst.conns:
            self.write(self.format_ident(pconn) + " ")
        self.write("\n")

    def write_instance_params(self, pvals: List[ParamVal]) -> None:
        """ 
        Format and write the parameter-values in dictionary `pvals`. 
        
        Parameter-values format:
        ```
        XNAME 
        + <ports> 
        + <subckt-name> 
        + name1=val1 name2=val2 name3=val3 
        """

        self.write("+ ")

        if not pvals:  # Write a quick comment for no parameters
            return self.write_comment("No parameters")

        # And write them
        for pval in pvals:
            self.write_param_val(pval)
            self.write(" ")

        self.write("\n")

    # def format_concat(self, pconc: vlsir.circuit.Concat) -> str:
    #     """ Format the Concatenation of several other Connections """
    #     out = ""
    #     for part in pconc.parts:
    #         out += self.format_connection(part) + " "
    #     return out

    # @classmethod
    # def format_port_decl(cls, pport: vlsir.circuit.Port) -> str:
    #     """ Get a netlist `Port` definition """
    #     return cls.format_signal_ref(pport.signal)

    # @classmethod
    # def format_port_ref(cls, pport: vlsir.circuit.Port) -> str:
    #     """ Get a netlist `Port` reference """
    #     return cls.format_signal_ref(pport.signal)

    # @classmethod
    # def format_signal_ref(cls, psig: vlsir.circuit.Signal) -> str:
    #     """ Get a netlist definition for Signal `psig` """
    #     if psig.width < 1:
    #         raise RuntimeError
    #     if psig.width == 1:  # width==1, i.e. a scalar signal
    #         return psig.name
    #     # Vector/ multi "bit" Signal. Creates several spice signals.
    #     return " ".join(
    #         [f"{psig.name}{cls.format_bus_bit(k)}" for k in reversed(range(psig.width))]
    #     )

    # @classmethod
    # def format_signal_slice(cls, pslice: vlsir.circuit.Slice) -> str:
    #     """ Get a netlist definition for Signal-Slice `pslice` """
    #     base = pslice.signal
    #     indices = list(reversed(range(pslice.bot, pslice.top + 1)))
    #     if not len(indices):
    #         raise RuntimeError(f"Attempting to netlist empty slice {pslice}")
    #     return " ".join([f"{base}{cls.format_bus_bit(k)}" for k in indices])

    @classmethod
    def format_bus_bit(cls, index: Union[int, str]) -> str:
        """ Format-specific string-representation of a bus bit-index """
        # Spectre netlisting uses an underscore prefix, e.g. `bus_0`
        return "_" + str(index)

    def write_param_decl(self, param: ParamDecl) -> str:
        """ Format a parameter declaration """

        if param.distr is not None:
            msg = f"Unsupported `distr` for parameter {param.name} will be ignored"
            warn(msg)
            self.write("\n+ ")
            self.write_comment(msg)
            self.write("\n+ ")

        if param.default is None:
            msg = f"Required (non-default) parameter {param.name} is not supported by {self.__class__.__name__}. "
            msg += "Setting to maximum floating-point value {sys.float_info.max}, which almost certainly will not work if instantiated."
            warn(msg)
            default = str(sys.float_info.max)
        else:
            default = self.format_expr(param.default)

        self.write(f"{self.format_ident(param.name)}={default}")

    def write_param_val(self, param: ParamVal) -> None:
        """ Write a parameter value """

        name = self.format_ident(param.name)
        val = self.format_expr(param.val)
        self.write(f"{name}={val}")

    def write_comment(self, comment: str) -> None:
        """ While dialects vary, the *generic* Spice-comment begins with the asterisk. """
        self.write(f"* {comment}\n")

    def format_expression(self, expr: str) -> str:
        """ Format a string such that the target format interprets it as an expression. 
        Example:
        ```
        * Parameter Declarations
        .param v0=1 v1='v0+1' * <= Here
        * Instance with the same name 
        v0 1 0 dc='v0+2*v1' * <= And here 
        ``` 
        Note the latter case includes the star character (`*`) for multiplication, 
        where in many other contexts it is treated as the comment-character. 
        """
        # The base class does what (we think) is the most common practice:
        # wrapping expressions in single-tick quotes.
        return f"'{expr}'"

    def write_options(self, options: Options) -> None:
        """ Write Options `options` """
        if options.name is not None:
            msg = f"Warning invalid `name`d Options"
            warn(msg)
            self.write_comment(msg)
            self.write("\n")

        # Get to the actual option-writing
        self.write(".option \n")
        for option in options.vals:
            self.write("+ ")
            # FIXME: add the cases included in `OptionVal` but not `ParamVal`, notably quoted string-paths 
            self.write_param_val(option)
            self.write(" \n")
        self.write("\n")

    def write_statistics_block(self, stats: StatisticsBlock) -> None:
        """ Write a StatisticsBlock `stats` """

        # Not supported currently, maybe ever, by any of the supported spice-formats.
        # Write as a comment. FIXME: add the option to bail

        self.write("\n")
        msg = f"Unsupported `StatisticsBlock` for writer {self.__class__.__name__}"
        warn(msg)
        self.write_comment(msg)
        self.write("\n")

    def write_param_decls(self, params: ParamDecls) -> None:
        """ Write parameter declarations """
        self.write(".param \n")
        for p in params.params:
            self.write("+ ")
            self.write_param_decl(p)
            self.write("\n")
        self.write("\n")

    def write_model_family(self, mfam: ModelFamily) -> None:
        """ Write a model family """
        # Just requires writing each variant.
        # They will be output with `modelname.variant` names, as most SPICE formats want.
        for variant in mfam.variants:
            self.write_model_variant(variant)

    def write_model_variant(self, mvar: ModelVariant) -> None:
        """ Write a model variant """

        # This just convertes to a `ModelDef` with a dot-separated name, and running `write_model_def`.
        model = ModelDef(
            name=Ident(f"{mvar.model.name}.{mvar.variant.name}"),
            mtype=mvar.mtype,
            args=mvar.args,
            params=mvar.params,
        )
        return self.write_model_def(model)

    def write_model_def(self, model: ModelDef) -> None:
        """ Write a model definition """

        mname = self.format_ident(model.name)
        mtype = self.format_ident(model.mtype)
        self.writeln(f".model {mname} {mtype}")

        self.write("+ ")
        for arg in model.args:
            self.write(self.format_expr(arg) + " ")

        self.write("\n")
        for param in model.params:
            self.write("+ ")
            self.write_param_decl(param)
            self.write("\n")

        self.write("\n")  # Ending blank-line

    def write_library_section(self, section: LibSection) -> None:
        """ Write a Library Section definition """
        self.write(f".lib {self.format_ident(section.name)}\n")
        for entry in section.entries:
            self.write_entry(entry)
        self.write(f".endl {self.format_ident(section.name)}\n\n")

    def write_include(self, inc: Include) -> None:
        """ Write a file-Include """
        # Format: `.include {path} `
        self.write(f".include {str(inc.path)}\n\n")

    def write_use_lib(self, uselib: UseLib) -> None:
        """ Write a sectioned Library-usage """
        # Format: `.lib {path} {section}`
        # Note quotes here are interpreted as part of `section`, and generally must be avoided.
        self.write(f".lib {str(uselib.path)} {self.format_ident(uselib.section)} \n\n")


class HspiceNetlister(SpiceNetlister):
    """
    # Hspice-Format Netlister 
    
    Other than its `NetlistFormat` enumeration, `HspiceNetlister` is identical to the base `SpiceNetlister`. 
    """

    @property
    def enum(self):
        """ Get our entry in the `NetlistFormat` enumeration """
        from . import NetlistFormat

        return NetlistFormat.HSPICE


class XyceNetlister(SpiceNetlister):
    """ Xyce-Format Netlister """

    @property
    def enum(self):
        """ Get our entry in the `NetlistFormat` enumeration """
        from . import NetlistFormat

        return NetlistFormat.XYCE

    def write_module_params(self, params: List[ParamDecl]) -> None:
        """ Write the parameter declarations for Module `module`. 
        Parameter declaration format:
        .SUBCKT <name> <ports> 
        + PARAMS: name1=val1 name2=val2 name3=val3 \n
        """
        self.write("+ PARAMS: ")  # <= Xyce-specific
        for param in params:
            self.write_param_decl(param)
            self.write(" ")
        self.write("\n")

    def write_instance_params(self, pvals: List[ParamVal]) -> None:
        """ Write the parameter-values for Instance `pinst`. 

        Parameter-values format:
        ```
        XNAME 
        + <ports> 
        + <subckt-name> 
        + PARAMS: name1=val1 name2=val2 name3=val3 
        """
        self.write("+ ")

        if not pvals:  # Write a quick comment for no parameters
            return self.write_comment("No parameters")

        self.write("PARAMS: ")  # <= Xyce-specific
        # And write them
        for pval in pvals:
            self.write_param_val(pval)
            self.write(" ")

        self.write("\n")

    def write_comment(self, comment: str) -> None:
        """ Xyce comments *kinda* support the Spice-typical `*` charater, 
        but *only* as the first character in a line. 
        Any mid-line-starting comments must use `;` instead. 
        So, just use it all the time. """
        self.write(f"; {comment}\n")

    def expression_delimiters(self) -> Tuple[str, str]:
        """ Return the starting and closing delimiters for expressions. """
        return ("{", "}")

    def write_options(self, options: Options) -> None:
        """ Write Options `options` 
        
        Xyce differs from many Spice-class simulators 
        in categorizing options, and requiring that users known their category. 
        Example categories include settings for the netlist parser, 
        device models, and many, many solver configurations. 

        The `netlist` AST also does not include this information, 
        so valid options per-category are defined here. 
        """

        if options.name is not None:
            msg = f"Warning invalid `name`d Options"
            warn(msg)
            self.write_comment(msg)
            self.write("\n")

        class Category(Enum):
            """ Xyce-specific categories """

            DEVICE = "device"
            PARSER = "parser"

        # Mapping from option-names to categories
        categories = {
            "scale": Category.PARSER,
            # FIXME: everything else
        }

        # Group the options by category
        by_category = {}
        for option in options.vals:
            name = self.format_ident(option.name)
            if name not in categories:
                msg = f"Unknown Xyce option `{name}`, cannot find `.options` category"
                raise RuntimeError(msg)
            category = categories[name].value
            if category not in by_category:
                by_category[category] = []
            by_category[category].append(option)

        for (category_name, category_list) in by_category.items():
            # Get to the actual option-writing
            self.write(f".options {category_name} \n")
            for option in category_list:
                self.write("+ ")
                # FIXME: add the cases included in `OptionVal` but not `ParamVal`, notably quoted string-paths 
                self.write_param_val(option)
                self.write(" \n")
            self.write("\n")


class NgspiceNetlister(SpiceNetlister):
    """ FIXME: Ngspice-Format Netlister """

    def __init__(self, *_, **__):
        raise NotImplementedError

    @property
    def enum(self):
        """ Get our entry in the `NetlistFormat` enumeration """
        from . import NetlistFormat

        return NetlistFormat.NGSPICE


class CdlNetlister(SpiceNetlister):
    """ FIXME: CDL-Format Netlister """

    def __init__(self, *_, **__):
        raise NotImplementedError

    @property
    def enum(self):
        """ Get our entry in the `NetlistFormat` enumeration """
        from . import NetlistFormat

        return NetlistFormat.CDL
