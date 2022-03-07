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
from typing import Dict, Union

# Local Imports
import vlsir

# Import the base-class
from .base import Netlister, ResolvedModule, ResolvedParams, SpicePrefix, ModuleLike


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

    def write_module_definition(self, module: vlsir.circuit.Module) -> None:
        """ Write the `SUBCKT` definition for `Module` `module`."""

        # Create the module name
        module_name = self.get_module_name(module)
        # Check for double-definition
        if module_name in self.module_names:
            raise RuntimeError(f"Module {module_name} doubly defined")
        # Add to our visited lists
        self.module_names.add(module_name)
        self.pmodules[module.name] = module

        # Create the sub-circuit definition header
        self.write(f".SUBCKT {module_name} \n")

        # Create its ports, if any are defined
        if module.ports:
            self.write_port_declarations(module)
        else:
            self.write("+ ")
            self.write_comment("No ports")

        # Create its parameters, if any are defined
        if module.parameters:
            self.write_param_declarations(module)
        else:
            self.write("+ ")
            self.write_comment("No parameters")

        # End the `subckt` header-content with a blank line
        self.write("\n")

        # Create its instances
        for pinst in module.instances:
            self.write_instance(pinst)

        # And close up the sub-circuit
        self.write(".ENDS\n\n")

    def write_port_declarations(self, module: vlsir.circuit.Module) -> None:
        """ Write the port declarations for Module `module`. """
        self.write("+ ")
        for pport in module.ports:
            self.write(self.format_port_decl(pport) + " ")
        self.write("\n")

    def write_param_declarations(self, module: vlsir.circuit.Module) -> None:
        """ Write the parameter declarations for Module `module`. 
        Parameter declaration format: `name1=val1 name2=val2 name3=val3 \n`"""
        self.write("+ ")
        for name, pparam in module.parameters.items():
            self.write(self.format_param_decl(name, pparam))
        self.write("\n")

    def write_instance_name(
        self, pinst: vlsir.circuit.Instance, rmodule: ResolvedModule
    ) -> None:
        """ Write the instance-name line for `pinst`, including the SPICE-dictated primitive-prefix. """
        self.write(f"{rmodule.spice_prefix.value}{pinst.name} \n")

    def write_instance(self, pinst: vlsir.circuit.Instance) -> None:
        """ Create and return a netlist-string for Instance `pinst`"""

        # Get its Module or ExternalModule definition,
        resolved = self.resolve_reference(pinst.module)

        # And dispatch to `subckt` or `primitive` writers
        if resolved.spice_prefix == SpicePrefix.SUBCKT:
            return self.write_subckt_instance(pinst, resolved)

        if resolved.spice_prefix == SpicePrefix.VSOURCE:
            # Voltage sources get weird, and vary between dialiects. Farm them out to a dedicated method.
            return self.write_voltage_source_instance(pinst, resolved)

        # Everything else falls into the `primitive` category
        return self.write_primitive_instance(pinst, resolved)

    def write_subckt_instance(
        self, pinst: vlsir.circuit.Instance, rmodule: ResolvedModule
    ) -> None:
        """ Write sub-circuit-instance `pinst` of `rmodule`. """

        # Write the instance name
        self.write_instance_name(pinst, rmodule)

        # Write its port-connections
        self.write_instance_conns(pinst, rmodule.module)

        # Write the sub-circuit name
        self.write("+ " + rmodule.module_name + " \n")

        # Write its parameter values
        resolved_param_values = self.get_instance_params(pinst, rmodule.module)
        self.write_instance_params(resolved_param_values)

        # Add a blank after each instance
        self.write("\n")

    def write_primitive_instance(
        self, pinst: vlsir.circuit.Instance, rmodule: ResolvedModule
    ) -> None:
        """ Write primitive-instance `pinst` of `rmodule`. 
        Note spice's primitive instances often differn syntactically from sub-circuit instances, 
        in that they can have positional (only) parameters. """

        # Write the instance name
        self.write_instance_name(pinst, rmodule)

        # Write its port-connections
        self.write_instance_conns(pinst, rmodule.module)

        # Resolve its parameter-values to spice-strings
        resolved_param_values = self.get_instance_params(pinst, rmodule.module)

        # Write special and/or positional parameters
        if rmodule.spice_prefix == SpicePrefix.RESISTOR:
            positional_keys = ["r"]
        elif rmodule.spice_prefix == SpicePrefix.CAPACITOR:
            positional_keys = ["c"]
        elif rmodule.spice_prefix == SpicePrefix.INDUCTOR:
            positional_keys = ["l"]
        elif rmodule.spice_prefix == SpicePrefix.ISOURCE:
            positional_keys = ["dc"]
        elif rmodule.spice_prefix in (
            SpicePrefix.VCVS,
            SpicePrefix.VCCS,
            SpicePrefix.CCCS,
            SpicePrefix.CCVS,
        ):
            positional_keys = ["gain"]
        elif rmodule.spice_prefix in (
            SpicePrefix.MOS,
            SpicePrefix.BIPOLAR,
            SpicePrefix.DIODE,
            SpicePrefix.TLINE,
        ):
            positional_keys = ["modelname"]
        else:
            positional_keys = []

        # Pop all positional parameters ("pp") from `resolved_param_values`
        pp = resolved_param_values.pop_many(positional_keys)

        # Write the positional parameters, in the order specified by `positional_keys`
        self.write("+ " + " ".join([pp[pkey] for pkey in positional_keys]) + " \n")

        # Now! Write its subckt-style by-name parameter values
        self.write_instance_params(resolved_param_values)

        # Add a blank after each instance
        self.write("\n")

    def write_voltage_source_instance(
        self, pinst: vlsir.circuit.Instance, rmodule: ResolvedModule,
    ) -> None:
        """ Write a voltage-source instance `pinst`.
        Throws an Exception if `rmodule` is not a known voltage-source type. """

        # Resolve its parameter values
        resolved_param_values = self.get_instance_params(pinst, rmodule.module)

        # Write the instance name
        self.write_instance_name(pinst, rmodule)

        # Write its port-connections
        self.write_instance_conns(pinst, rmodule.module)

        # Handle each of the voltage-source cases
        name = rmodule.module.name.name
        if name == "vdc":
            dc = resolved_param_values.pop("dc")
            self.write(f"+ dc {dc} \n")

        elif name == "vpulse":
            keys = ["v1", "v2", "td", "tr", "tf", "tpw", "tper"]
            pp = resolved_param_values.pop_many(keys)
            self.write(f"+ pulse (" + " ".join([pp[k] for k in keys]) + ") \n")

        elif name == "vsin":
            keys = ["voff", "vamp", "freq", "td", "phase"]
            pp = resolved_param_values.pop_many(keys)
            self.write(f"+ sin (" + " ".join([pp[k] for k in keys]) + ") \n")

        else:
            raise ValueError(f"Invalid or unsupported voltage-source type: {name}")

        # Now! Write its subckt-style by-name parameter values
        self.write_instance_params(resolved_param_values)

        # Add a blank after each instance
        self.write("\n")

    def write_instance_conns(
        self, pinst: vlsir.circuit.Instance, module: ModuleLike
    ) -> None:
        """ Write the port-connections for Instance `pinst` """

        # Write a quick comment for port-less modules
        if not module.ports:
            self.write("+ ")
            return self.write_comment("No ports")

        self.write("+ ")
        # And write the Instance ports, in that order
        port_order = [pport.signal.name for pport in module.ports]
        for pname in port_order:
            pconn = pinst.connections.get(pname, None)
            if pconn is None:
                raise RuntimeError(f"Unconnected Port {pname} on {pinst.name}")
            self.write(self.format_connection(pconn) + " ")
        self.write("\n")

    def write_instance_params(self, pvals: ResolvedParams) -> None:
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
            self.write_comment("No parameters")

        # And write them
        for (pname, pval) in pvals.items():
            self.write(f"{pname}={self.format_expression(pval)} ")

        self.write("\n")

    def format_concat(self, pconc: vlsir.circuit.Concat) -> str:
        """ Format the Concatenation of several other Connections """
        out = ""
        for part in pconc.parts:
            out += self.format_connection(part) + " "
        return out

    @classmethod
    def format_port_decl(cls, pport: vlsir.circuit.Port) -> str:
        """ Get a netlist `Port` definition """
        return cls.format_signal_ref(pport.signal)

    @classmethod
    def format_port_ref(cls, pport: vlsir.circuit.Port) -> str:
        """ Get a netlist `Port` reference """
        return cls.format_signal_ref(pport.signal)

    @classmethod
    def format_signal_ref(cls, psig: vlsir.circuit.Signal) -> str:
        """ Get a netlist definition for Signal `psig` """
        if psig.width < 1:
            raise RuntimeError
        if psig.width == 1:  # width==1, i.e. a scalar signal
            return psig.name
        # Vector/ multi "bit" Signal. Creates several spice signals.
        return " ".join(
            [f"{psig.name}{cls.format_bus_bit(k)}" for k in reversed(range(psig.width))]
        )

    @classmethod
    def format_signal_slice(cls, pslice: vlsir.circuit.Slice) -> str:
        """ Get a netlist definition for Signal-Slice `pslice` """
        base = pslice.signal
        indices = list(reversed(range(pslice.bot, pslice.top + 1)))
        if not len(indices):
            raise RuntimeError(f"Attempting to netlist empty slice {pslice}")
        return " ".join([f"{base}{cls.format_bus_bit(k)}" for k in indices])

    @classmethod
    def format_bus_bit(cls, index: Union[int, str]) -> str:
        """ Format-specific string-representation of a bus bit-index """
        # Spectre netlisting uses an underscore prefix, e.g. `bus_0`
        return "_" + str(index)

    @classmethod
    def format_param_decl(cls, name: str, param: vlsir.circuit.Parameter) -> str:
        """ Format a parameter-declaration """
        default = cls.get_param_default(param)
        if default is None:
            msg = f"Invalid non-default parameter {param} for Spice netlisting"
            raise RuntimeError(msg)
        return f"{name}={default}"

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

    def write_param_declarations(self, module: vlsir.circuit.Module) -> None:
        """ Write the parameter declarations for Module `module`. 
        Parameter declaration format:
        .SUBCKT <name> <ports> 
        + PARAMS: name1=val1 name2=val2 name3=val3 \n
        """
        self.write("+ PARAMS: ")  # <= Xyce-specific
        for name, pparam in module.parameters.items():
            self.write(self.format_param_decl(name, pparam))
        self.write("\n")

    def write_instance_params(self, pvals: Dict[str, str]) -> None:
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
        for (pname, pval) in pvals.items():
            self.write(f"{pname}={self.format_expression(pval)} ")

        self.write("\n")

    def write_comment(self, comment: str) -> None:
        """ Xyce comments *kinda* support the Spice-typical `*` charater, 
        but *only* as the first character in a line. 
        Any mid-line-starting comments must use `;` instead. 
        So, just use it all the time. """
        self.write(f"; {comment}\n")

    def format_expression(self, expr: str) -> str:
        # Xyce expressions are wrapped in curly braces.
        return f"{{{expr}}}"


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
