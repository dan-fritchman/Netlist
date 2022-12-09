"""
# Abstract to Concrete Syntax Tree (AST to CST) Conversion 
"""

# Std-Lib Imports
from copy import copy
from pathlib import Path
from enum import Enum, auto
from dataclasses import field
from typing import Optional, Union, List, Dict, Generic, TypeVar, Sequence, Set

# PyPi Imports
from pydantic.dataclasses import dataclass
from pydantic.generics import GenericModel

# Local Imports
from .data import *


class ScopeType(Enum):
    """Enumerated Scope-Types
    Serves as an annotation for each nested scope."""

    ROOT = auto()
    SUBCKT_DEF = auto()
    LIB_SECTION = auto()


CURRENT_SCOPE_ID = 0


@dataclass(frozen=True)
class ScopeId:
    """Integer scope identifier.
    Incremented with each call to `next`."""

    num: int

    @classmethod
    def next(cls) -> "ScopeId":
        global CURRENT_SCOPE_ID
        rv = ScopeId(CURRENT_SCOPE_ID)
        CURRENT_SCOPE_ID += 1
        return rv


DataT = TypeVar("DataT")


class NoRedefDict(GenericModel, Generic[DataT]):
    """A strongly typed Dictionary which throws an error if one overwrites any of its keys.
    The key-type is always `str`, and the value-types are `DataT`."""

    data: Dict[str, DataT]

    @classmethod
    def new(cls) -> "NoRedefDict[DataT]":
        return cls(data=dict())

    def set(self, key: str, val: DataT) -> None:
        if key in self.data:
            raise RedefinitionError
        self.data[key] = val

    def __setitem__(self, key: str, val: DataT) -> None:
        return self.set(key, val)

    def get(self, key: str) -> Optional[DataT]:
        return self.data.get(key, None)

    def __getitem__(self, key: str) -> DataT:
        val = self.get(key)
        if val is None:
            raise KeyError
        return val

    def merge(self, other: "NoRedefDict[DataT]") -> None:
        for (key, val) in other.data.items():
            self.set(key, val)

    def values(self):
        return self.data.values()

    def __contains__(self, key: str) -> bool:
        return key in self.data


class RedefinitionError(Exception):
    """Error multiply defining a same-type entity in the same scope."""

    ...  #


# Union AST types which can't really be referred to by others.
# NOTE: some of these can *refer* to other AST nodes, such as an `Option` set by a `ParamVal`.
Unreferable = Union[Unknown, Options, StatisticsBlock, End]

# Shorthand for the default value of many fields
_f = lambda: field(default_factory=NoRedefDict.new)


@dataclass
class Scope:
    """Scoped AST Entries

    AST to CST conversion occurs in two steps: AST nodes are gathered up by name into scopes,
    and then resolved to what they mean. `Scope`s are the intermediate data,
    after which we have organized by type, but have not resolved referents.
    """

    sid: ScopeId  # Scope ID Number
    stype: ScopeType  # Enumerated Scope-Type

    # Parent Scope. Our one required parameter.
    parent: Optional["Scope"]

    # Name of the paired netlist entity, e.g. subckt or section.
    # `None` for the root/ program scope.
    name: Optional[str] = None
    # Child Scopes, keyed by `name`
    children: Dict[str, "Scope"] = field(default_factory=dict)

    # Contents defined in the source of this scope
    # Parameters
    params: NoRedefDict[ParamDecl] = _f()
    # Subcircuit Definitions
    subckts: NoRedefDict[SubcktDef] = _f()
    # Model Definitions
    models: NoRedefDict[Union[ModelDef, ModelFamily]] = _f()
    # Function Definitions
    functions: NoRedefDict[FunctionDef] = _f()
    # Instances of Primitive elements
    primitive_instances: NoRedefDict[Primitive] = _f()
    # Instances of sub-circuits
    # Note some of these *can* still resolve to `Primitive`s at this point.
    subckt_instances: NoRedefDict[Instance] = _f()
    # Union of all primitives & instances
    # These generally share a namespace and cannot conflict (unlike, say, instance and param).
    # This merged `NoRedefDict` ensures there are non Instances and Primitives with the same names.
    all_instances: NoRedefDict[Union[Instance, Primitive]] = _f()
    # Library Section Definitions
    sections: NoRedefDict[LibSection] = _f()
    # Entries with no keys, not referable-to by other entries
    others: List[Unreferable] = field(default_factory=list)
    # External References
    external_refs: List[ExternalRef] = field(default_factory=list)

    @classmethod
    def root(cls) -> "Scope":
        """Create a root file scope"""
        return Scope(parent=None, stype=ScopeType.ROOT, sid=ScopeId.next())

    def child(self, stype: ScopeType, name: str) -> "Scope":
        """Create a new and initially empty child scope.
        Add it to our `children` list along the way."""
        child_scope = Scope(parent=self, stype=stype, name=name, sid=ScopeId.next())
        self.children[name] = child_scope
        return child_scope

    # def merge(self, other: "Scope") -> None:
    #     """Merge the content of another `Scope` into our own.
    #     Content is added as "peers"; parents and children remain unmodified."""

    #     self.params.merge(other.params)
    #     self.subckts.merge(other.subckts)
    #     self.models.merge(other.models)
    #     self.functions.merge(other.functions)
    #     self.instances.merge(other.instances)
    #     self.sections.merge(other.sections)

    #     self.others.extend(other.sections)


# @dataclass
# class MergedScope:
#     """A marker for files that are collected,
#     but into another file's scope instead of one of their own."""

#     sid: ScopeId


class ScopeCollector:
    """AST to CST ScopeCollector"""

    def __init__(self, ast: Program):
        self.ast_program = ast
        # Create a mapping from (absolute) `Path` to `SourceFile`, for Include/ lib lookups
        self.paths_to_sourcefiles: Dict[Path, SourceFile] = {
            f.path: f for f in ast.files
        }

        # Initialize our scopes
        self.root_scope: Scope = Scope.root()
        self.scope: Scope = self.root_scope
        self.pending: Set[Path] = set()
        self.done: Set[Path] = set()

    def push_scope(self, stype: ScopeType, name: str):
        """Push a new child scope onto our stack of them."""
        self.scope = self.scope.child(stype, name)

    def pop_scope(self) -> Scope:
        """Pop up a level of scope-stack, returning the popped element.
        Raises an Exception if the current scope is a root, i.e. it has no parent."""
        if self.scope.parent is None:
            raise RuntimeError(f"Cannot pop from top-level scope {self.scope}")
        prior = self.scope
        self.scope = self.scope.parent
        return prior

    def collect(self):
        """Collect `self.ast_program`'s content into `self.scopes`."""

        for source_file in self.ast_program.files:
            self.collect_source_file(source_file)

        # Check that we've gotten back up to the "no current scope" state, or something went wrong
        if self.scope is not self.root_scope:
            raise RuntimeError(f"Error collecting {self}")

    def collect_source_file(self, source_file: SourceFile):
        """Collect an `SourceFile` into the current AST scope."""

        # FIXME: would we ever hit one of these more than once...?
        # Maybe get rid of this pending/ done check
        if source_file.path in self.done:
            raise RuntimeError(f"Doubly included file {source_file}")
        if source_file.path in self.pending:
            raise RuntimeError(f"Hit pending file {source_file}")

        self.pending.add(source_file.path)
        self.collect_entries(source_file.contents)
        self.pending.remove(source_file.path)
        self.done.add(source_file.path)

    def collect_entries(self, entries: Sequence[Entry]):
        """
        Collect a list of `Entry`s into the current AST scope.

        Entries come in a few flavors:
        * Compound entries which create new child scopes, e.g. sub-circuit definitions
        * Compound entries which *add to the current scope*, e.g. file inclusions
        * Simpler "scalar" entries which are added to the definitions of the active scope
        * "Unreferable" entries, which others can't get references to, which are added to the un-keyed lists of the current scope.
        """

        UNSUPPORTED_AST_ENTRIES = (
            DialectChange,
            AhdlInclude,
            Library,
        )

        # We collect any loose `ModelVariant`s into a `ModelFamily` here.
        # Could this happen somewhere better? Maybe. Sure.
        new_model_families = dict()

        for entry in entries:

            if entry in UNSUPPORTED_AST_ENTRIES:
                raise RuntimeError(f"Invalid AST entry {entry}")

            # Compound entries which create new child scopes
            if isinstance(entry, SubcktDef):
                # Create a child scope
                self.push_scope(stype=ScopeType.SUBCKT_DEF, name=entry.name.name)
                # Collect its entries
                self.collect_entries(entry.entries)
                # And pop back up to the current scope
                subscope = self.pop_scope()
                # And add the entry to the current scope
                self.scope.subckts.set(entry.name.name, entry)

            elif isinstance(entry, LibSection):
                # Create a child scope
                self.push_scope(
                    stype=ScopeType.LIB_SECTION, name=entry.section.name.name
                )
                # Collect its entries
                self.collect_entries(entry.entries)
                # And pop back up to the current scope
                subscope = self.pop_scope()
                # And add the entry to the current scope
                self.scope.sections.set(entry.name.name, entry)

            # Compound entries, adding on to the current scope
            elif isinstance(entry, (Include, UseLib)):
                # Roll a `section` of library `entry` into the current scope
                libfile: Optional[SourceFile] = self.paths_to_sourcefiles.get(
                    entry.path, None
                )
                if libfile is None:
                    # Not defined, create an External Ref
                    eref = ExternalRef(Ident(str(entry.path)), types=[RefType.FILEPATH])
                    self.scope.external_refs.append(eref)
                    self.external_refs.append(eref)

                elif isinstance(entry, Include):
                    # Include the whole file-contents into the current scope
                    self.collect_source_file(libfile)

                else:  # isinstance(entry, UseLib)
                    # File found, now find section `entry.section.name`
                    section_list: List[LibSection] = [
                        e for e in libfile.entries if isinstance(e, LibSection)
                    ]
                    section_map: Dict[str, LibSection] = {
                        s.name.name: s for s in section_list
                    }
                    if entry.section.name not in section_map:
                        raise RuntimeError(
                            f"Invalid section {entry.section.name} in file {entry.path}"
                        )
                    # Alright now we've got it. Grab the section and add its content to the current scope.
                    section = section_map[entry.section.name]
                    self.collect_entries(section.entries)

            # Scalar types, added to the current scope
            elif isinstance(entry, Instance):
                self.scope.subckt_instances.set(entry.name.name, entry)
                self.scope.all_instances.set(entry.name.name, entry)
            elif isinstance(entry, Primitive):
                self.scope.primitive_instances.set(entry.name.name, entry)
                self.scope.all_instances.set(entry.name.name, entry)

            elif isinstance(entry, ParamDecl):
                self.scope.params.set(entry.name.name, entry)

            elif isinstance(entry, ParamDecls):
                for param in entry.params:
                    self.scope.params.set(param.name.name, entry)

            elif isinstance(entry, FunctionDef):
                self.scope.functions.set(entry.name.name, entry)

            elif isinstance(entry, (ModelDef, ModelFamily)):
                self.scope.models.set(entry.name.name, entry)

            elif isinstance(entry, ModelVariant):
                # Encountered a model variant, i.e. `nmos.0`.
                # Either merge it into an existing `ModelFamily`, or create a new one.
                family = new_model_families.get(entry.name.name, None)
                if family is None:
                    # Create the `ModelFamily` here and now
                    family = ModelFamily(
                        name=entry.model,
                        mtype=entry.mtype,
                        variants=list(),
                    )
                    new_model_families[entry.model.name] = family
                family.variants.append(entry)

            # "Un-refer-able" entries, kept in the `others` list of the current scope.
            elif isinstance(entry, (Unknown, Options, StatisticsBlock, End)):
                self.scope.others.append(entry)

        # Now that `entries` are done, check and add any newly created model-families
        for family in new_model_families:
            self.scope.models.set(family.name.name, family)


class RefResolver:
    """Resolve all references in `root_scope` and its children."""

    def __init__(self, root_scope: Scope):
        self.root_scope = root_scope
        self.scope = root_scope
        # Combined list of all external refs encountered in all scopes
        self.external_refs: List[ExternalRef] = list()

    def resolve(self) -> None:
        """Resolve all of our AST scopes into our CST."""
        self.resolve_scope(self.root_scope)

    def resolve_scope(self, scope: Scope) -> None:
        """Resolve a single scope."""

        for param_decl in scope.params.values():
            self.resolve_param_decl(param_decl, scope)
        for function in scope.functions.values():
            self.resolve_function(function, scope)
        for model in scope.models.values():
            self.resolve_model(model, scope)

        for subckt_def in scope.subckts.values():
            # Look up the subckt's paired `Scope`
            subckt_scope = scope.children.get(subckt_def.name.name, None)
            if subckt_scope is None:
                raise RuntimeError(f"Invalid subckt with no scope {subckt_def}")
            # Recursively resolve everything in its scope
            self.resolve_scope(subckt_scope)

        for section in scope.sections.values():
            # Look up the sections's paired `Scope`
            section_scope = scope.children.get(section.name.name, None)
            if section_scope is None:
                raise RuntimeError(f"Invalid section with no scope {section}")
            # Recursively resolve everything in its scope
            self.resolve_scope(section_scope)

        # The final, real content: instances of sub-circuits and primitives
        for instance in scope.subckt_instances.values():
            self.resolve_subckt_instance(inst=instance, scope=scope)
        for prim in scope.primitive_instances.values():
            self.resolve_primitive_instance(prim=prim, scope=scope)

    def resolve_subckt_instance(self, inst: Instance, scope: Scope):
        """Resolve an AST Instance to a CST one, particularly resolving its target Subckt or Primitive and parameter values.
        Note that AST instances *can* be turned into primitives.
        This is particularly common when parsing Spectre format, for which primitives look more like a
        standard library of subcircuits. The AST level does not differentiate between the two."""

        # FIXME: probably add some primitive vs subckt stuff here, particularly for Spectre.
        search_result = self.search(
            scope=scope,
            key=inst.module.ident.name,
            types=[RefType.SUBCKT, RefType.MODEL],
        )
        if isinstance(search_result, ExternalRef):
            print(f"External/ undefined Subckt ref {search_result}")
        elif isinstance(search_result.resolved, Model):
            print(f"Model instance ref {search_result}")
        inst.module = search_result

        # And resolve each instance-parameter value
        for param_val in inst.params:
            self.resolve_expr(param_val.val, scope)

    def resolve_primitive_instance(self, prim: Primitive, scope: Scope):
        """Resolve a primitive instance. Particularly its parameters."""

        for arg in prim.args:
            self.resolve_expr(arg)
        for param_val in prim.kwargs:
            self.resolve_expr(param_val.val)

    def resolve_model(self, model: Model, scope: Scope):
        """Resolve a model definition or family thereof."""

        if isinstance(model, ModelDef):
            return self.resolve_model_def_or_variant(model, scope)
        elif isinstance(model, ModelFamily):
            for variant in model.variants:
                self.resolve_model_def_or_variant(variant, scope)
        else:
            raise TypeError(f"Invalid Model {model}")

    def resolve_model_def_or_variant(
        self, model: Union[ModelDef, ModelVariant], scope: Scope
    ):
        """Resolve either a `ModelDef` or `ModelVariant`."""
        # This works because the two have the same fields, except for the `variant` identifier.
        # FIXME: `args` are just identifiers. Should they be expressions and get resolved instead?
        # for arg in model.args:
        #     self.resolve_expr(arg)
        # FIXME: shouldn't these be `ParamVal`?
        # for param_decl in model.params:
        #     self.resolve_param_decl(param_decl)
        raise NotImplementedError

    def resolve_function(self, func: FunctionDef, scope: Scope):
        """Resolve a function definition, particularly any "closure" references to unfree parameters.
        Note function support is very limited to a single "return statement", which can be any `Expr`."""
        if len(func.statements) != 1:
            msg = f"Unsupported function {func}. Only one `return` line supported."
            raise RuntimeError(msg)

        return_stmt = func.statements[0]
        self.resolve_expr(return_stmt.val, scope)

    def resolve_param_decl(self, param_decl: ParamDecl, scope: Scope):
        """Resolve a parameter declaration - particularly its `default` value expression."""
        # The `None` default value indicates "no default", and nothing to resolve.
        if param_decl.default is not None:
            self.resolve_expr(param_decl.default, scope)

    def resolve_expr(self, expr: Expr, scope: Scope):
        """Resolve an `Expr` expression. Largely dispatches across the `Expr` type-union."""

        # The `Expr` type union:
        # Expr = Union[UnaryOp, BinaryOp, TernOp, Int, Float, MetricNum, Ref, Call]

        if isinstance(expr, Int, Float, MetricNum):
            return  # Literals. Nothing to resolve. 

        if isinstance(expr, Ref):
            return self.resolve_expr_ref(expr, scope)

        if isinstance(expr, Call):
            for arg in expr.args:
                self.resolve_expr(arg, scope)
            return self.resolve_function_ref(expr.func, scope)

        if isinstance(expr, UnaryOp):
            return self.resolve_expr(expr.targ, scope)

        if isinstance(expr, BinaryOp):
            self.resolve_expr(expr.left, scope)
            return self.resolve_expr(expr.right, scope)

        if isinstance(expr, TernOp):
            self.resolve_expr(expr.cond, scope)
            self.resolve_expr(expr.if_true, scope)
            return self.resolve_expr(expr.if_false, scope)

        raise TypeError(f"Invalid Expr {expr}")

    def resolve_expr_ref(self, ref: Ref, scope: Scope):
        """Resolve a parameter-identifier reference in an expression"""
        if ref.resolved is not None:
            return  # Already done
        ref.resolved = self.search(
            scope=scope, key=ref.ident.name, types=[RefType.PARAM]
        )

    def resolve_function_ref(self, ref: Ref[FunctionDef], scope: Scope):
        """Resolve a function-reference to a function definition, or external reference."""
        if ref.resolved is not None:
            return  # Already done
        ref.resolved = self.search(
            scope=scope, key=ref.ident.name, types=[RefType.FUNCTION]
        )

    def search(
        self, scope: Scope, key: str, types: Sequence[RefType]
    ) -> Union[Ref, ExternalRef]:
        """Hierarchically search scopes for `key`, starting from `scope`.
        Returns an `ExternalRef` if `key` is not found in any scope."""

        while scope is not None:
            dicts = ast_dicts(scope, types)
            for d in dicts:
                if key in d:
                    # Hit! Return a new Ref, resolved to `d[key]`.
                    return Ref(ident=Ident(key), resolved=d[key])

            # Didn't hit, jump up one level, check the parent scope
            scope = scope.parent

        # Getting here means we ran out of scopes without ever finding `key`.
        # Create, return, and store an `ExternalRef`.
        eref = ExternalRef(Ident(key), types=types)
        scope.external_refs.append(eref)
        self.external_refs.append(eref)
        return eref


def ast_dicts(scope: Scope, types: Sequence[RefType]) -> List[NoRedefDict]:
    """Get the dictionaries corresponding to `types` in `scope`"""

    dicts = {
        RefType.SUBCKT: scope.subckts,
        RefType.MODEL: scope.models,
        RefType.FUNCTION: scope.functions,
        RefType.PARAM: scope.params,
    }
    if any(tp not in dicts for tp in types):
        raise ValueError(f"Invalid AST dictionary types {types}")

    return [dicts[tp] for tp in types]


def get_external_refs(scope: Scope) -> List[ExternalRef]:
    """Get all `ExternalRef`s in `scope`, including in all child scopes."""

    def helper(scope: Scope, refs: List[ExternalRef]):
        # Recursive inner helper
        refs.extend(copy(scope.external_refs))
        for child in scope.children.values():
            helper(child, refs)

    # Kick off our recursive helper with an empty list
    rv = list()
    helper(scope, rv)
    return rv


def has_external_refs(scope: Scope) -> bool:
    """Boolean indication of whether `scope` requires anything external"""
    erefs = get_external_refs(scope)
    return len(erefs) != 0


def ast_to_cst(ast_program: Program) -> Scope:
    """
    # "Concretize" a netlist AST Program

    Primarily this consists of "linking" every `Ref` in the input `Program`.
    Instances are bound to Subckts, Param expressions are bound to their dependents, etc.
    Returns the "root" `Scope` for the netlist program, consisting of everything
    available at its top-level, and nested definitions in child `Scope`s.
    """

    # Step 1: collect into `Scope`s
    collector = ScopeCollector(ast_program)
    collector.collect()

    # Step 2: resolve all the references between elements,
    resolver = RefResolver(collector.root_scope)
    resolver.resolve()
    return resolver.root_scope
