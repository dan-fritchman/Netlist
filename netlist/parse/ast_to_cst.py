"""
# Abstract to Concrete Syntax Tree (AST to CST) Conversion 
"""

# Std-Lib Imports
from copy import copy
from enum import Enum, auto
from pathlib import Path
from typing import (
    Optional,
    Union,
    List,
    Dict,
    Generic,
    TypeVar,
    Sequence,
    Tuple,
    Any,
    Set,
)
from dataclasses import field

# PyPi Imports
from pydantic.dataclasses import dataclass
from pydantic.generics import GenericModel

# Local Imports
from ..data import ast, cst


def ast_to_cst(ast: ast.Program) -> cst.Program:
    """Perform all the necessary resolution to convert an `ast.Program` to a `cst.Program`."""
    return Converter(ast).convert()


class ScopeType(Enum):
    """Enumerated Scope-Types
    Serves as an annotation for each nested scope."""

    ROOT = auto()
    SUBCKT_DEF = auto()
    LIB_SECTION = auto()


CURRENT_SCOPE_ID = 0


@dataclass
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


class RedefinitionError(Exception):
    """Error multiply defining a same-type entity in the same scope."""

    ...  #


# Union AST types which can't really be referred to by others.
# NOTE: some of these can *refer* to other AST nodes, such as an `Option` set by a `ParamVal`.
Unreferable = Union[ast.Unknown, ast.Options, ast.StatisticsBlock, ast.End]


@dataclass
class AstScope:
    """Scoped AST Entries

    AST to CST conversion occurs in two steps: AST nodes are gathered up by name into scopes,
    and then resolved to what they mean. `AstScope`s are the intermediate data,
    after which we have organized by type, but have not resolved referents.

    Note all fields refer to `ast.*` datatypes."""

    sid: ScopeId  # Scope ID Number
    stype: ScopeType  # Enumerated Scope-Type

    parent: Optional["AstScope"]  # Parent Scope. Our one required parameter.
    children: List["AstScope"] = field(default_factory=list)  # Child Scopes

    # Contents defined in the source of this scope
    # Parameters
    params: NoRedefDict[ast.ParamDecl] = field(default_factory=NoRedefDict.new)
    # Subcircuit Definitions
    subckts: NoRedefDict[ast.SubcktDef] = field(default_factory=NoRedefDict.new)
    # Model Definitions
    models: NoRedefDict[Union[ast.ModelDef, ast.ModelFamily]] = field(
        default_factory=NoRedefDict.new
    )
    # Function Definitions
    functions: NoRedefDict[ast.FunctionDef] = field(default_factory=NoRedefDict.new)
    # Instances.
    # Note these are not determined to be `Primitive` or `Subckt` for the CST at this point.
    instances: NoRedefDict[Union[ast.Instance, ast.Primitive]] = field(
        default_factory=NoRedefDict.new
    )
    # Library Section Definitions
    sections: NoRedefDict[ast.LibSection] = field(default_factory=NoRedefDict.new)

    # Entries with no keys, not referable-to by other entries
    others: List[Unreferable] = field(default_factory=list)

    @classmethod
    def root(cls) -> "AstScope":
        """Create a root file scope"""
        return AstScope(parent=None, stype=ScopeType.ROOT, sid=ScopeId.next())

    def child(self, stype: ScopeType) -> "AstScope":
        """Create a new and initially empty child scope.
        Add it to our `children` list along the way."""
        child_scope = AstScope(parent=self, stype=stype, sid=ScopeId.next())
        self.children.append(child_scope)
        return child_scope

    def merge(self, other: "AstScope") -> None:
        """Merge the content of another `AstScope` into our own.
        Content is added as "peers"; parents and children remain unmodified."""

        self.params.merge(other.params)
        self.subckts.merge(other.subckts)
        self.models.merge(other.models)
        self.functions.merge(other.functions)
        self.instances.merge(other.instances)
        self.sections.merge(other.sections)

        self.others.extend(other.sections)


@dataclass
class MergedScope:
    """A marker for files that are collected,
    but into another file's scope instead of one of their own."""

    sid: ScopeId


class Converter:
    """AST to CST Converter"""

    def __init__(self, ast: ast.Program):
        self.ast_program = ast

        # Initialize our AST scopes
        self.ast_root_scope: AstScope = AstScope.root()
        self.ast_scope: AstScope = self.ast_root_scope
        self.ast_files = {str(file.path): file for file in self.ast_program.files}
        self.ast_file_scopes: Dict[str, Union[AstScope, MergedScope]] = dict()
        self.pending: Set[str] = set()
        self.done: Set[str] = set()

        # And initialize our eventual output CST
        self.cst = None

    def convert(self) -> cst.Program:
        """Primary conversion method"""
        # Done in two steps:
        self.collect()  # Collect into nested `AstScope`s
        self.resolve()  # Resolve all the references between them
        return self.cst

    def collect(self):
        """Conversion step 1 of 2: collect `self.ast`'s content into `AstScope`s in `self.ast_scopes`."""
        # FIXME: why would we ever hit one of these more than once...?
        # for path, source_file in self.ast_files.items():
        #     if path not in self.ast_file_scopes:
        #         self.collect_source_file(source_file)

        for source_file in self.ast_program.files:
            source_file_path = str(source_file.path)
            if source_file_path in self.pending or source_file_path in self.done:
                raise RuntimeError(f"Hit pending file {source_file}")
            self.pending.add(source_file_path)
            self.collect_source_file(source_file)
            self.pending.remove(source_file_path)
            self.done.add(source_file_path)

        # Check that we've gotten back up to the "no current scope" state, or something went wrong
        if self.ast_scope is not self.ast_root_scope:
            raise RuntimeError(f"Error collecting {self}")

    UNSUPPORTED_AST_ENTRIES = (
        ast.DialectChange,
        ast.AhdlInclude,
        ast.Library,
    )

    def collect_source_file(self, source_file: ast.SourceFile):
        """Collect an `ast.SourceFile` into the current AST scope."""
        # self.ast_scope = self.ast_file_scopes[str(source_file.path)] = AstScope.root()
        self.collect_entries(source_file.contents)
        # self.pop_scope()

    def push_scope(self, stype: ScopeType):
        """Push a new child scope onto our "stack" of them."""
        if self.ast_scope is None:
            raise RuntimeError(f"Cannot create child scope, not in an existing scope")
        self.ast_scope = self.ast_scope.child(stype)

    def pop_scope(self) -> AstScope:
        """Pop up a level of scope-stack, returning the popped element.
        Raises an Exception if the current scope is a root, i.e. it has no parent."""
        old_scope = self.ast_scope
        # FIXME: whether to check for this?
        # if old_scope.parent is None:
        #     raise TabError
        self.ast_scope = self.ast_scope.parent
        return old_scope

    def collect_entries(self, entries: Sequence[ast.Entry]):
        """
        Collect a list of `ast.Entry`s into the current AST scope.

        Entries come in a few flavors:
        * Compound entries which create new child scopes, e.g. sub-circuit definitions
        * Compound entries which *add to the current scope*, e.g. file inclusions
        * Simpler "scalar" entries which are added to the definitions of the active scope
        * "Unreferable" entries, which others can't get references to, which are added to the un-keyed lists of the current scope.
        """

        # We collect any loose `ModelVariant`s into a `ModelFamily` here.
        # Could this happen somewhere better? Maybe. Sure.
        new_model_families = dict()

        for entry in entries:

            if entry in self.UNSUPPORTED_AST_ENTRIES:
                raise RuntimeError(f"Invalid AST entry {entry}")

            # Compound entries which create new child scopes
            if isinstance(entry, ast.SubcktDef):
                # Create a child scope
                self.push_scope(stype=ScopeType.SUBCKT_DEF)
                # Collect its entries
                self.collect_entries(entry.entries)
                # And pop back up to the current scope
                subscope = self.pop_scope()
                # And add the entry to the current scope
                self.ast_scope.subckts.set(entry.name.name, entry)

            elif isinstance(entry, ast.LibSection):
                # Create a child scope
                self.push_scope(stype=ScopeType.LIB_SECTION)
                # Collect its entries
                self.collect_entries(entry.entries)
                # And pop back up to the current scope
                subscope = self.pop_scope()
                # And add the entry to the current scope
                self.ast_scope.sections.set(entry.name.name, entry)

            # Compound entries, adding on to the current scope
            elif isinstance(entry, (ast.Include, ast.UseLib)):
                raise TabError("NO INCLUDES FOR YOU")
                # Get the file content scoped up into `self.ast_file_scopes`
                path = str(entry.path)
                source_file = self.ast_files.get(path, None)
                if source_file is None:
                    raise TabError
                file_scope = self.ast_file_scopes.get(path, None)
                if file_scope is None:
                    # Not collected yet. Do it now.
                    old_scope = self.ast_scope
                    file_scope = self.ast_file_scopes[path] = AstScope.root()
                    self.collect_entries(source_file.contents)
                    self.ast_scope = old_scope

                # At this point we've collected the included-file as `file_scope`.

                if isinstance(entry, ast.Include):
                    # Merge the full file into the current scope
                    scope_to_merge = file_scope
                    if isinstance(scope_to_merge, MergedScope):
                        # We've doubly included this file somewhere. Error time.
                        raise TabError  # FIXME! a slightly better error.
                    self.ast_file_scopes[path] = MergedScope(scope_to_merge.sid)
                elif isinstance(entry, ast.UseLib):
                    scope_to_merge = file_scope.section_scopes[entry.section.name]

                # And finally merge it all into the current scope
                self.ast_scope.merge(scope_to_merge)

            # Scalar types, added to the current scope
            elif isinstance(entry, (ast.Instance, ast.Primitive)):
                self.ast_scope.instances.set(entry.name.name, entry)
            elif isinstance(entry, ast.ParamDecls):
                for param in entry.params:
                    raise TabError("TYPO THERE I SEE")
                    self.ast_scope.params.set(entry.name.name, entry)
            elif isinstance(
                entry,
                (
                    ast.ModelDef,
                    ast.ModelFamily,
                ),
            ):
                self.ast_scope.models.set(entry.name.name, entry)
            elif isinstance(entry, ast.ModelVariant):
                family = new_model_families.get(entry.name.name, None)
                if family is None:
                    # Create the `ModelFamily` here and now
                    family = ast.ModelFamily(
                        name=entry.model,
                        mtype=entry.mtype,
                        variants=list(),
                    )
                    new_model_families[entry.model.name] = family
                family.variants.append(entry)
            elif isinstance(entry, ast.FunctionDef):
                self.ast_scope.functions.set(entry.name.name, entry)

            # "Un-refer-able" entries, kept in the `others` list of the current scope.
            elif isinstance(
                entry, (ast.Unknown, ast.Options, ast.StatisticsBlock, ast.End)
            ):
                self.ast_scope.others.append(entry)

        # Now that `entries` are done, check and add any newly created model-families
        for family in new_model_families:
            self.ast_scope.models.set(family.name.name, family)

    def resolve(self):
        Resolver(...)


RT = cst.RefType  # Add a shorthand


class Resolver:
    """Resolve all references in AST scopes"""

    def __init__(self, root_ast_scope: AstScope):
        self.root_ast_scope = root_ast_scope
        self.ast_scope = root_ast_scope
        # self.ast_scopes = {s.sid: s for s in ast_scopes}
        self.cst = cst.Program()
        self.cst_scope = self.cst.scope
        # Create a mapping from AST scope (ids) to CST scopes
        self.ast_to_cst_scopes: Dict[ScopeId : cst.Scope] = {
            root_ast_scope.sid: self.cst_scope
        }

    def resolve(self) -> None:
        """Resolve all of our AST scopes into our CST."""
        self.resolve_scope(self.root_ast_scope)
        # for ast_scope in self.ast_scopes.values():
        #     if ast_scope.sid in self.cst_scopes:
        #         continue
        #     if isinstance(ast_scope, MergedScope):
        #         continue
        #     self.cst_scopes[ast_scope.sid] = Something()  # FIXME!
        #     self.ast_scope = ast_scope
        #     self.resolve_scope()

    def resolve_scope(self, ast_scope: AstScope) -> None:
        """Resolve a single AST scope."""

        for param_decl in ast_scope.params.values():
            self.resolve_param_decl(param_decl)
        for function in ast_scope.functions.values():
            self.resolve_function(function)
        for model in ast_scope.models.values():
            self.resolve_model(model)
        for subckt_def in ast_scope.subckt_defs.values():
            self.resolve_subckt_def(subckt_def)
        for instance in ast_scope.instances.values():
            self.resolve_instance(instance)
        for section in ast_scope.sections.values():
            raise TabError("FIXME: handling of lib-section resolution")
            self.resolve_lib_section(section)
        self.cst_scope.others.extend(copy(ast_scope.others))

    def resolve_param_decl(self, param_decl: ast.ParamDecl):
        # Expr = Union["UnaryOp", "BinaryOp", "TernOp", Int, Float, MetricNum, Ident, Call]
        if param_decl.default is None: 
            raise RuntimeError(f"failure to resolve param")
        if isinstance(param_decl.default, (ast.Int, ast.Float, ast.MetricNum)):
            cst_
        ast_scope_id,  = self.search(key=param_decl.name, types=[RT.PARAM])
        cst_scope = 
        self.cst_scope.params[param_decl.name] = 

    def resolve_instance(
        self, inst: ast.Instance
    ) -> Union[cst.SubcktInstance, cst.PrimitiveInstance]:
        """Resolve an AST Instance to a CST one, particularly resolving its target Subckt or Primitive and parameter values.
        Note that AST instances *can* be turned into primitives.
        This is particularly common when parsing Spectre format, for which primitives look more like a
        standard library of subcircuits. The AST level does not differentiate between the two."""
        target = self.search(key=inst.module.name, types=[RT.SUBCKT])
        if isinstance(target, ast.SubcktDef):
            # where do we get get cst subckt now...
            ...

    def search(self, key: str, types: Sequence[RT]) -> Tuple[ScopeId, Any]:
        """Hierarchically search AST scopes for `key`, starting from the current scope."""
        ast_scope = self.ast_scope
        while True:
            if ast_scope is None:  # Not found. Create an `ExternalRef`.
                eref = cst.ExternalRef(cst.Ident(key), valid_types=types)
                self.external_refs.append(eref)
                self.cst_scope.external_refs.append(eref)
                return eref
            dicts = ast_dicts(ast_scope, types)
            for d in dicts:
                something = d.get(key, None)
                if something is not None:
                    return (ast_scope.sid, something)

            ast_scope = ast_scope.parent


def ast_dicts(scope: AstScope, types: Sequence[RT]) -> List[NoRedefDict]:
    """Get the dictionaries corresponding to `types` in `scope`"""

    dicts = {
        RT.SUBCKT: scope.subckts,
        RT.FUNCTION: scope.functions,
        RT.MODEL: scope.models,
        RT.PARAM: scope.params,
    }
    if any(tp not in dicts for tp in types):
        raise ValueError(f"Invalid AST dictionary types {types}")

    return [dicts[tp] for tp in types]
