#[macro_use]
extern crate serde_derive;
extern crate serde;

use std::collections::{HashMap, HashSet};
use std::mem;
use std::ops::Deref;

pub trait TopologicalVisit {
    /// Visit every item in reverse topological order of the item graph and
    /// call the closure.
    ///
    /// A true topological ordering does not exist in a graph if there is a
    /// cycle, in which case the closure will be called with a list of the
    /// paths in the current item which are part of a cycle. Those referenced
    /// items will have their callback called after the closure, instead of
    /// before it.
    fn visit_items_reverse_topological<F: FnMut(Path, Vec<Vec<Link>>)>(&self, callback: &mut F);
}

impl<'a> TopologicalVisit for &'a [Item] {
    fn visit_items_reverse_topological<F: FnMut(Path, Vec<Vec<Link>>)>(&self, callback: &mut F) {
        let mut context = TopologicalContext::new(self, callback);
        for (i, item) in self.iter().enumerate() {
            item.visit_items_reverse_topological(Path::new(i), LinkKind::Indirect, &mut context);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LinkKind {
    Direct,
    Indirect,
}

#[derive(Clone, Copy, Debug)]
pub struct Link {
    pub path: Path,
    pub kind: LinkKind,
}

struct TopologicalContext<'a, F: FnMut(Path, Vec<Vec<Link>>) + 'a> {
    items: &'a [Item],
    callback: &'a mut F,
    visited: HashSet<Path>,
    processing: Vec<Link>,
    cycle_paths: Vec<Vec<Link>>,
}

impl<'a, F: FnMut(Path, Vec<Vec<Link>>)> TopologicalContext<'a, F> {
    fn new(items: &'a [Item], callback: &'a mut F) -> TopologicalContext<'a, F> {
        TopologicalContext {
            items,
            callback,
            visited: HashSet::new(),
            processing: Vec::new(),
            cycle_paths: Vec::new(),
        }
    }
}

#[serde(rename = "mutability")]
#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutability {
    Const,
    Mut,
}

impl Mutability {
    pub fn is_const(&self) -> bool {
        *self == Mutability::Const
    }

    pub fn is_mutable(&self) -> bool {
        *self != Mutability::Const
    }
}

#[serde(rename = "pointer")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Pointer {
    #[serde(rename = "referenced")]
    pub referenced: Box<Type>,
    pub mutability: Mutability,
}

impl Pointer {
    pub fn new(ty: Type, mutability: Mutability) -> Pointer {
        Pointer {
            referenced: Box::new(ty),
            mutability,
        }
    }

    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.referenced.for_each_path_mut(callback);
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        self.referenced
            .visit_items_reverse_topological(true, context);
    }
}

#[serde(rename = "abi")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub enum Abi {
    Cdecl,
    Stdcall,
    Fastcall,
    Vectorcall,
    Thiscall,
    Win64,
    SysV64,
    C,
    System,
}

#[serde(rename = "function-signature")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct FunctionSignature {
    #[serde(rename = "output")]
    pub output: Box<Type>,
    #[serde(rename = "inputs")]
    pub inputs: Vec<Type>,
    pub abi: Abi,
}

impl FunctionSignature {
    pub fn new(output: Type, inputs: Vec<Type>, abi: Abi) -> FunctionSignature {
        FunctionSignature {
            output: Box::new(output),
            inputs,
            abi,
        }
    }

    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.output.for_each_path_mut(callback);
        for arg in &mut self.inputs {
            arg.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        self.output.visit_items_reverse_topological(true, context);
        for arg in &self.inputs {
            arg.visit_items_reverse_topological(true, context);
        }
    }
}

#[serde(rename = "array")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Array {
    #[serde(rename = "referenced")]
    pub referenced: Box<Type>,
    pub size: usize,
}

impl Array {
    pub fn new(ty: Type, size: usize) -> Array {
        Array {
            referenced: Box::new(ty),
            size,
        }
    }

    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.referenced.for_each_path_mut(callback);
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        indirect: bool,
        context: &mut TopologicalContext<'a, F>,
    ) {
        self.referenced
            .visit_items_reverse_topological(indirect, context);
    }
}

#[serde(rename = "path")]
#[derive(Deserialize, Serialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub index: usize,
}

impl Path {
    pub fn new(index: usize) -> Path {
        Path { index }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        indirect: bool,
        context: &mut TopologicalContext<'a, F>,
    ) {
        let kind = if indirect {
            LinkKind::Indirect
        } else {
            LinkKind::Direct
        };

        context.items[self.index].visit_items_reverse_topological(*self, kind, context);
    }
}

#[serde(rename = "type")]
#[serde(rename_all = "lowercase")]
#[serde(tag = "kind")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub enum Type {
    /// A zero-sized type used when a function returns ()
    Void,

    Bool,
    Char,

    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,

    U8,
    U16,
    U32,
    U64,
    U128,
    USize,

    F32,
    F64,

    #[serde(rename = "Pointer")]
    RawPtr(Pointer),
    #[serde(rename = "ReferencePointer")]
    RefPtr(Pointer),
    #[serde(rename = "FunctionPointer")]
    FnPtr(FunctionSignature),

    #[serde(rename = "Array")]
    Array(Array),

    #[serde(rename = "Path")]
    Path(Path),
}

impl Type {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        match self {
            &mut Type::RawPtr(ref mut pointer) => {
                pointer.for_each_path_mut(callback);
            }
            &mut Type::RefPtr(ref mut pointer) => {
                pointer.for_each_path_mut(callback);
            }
            &mut Type::FnPtr(ref mut signature) => {
                signature.for_each_path_mut(callback);
            }
            &mut Type::Array(ref mut array) => {
                array.for_each_path_mut(callback);
            }
            &mut Type::Path(ref mut path) => {
                callback(path);
            }
            _ => {}
        }
    }

    fn is_sized_path(&self) -> Option<Path> {
        match self {
            &Type::Path(ref path) => Some(*path),
            &Type::Array(ref array) => array.referenced.is_sized_path(),
            _ => None,
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        indirect: bool,
        context: &mut TopologicalContext<'a, F>,
    ) {
        match self {
            &Type::RawPtr(ref pointer) => {
                pointer.visit_items_reverse_topological(context);
            }
            &Type::RefPtr(ref pointer) => {
                pointer.visit_items_reverse_topological(context);
            }
            &Type::FnPtr(ref signature) => {
                signature.visit_items_reverse_topological(context);
            }
            &Type::Array(ref array) => {
                array.visit_items_reverse_topological(indirect, context);
            }
            &Type::Path(ref path) => {
                path.visit_items_reverse_topological(indirect, context);
            }
            _ => {}
        }
    }
}

#[serde(rename = "field")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Field {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: Type,
}

impl Field {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.ty.for_each_path_mut(callback);
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        self.ty.visit_items_reverse_topological(false, context);
    }
}

#[serde(rename = "reason")]
#[serde(rename_all = "lowercase")]
#[serde(tag = "reason")]
#[derive(Deserialize, Serialize, Clone, Debug, Eq, PartialEq)]
pub enum DiscriminantType {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
}

#[serde(rename = "repr")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Repr {
    pub discriminant: Option<DiscriminantType>,
    pub max_align: u32,
    pub min_pack: u32,
    pub transparent: bool,
    pub simd: bool,
}

#[serde(rename = "struct")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Struct {
    pub fields: Vec<Field>,
    pub repr: Repr,
}

impl Struct {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for field in &mut self.fields {
            field.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        for field in &self.fields {
            field.visit_items_reverse_topological(context);
        }
    }
}

#[serde(rename = "union")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Union {
    pub fields: Vec<Field>,
    pub repr: Repr,
}

impl Union {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for field in &mut self.fields {
            field.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        for field in &self.fields {
            field.visit_items_reverse_topological(context);
        }
    }
}

#[serde(rename = "tuple")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Tuple {
    pub fields: Vec<Type>,
    pub repr: Repr,
}

impl Tuple {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for field in &mut self.fields {
            field.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        for field in &self.fields {
            field.visit_items_reverse_topological(false, context);
        }
    }
}

#[serde(rename = "variant-tuple")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct VariantTuple {
    pub fields: Vec<Type>,
}

impl VariantTuple {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for field in &mut self.fields {
            field.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        for field in &self.fields {
            field.visit_items_reverse_topological(false, context);
        }
    }
}

#[serde(rename = "variant-struct")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct VariantStruct {
    pub fields: Vec<Field>,
}

impl VariantStruct {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for field in &mut self.fields {
            field.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        for field in &self.fields {
            field.visit_items_reverse_topological(context);
        }
    }
}

#[serde(rename = "variant-kind")]
#[serde(tag = "kind")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub enum VariantKind {
    #[serde(rename = "Unit")]
    Unit,
    #[serde(rename = "Tuple")]
    Tuple(VariantTuple),
    #[serde(rename = "Struct")]
    Struct(VariantStruct),
}

impl VariantKind {
    pub fn is_unit(&self) -> bool {
        match self {
            &VariantKind::Unit => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            &VariantKind::Tuple(..) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            &VariantKind::Struct(..) => true,
            _ => false,
        }
    }

    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        match self {
            &mut VariantKind::Unit => {}
            &mut VariantKind::Tuple(ref mut tuple) => {
                tuple.for_each_path_mut(callback);
            }
            &mut VariantKind::Struct(ref mut structure) => {
                structure.for_each_path_mut(callback);
            }
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        match self {
            &VariantKind::Unit => {}
            &VariantKind::Tuple(ref tuple) => {
                tuple.visit_items_reverse_topological(context);
            }
            &VariantKind::Struct(ref structure) => {
                structure.visit_items_reverse_topological(context);
            }
        }
    }
}

#[serde(rename = "variant")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Variant {
    pub name: String,
    pub node: VariantKind,
    pub discriminant: isize,
}

impl Variant {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.node.for_each_path_mut(callback);
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        self.node.visit_items_reverse_topological(context);
    }
}

#[serde(rename = "enum")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Enum {
    pub variants: Vec<Variant>,
    pub repr: Repr,
}

impl Enum {
    pub fn new(variants: Vec<Variant>, repr: Repr) -> Enum {
        Enum { variants, repr }
    }

    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for variant in &mut self.variants {
            variant.for_each_path_mut(callback);
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        for variant in &self.variants {
            variant.visit_items_reverse_topological(context);
        }
    }
}

#[serde(rename = "reason")]
#[serde(tag = "reason")]
#[derive(Deserialize, Serialize, Clone, Debug, Eq, PartialEq)]
pub enum Reason {
    NotReprC,
    NotNoMangle,
    ContainsGeneric,
    IsGeneric,
    #[serde(rename_all = "kebab-case")]
    InvalidType {
        invalid_type: String,
    },
    #[serde(rename_all = "kebab-case")]
    InvalidAbi {
        invalid_abi: String,
    },
    #[serde(rename_all = "kebab-case")]
    DirectlyContainsOpaque {
        variant: Option<String>,
        field: Option<String>,
        path: Path,
    },
}

#[serde(rename = "item-kind")]
#[serde(tag = "kind")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub enum ItemKind {
    #[serde(rename = "UnitStruct")]
    Unit,
    #[serde(rename = "TupleStruct")]
    Tuple(Tuple),
    #[serde(rename = "Struct")]
    Struct(Struct),
    #[serde(rename = "Union")]
    Union(Union),
    #[serde(rename = "Enum")]
    Enum(Enum),
    #[serde(rename = "Alias")]
    Alias {
        #[serde(rename = "type")]
        ty: Type,
    },
    #[serde(rename = "Opaque")]
    Opaque(Reason),
}

impl ItemKind {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        match self {
            &mut ItemKind::Unit => {}
            &mut ItemKind::Tuple(ref mut tuple) => {
                tuple.for_each_path_mut(callback);
            }
            &mut ItemKind::Struct(ref mut structure) => {
                structure.for_each_path_mut(callback);
            }
            &mut ItemKind::Union(ref mut union) => {
                union.for_each_path_mut(callback);
            }
            &mut ItemKind::Enum(ref mut enumeration) => {
                enumeration.for_each_path_mut(callback);
            }
            &mut ItemKind::Alias { ref mut ty } => {
                ty.for_each_path_mut(callback);
            }
            &mut ItemKind::Opaque(_) => {}
        }
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        context: &mut TopologicalContext<'a, F>,
    ) {
        match self {
            &ItemKind::Unit => {}
            &ItemKind::Tuple(ref tuple) => {
                tuple.visit_items_reverse_topological(context);
            }
            &ItemKind::Struct(ref structure) => {
                structure.visit_items_reverse_topological(context);
            }
            &ItemKind::Union(ref union) => {
                union.visit_items_reverse_topological(context);
            }
            &ItemKind::Enum(ref enumeration) => {
                enumeration.visit_items_reverse_topological(context);
            }
            &ItemKind::Alias { ref ty } => {
                ty.visit_items_reverse_topological(false, context);
            }
            &ItemKind::Opaque(_) => {}
        }
    }
}

#[serde(rename = "item")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Item {
    pub name: String,
    pub node: ItemKind,
}

impl Item {
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.node.for_each_path_mut(callback);
    }

    fn visit_items_reverse_topological<'a, F: FnMut(Path, Vec<Vec<Link>>)>(
        &self,
        path: Path,
        kind: LinkKind,
        context: &mut TopologicalContext<'a, F>,
    ) {
        if context.visited.contains(&path) {
            return;
        }

        let link = Link { path, kind };

        if let Some(cycle_start) = context.processing.iter().rposition(|x| x.path == path) {
            let mut cycle: Vec<_> = (&context.processing[cycle_start..]).into();
            cycle.push(link);

            assert!(cycle.len() >= 2);
            assert!(cycle[0].path == cycle[cycle.len() - 1].path);
            // The link into this cycle doesn't matter, replace it with the
            // link from the end of the cycle to the beginning
            cycle[0].kind = cycle[cycle.len() - 1].kind;

            context.cycle_paths.push(cycle);
            return;
        }

        context.processing.push(link);

        self.node.visit_items_reverse_topological(context);

        context.processing.pop();
        context.visited.insert(path);

        let cycle_paths = mem::replace(&mut context.cycle_paths, Vec::new());
        (context.callback)(path, cycle_paths);
    }
}

#[serde(rename = "function")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Function {
    pub name: String,
    #[serde(flatten)]
    pub signature: FunctionSignature,
}

impl Function {
    pub fn new(name: String, signature: FunctionSignature) -> Function {
        Function { name, signature }
    }

    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        self.signature.for_each_path_mut(callback);
    }
}

#[serde(rename = "invalid-function")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct InvalidFunction {
    pub name: String,
    pub reason: Reason,
}

impl InvalidFunction {
    pub fn new(name: String, reason: Reason) -> InvalidFunction {
        InvalidFunction { name, reason }
    }
}

#[serde(rename = "crate")]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Crate {
    #[serde(rename = "crate-name")]
    pub crate_name: String,
    pub items: Vec<Item>,
    pub functions: Vec<Function>,
    pub invalid_functions: Vec<InvalidFunction>,
}

impl Crate {
    pub fn new(
        crate_name: String,
        items: Vec<Item>,
        functions: Vec<Function>,
        invalid_functions: Vec<InvalidFunction>,
    ) -> Crate {
        Crate {
            crate_name,
            items,
            functions,
            invalid_functions,
        }
    }

    pub fn validate(&self) -> Result<(), ()> {
        Ok(())
    }

    /// Apply a closure on all paths in all items and functions
    pub fn for_each_path_mut<F: FnMut(&mut Path)>(&mut self, callback: &mut F) {
        for item in &mut self.items {
            item.for_each_path_mut(callback);
        }
        for function in &mut self.functions {
            function.for_each_path_mut(callback);
        }
    }
}

impl<'a> TopologicalVisit for Crate {
    fn visit_items_reverse_topological<F: FnMut(Path, Vec<Vec<Link>>)>(&self, callback: &mut F) {
        self.items.deref().visit_items_reverse_topological(callback);
    }
}

pub struct Analysis {
    pub opaque_set: HashMap<Path, Reason>,
}

pub enum Failure {
    InfiniteTypeSize(Vec<Link>),
}

pub fn analyze(items: &[Item]) -> Result<Analysis, Failure> {
    // Aliases can be direct references to opaque items, but then they
    // themselves cannot be used directly
    let mut opaque_aliases = HashSet::new();
    let mut opaque_set = HashMap::new();
    let mut failure = None;

    items.visit_items_reverse_topological(&mut |path: Path, cycles| {
        let item = &items[path.index];

        if failure.is_some() {
            return;
        }

        for cycle in cycles {
            let infinite_size = cycle.iter().all(|x| x.kind == LinkKind::Direct);
            if infinite_size {
                failure = Some(Failure::InfiniteTypeSize(cycle));
            }
            return;
        }

        macro_rules! field {
            ($variant:expr, $field:expr, $ty:expr) => {
                if let Some(ty_path) = ($ty).is_sized_path() {
                    if opaque_set.contains_key(&ty_path) {
                        opaque_set.insert(
                            path,
                            Reason::DirectlyContainsOpaque {
                                variant: ($variant).map(|x: &String| x.to_owned()),
                                field: ($field).map(|x: &String| x.to_owned()),
                                path: ty_path,
                            },
                        );
                        return;
                    }
                }
            };
        }
        macro_rules! variant {
            ($variant:expr) => {
                match &($variant).node {
                    &VariantKind::Unit => {}
                    &VariantKind::Tuple(ref tuple) => {
                        for ty in &tuple.fields {
                            field!(Some(&($variant).name), None, ty);
                        }
                    }
                    &VariantKind::Struct(ref structure) => {
                        for field in &structure.fields {
                            field!(Some(&($variant).name), Some(&field.name), &field.ty);
                        }
                    }
                }
            };
        }

        match &item.node {
            &ItemKind::Unit => {}
            &ItemKind::Tuple(ref tuple) => {
                for ty in &tuple.fields {
                    field!(None, None, ty);
                }
            }
            &ItemKind::Struct(ref structure) => {
                for field in &structure.fields {
                    field!(None, Some(&field.name), &field.ty);
                }
            }
            &ItemKind::Union(ref union) => {
                for field in &union.fields {
                    field!(None, Some(&field.name), &field.ty);
                }
            }
            &ItemKind::Enum(ref enumeration) => {
                for variant in &enumeration.variants {
                    variant!(variant);
                }
            }
            &ItemKind::Alias { ref ty } => {
                if let Some(ty_path) = ty.is_sized_path() {
                    if opaque_set.contains_key(&ty_path) || opaque_aliases.contains(&ty_path) {
                        opaque_aliases.insert(ty_path);
                    }
                }
            }
            &ItemKind::Opaque(ref reason) => {
                opaque_set.insert(path, reason.clone());
            }
        }
    });

    if let Some(failure) = failure {
        Err(failure)
    } else {
        Ok(Analysis { opaque_set })
    }
}
