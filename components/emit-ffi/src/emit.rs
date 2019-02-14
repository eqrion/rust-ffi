use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;

use getopts;
use rustc::hir::def::{CtorKind, Def};
use rustc::hir::def_id::{CrateNum, DefId, CRATE_DEF_INDEX, LOCAL_CRATE};
use rustc::hir::itemlikevisit::ItemLikeVisitor;
use rustc::hir::{ImplItem, Item, ItemKind, Mutability, TraitItem};
use rustc::middle::privacy::AccessLevels;
use rustc::mir::interpret::{ConstValue, GlobalId};
use rustc::session::config::{self, ErrorOutputType, Input};
use rustc::session::Session;
use rustc::ty::subst::Substs;
use rustc::ty::{
    AdtKind, LazyConst, Const, FnSig, Instance, ParamEnv, ReprOptions, Ty, TyCtxt, TyKind, TypeFlags,
    VariantDef, VariantDiscr,
};
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use rustc_driver::{driver, Compilation, CompilerCalls, RustcDefaultCalls};
use rustc_metadata::cstore::CStore;
use rustc_target::spec::abi::Abi;
use syntax::{ast, errors};

use serde_json;

use ffi;

pub struct Driver {
    default_calls: RustcDefaultCalls,
}

impl Driver {
    pub fn new() -> Driver {
        Driver {
            default_calls: RustcDefaultCalls,
        }
    }
}

impl<'a> CompilerCalls<'a> for Driver {
    fn early_callback(
        &mut self,
        m: &getopts::Matches,
        o: &config::Options,
        cc: &ast::CrateConfig,
        r: &errors::registry::Registry,
        eot: ErrorOutputType,
    ) -> Compilation {
        self.default_calls.early_callback(m, o, cc, r, eot)
    }

    fn late_callback(
        &mut self,
        t: &CodegenBackend,
        m: &getopts::Matches,
        s: &Session,
        c: &CStore,
        i: &Input,
        odir: &Option<PathBuf>,
        ofile: &Option<PathBuf>,
    ) -> Compilation {
        self.default_calls.late_callback(t, m, s, c, i, odir, ofile)
    }

    fn some_input(
        &mut self,
        input: Input,
        input_path: Option<PathBuf>,
    ) -> (Input, Option<PathBuf>) {
        self.default_calls.some_input(input, input_path)
    }

    fn no_input(
        &mut self,
        m: &getopts::Matches,
        o: &config::Options,
        cc: &ast::CrateConfig,
        odir: &Option<PathBuf>,
        ofile: &Option<PathBuf>,
        r: &errors::registry::Registry,
    ) -> Option<(Input, Option<PathBuf>)> {
        self.default_calls.no_input(m, o, cc, odir, ofile, r)
    }

    fn build_controller(
        self: Box<Self>,
        s: &Session,
        m: &getopts::Matches,
    ) -> driver::CompileController<'a> {
        let mut controller = Box::new(RustcDefaultCalls).build_controller(s, m);
        controller.continue_parse_after_error = true;
        controller.after_analysis.callback = box |state: &mut driver::CompileState| {
            emit_ffi(state);
        };
        controller
    }
}

fn emit_ffi(state: &mut driver::CompileState) {
    let tcx = state.tcx.unwrap();
    let access_levels = tcx.privacy_access_levels(LOCAL_CRATE);

    let mut cx = GatherContext::new(
        tcx,
        IndexAllocator::new(),
        Rc::clone(&access_levels),
    );

    let mut visitor = Visitor::new(&mut cx);

    state.hir_crate.unwrap().visit_all_item_likes(&mut visitor);

    visitor.visit_extern_crates();

    let krate = create_compact_sorted_library(
        state.crate_name.unwrap_or("").to_owned(),
        visitor.local_items,
        visitor.extern_items,
        visitor.functions,
        visitor.invalid_functions,
    );

    // Output the file
    let crate_name = state.crate_name.unwrap_or("temp").to_owned();
    // let out_dir = state.out_dir;

    // let mut root_path = match out_dir {
    //     Some(val) => val.join("emit-ffi"),
    //     None => PathBuf::from("emit-ffi-temp"),
    // };

    // if let Err(e) = std::fs::create_dir_all(&root_path) {
    //     println!("Could not create directory {}: {}", root_path.display(), e);
    //     return;
    // }

    let mut out_name = String::new();
    out_name.push_str(&crate_name);
    out_name.push_str(".json");
    // root_path.push(&out_name);

    let file = match File::create(&out_name) {
        Ok(file) => file,
        Err(e) => {
            println!("Could not open file {}: {}", out_name, e);
            return;
        }
    };

    serde_json::to_writer_pretty(file, &krate).unwrap();
}

fn create_compact_sorted_library(
    crate_name: String,
    mut local_items: HashMap<usize, ffi::Item>,
    extern_items: HashMap<usize, ffi::Item>,
    mut functions: Vec<ffi::Function>,
    invalid_functions: Vec<ffi::InvalidFunction>,
) -> ffi::Crate {
    // Collect the extern items that are referenced from local items and
    // filter the unused ones from the output
    let mut used_extern_items = HashSet::new();
    {
        let collect_used_extern_items = &mut |path: &mut ffi::Path| {
            if extern_items.contains_key(&path.index) {
                used_extern_items.insert(path.index);
            }
        };
        for (_, item) in &mut local_items {
            item.for_each_path_mut(collect_used_extern_items);
        }
        for function in &mut functions {
            function.for_each_path_mut(collect_used_extern_items);
        }
    }

    // Gather all of the items, filtering them, and sort them by name
    let mut name_sorted = Vec::new();
    for (index, item) in local_items {
        name_sorted.push((item.name.clone(), (index, item)));
    }
    for (index, item) in extern_items {
        if used_extern_items.contains(&index) {
            name_sorted.push((item.name.clone(), (index, item)));
        }
    }
    name_sorted.sort_by(|x, y| x.0.cmp(&y.0));

    // Collect the sorted and filtered items and build an index fixup table for
    // the new order of items
    let mut fixup = HashMap::new();
    let mut final_items = Vec::new();
    for (i, (_, (index, item))) in name_sorted.into_iter().enumerate() {
        fixup.insert(index, i);
        final_items.push(item);
    }

    // Create the library
    let mut lib = ffi::Crate::new(crate_name, final_items, functions, invalid_functions);

    // Fixup the paths
    lib.for_each_path_mut(&mut |path: &mut ffi::Path| {
        if let Some(fixed) = fixup.get(&path.index) {
            path.index = *fixed;
        }
    });

    lib
}

struct IndexAllocator {
    last_index: usize,
    allocated: HashMap<DefId, usize>,
    order: Vec<DefId>,
}

impl IndexAllocator {
    fn new() -> IndexAllocator {
        IndexAllocator {
            last_index: 0,
            allocated: HashMap::new(),
            order: Vec::new(),
        }
    }

    fn allocate(&mut self, def_id: DefId) -> usize {
        let last_index = self.last_index;
        let mut new_item = false;

        let index = *self.allocated.entry(def_id).or_insert_with(|| {
            new_item = true;
            last_index + 1
        });

        if new_item {
            self.last_index += 1;
            self.order.push(def_id);
        }
        index
    }
}

fn evaluate_array_size_lazy<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, value: &LazyConst<'tcx>) -> usize {
    match value {
        LazyConst::Unevaluated(def_id, subst) => {
            let instance = Instance::new(*def_id, subst);
            let env = ParamEnv::empty();
            let global_id = GlobalId {
                instance,
                promoted: None,
            };
            let key = env.and(global_id);

            let value = tcx.const_eval(key).unwrap();

            // `const_eval` is guaranteed to not return `Unevaluated`, so this
            // should not infinitely recurse
            evaluate_array_size(tcx, &value)
        }
        LazyConst::Evaluated(value) => evaluate_array_size(tcx, value),
    }
}

fn evaluate_array_size<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, value: &Const<'tcx>) -> usize {
    assert!(match value.ty.sty {
        TyKind::Uint(ast::UintTy::Usize) => true,
        _ => false,
    });

    match value.val {
        ConstValue::Scalar(scalar) => scalar.to_usize(&tcx).map(|x| x as usize).unwrap(),
        ConstValue::Slice(..) => {
            unreachable!();
        }
        ConstValue::ByRef(..) => {
            unreachable!();
        }
    }
}

fn evaluate_variant_discr<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, def_id: DefId) -> isize {
    let subst = Substs::identity_for_item(tcx, def_id);
    let instance = Instance::new(def_id, subst);
    let env = ParamEnv::empty();
    let global_id = GlobalId {
        instance,
        promoted: None,
    };
    let key = env.and(global_id);
    let value = tcx.const_eval(key).unwrap();

    assert!(match value.ty.sty {
        TyKind::Int(ast::IntTy::Isize) => true,
        _ => false,
    });

    match value.val {
        ConstValue::Scalar(scalar) => scalar.to_isize(&tcx).map(|x| x as isize).unwrap(),
        ConstValue::Slice(..) | ConstValue::ByRef(..) => {
            unreachable!();
        }
    }
}

fn attribute_is_word(attr: &ast::Attribute, name: &str) -> bool {
    attr.meta()
        .map(|x| x.is_word() && x.name() == name)
        .unwrap_or(false)
}

struct GatherContext<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    index_alloc: IndexAllocator,
    access_levels: Rc<AccessLevels>,
}

impl<'a, 'tcx: 'a> GatherContext<'a, 'tcx> {
    fn new(
        tcx: TyCtxt<'a, 'tcx, 'tcx>,
        index_alloc: IndexAllocator,
        access_levels: Rc<AccessLevels>,
    ) -> Self {
        GatherContext {
            tcx,
            index_alloc,
            access_levels,
        }
    }
}

trait Gather<'a, 'tcx: 'a> {
    type Output;

    fn gather(self, cx: &mut GatherContext<'a, 'tcx>) -> Self::Output;
}

impl<'a, 'tcx: 'a> Gather<'a, 'tcx> for Abi {
    type Output = Result<ffi::Abi, ffi::Reason>;

    fn gather(self, _: &mut GatherContext<'a, 'tcx>) -> Self::Output {
        Ok(match self {
            Abi::Cdecl => ffi::Abi::Cdecl,
            Abi::Stdcall => ffi::Abi::Stdcall,
            Abi::Fastcall => ffi::Abi::Fastcall,
            Abi::Vectorcall => ffi::Abi::Vectorcall,
            Abi::Thiscall => ffi::Abi::Thiscall,
            Abi::Win64 => ffi::Abi::Win64,
            Abi::SysV64 => ffi::Abi::SysV64,
            Abi::C => ffi::Abi::C,
            Abi::System => ffi::Abi::System,
            _ => {
                return Err(ffi::Reason::InvalidAbi {
                    invalid_abi: format!("{}", self),
                })
            }
        })
    }
}

impl<'a, 'b, 'tcx: 'b> Gather<'b, 'tcx> for &'a FnSig<'tcx> {
    type Output = Result<ffi::FunctionSignature, ffi::Reason>;

    fn gather(self, cx: &mut GatherContext<'b, 'tcx>) -> Self::Output {
        let inputs = self.inputs();
        let output = self.output();
        let abi = self.abi.gather(cx)?;

        let ret = output.gather(cx)?;

        let mut args = Vec::new();
        for input in inputs {
            args.push(input.gather(cx)?);
        }

        Ok(ffi::FunctionSignature::new(ret, args, abi))
    }
}

impl<'a, 'tcx: 'a> Gather<'a, 'tcx> for Ty<'tcx> {
    type Output = Result<ffi::Type, ffi::Reason>;

    fn gather(self, cx: &mut GatherContext<'a, 'tcx>) -> Self::Output {
        match self.sty {
            TyKind::Bool => Ok(ffi::Type::Bool),
            TyKind::Char => Ok(ffi::Type::Char),
            TyKind::Int(width) => {
                use syntax::ast::IntTy::*;
                Ok(match width {
                    Isize => ffi::Type::ISize,
                    I128 => ffi::Type::I128,
                    I64 => ffi::Type::I64,
                    I32 => ffi::Type::I32,
                    I16 => ffi::Type::I16,
                    I8 => ffi::Type::I8,
                })
            }
            TyKind::Uint(width) => {
                use syntax::ast::UintTy::*;
                Ok(match width {
                    Usize => ffi::Type::USize,
                    U128 => ffi::Type::U128,
                    U64 => ffi::Type::U64,
                    U32 => ffi::Type::U32,
                    U16 => ffi::Type::U16,
                    U8 => ffi::Type::U8,
                })
            }
            TyKind::Float(width) => {
                use syntax::ast::FloatTy::*;
                Ok(match width {
                    F32 => ffi::Type::F32,
                    F64 => ffi::Type::F64,
                })
            }
            TyKind::Ref(_, ty, mutable) => {
                let ty = ty.gather(cx)?;
                let mutability = match mutable {
                    Mutability::MutImmutable => ffi::Mutability::Const,
                    Mutability::MutMutable => ffi::Mutability::Mut,
                };
                Ok(ffi::Type::RefPtr(ffi::Pointer::new(ty, mutability)))
            }
            TyKind::RawPtr(ty_and_mutable) => {
                let ty = ty_and_mutable.ty.gather(cx)?;
                let mutability = match ty_and_mutable.mutbl {
                    Mutability::MutImmutable => ffi::Mutability::Const,
                    Mutability::MutMutable => ffi::Mutability::Mut,
                };
                Ok(ffi::Type::RawPtr(ffi::Pointer::new(ty, mutability)))
            }
            TyKind::FnPtr(sig) => {
                let sig = sig.skip_binder().gather(cx)?;
                Ok(ffi::Type::FnPtr(sig))
            }
            TyKind::Array(ty, size) => {
                let size = evaluate_array_size_lazy(cx.tcx, size);
                let ty = ty.gather(cx)?;
                Ok(ffi::Type::Array(ffi::Array::new(ty, size)))
            }
            TyKind::Adt(adt, subst) => {
                if !subst.is_empty() {
                    return Err(ffi::Reason::ContainsGeneric);
                }
                let index = cx.index_alloc.allocate(adt.did);
                Ok(ffi::Type::Path(ffi::Path::new(index)))
            }
            TyKind::Tuple(tys) if tys.is_empty() => Ok(ffi::Type::Void),
            _ => Err(ffi::Reason::InvalidType {
                invalid_type: format!("{}", self),
            }),
        }
    }
}

impl<'a, 'tcx: 'a> Gather<'a, 'tcx> for (&'a VariantDef, isize, &'tcx Substs<'tcx>) {
    type Output = Result<ffi::Variant, ffi::Reason>;

    fn gather(self, cx: &mut GatherContext<'a, 'tcx>) -> Self::Output {
        let (variant, last_discriminant, subst) = (self.0, self.1, self.2);

        let mut names = Vec::new();
        let mut tys = Vec::new();

        for field in &variant.fields {
            let field_name = field.ident.to_string();
            let field_ty = field.ty(cx.tcx, subst).gather(cx)?;

            names.push(field_name);
            tys.push(field_ty);
        }

        let name = variant.ident.name.to_string();
        let data = match variant.ctor_kind {
            CtorKind::Fn => ffi::VariantKind::Tuple(ffi::VariantTuple { fields: tys }),
            CtorKind::Fictive => {
                let fields = names
                    .into_iter()
                    .zip(tys.into_iter())
                    .map(|(name, ty)| ffi::Field { name, ty })
                    .collect();

                ffi::VariantKind::Struct(ffi::VariantStruct { fields })
            }
            CtorKind::Const => {
                assert!(names.is_empty());
                ffi::VariantKind::Unit
            }
        };

        let discriminant = match variant.discr {
            VariantDiscr::Explicit(def_id) => evaluate_variant_discr(cx.tcx, def_id),
            VariantDiscr::Relative(_) => {
                if last_discriminant == isize::max_value() {
                    0
                } else {
                    last_discriminant + 1
                }
            }
        };

        Ok(ffi::Variant {
            name,
            node: data,
            discriminant,
        })
    }
}

impl<'a, 'tcx: 'a> Gather<'a, 'tcx> for ReprOptions {
    type Output = Result<ffi::Repr, ffi::Reason>;

    fn gather(self, _: &mut GatherContext<'a, 'tcx>) -> Self::Output {
        if !self.c() && !self.transparent() {
            return Err(ffi::Reason::NotReprC);
        }

        use syntax::ast::{IntTy, UintTy};
        use syntax::attr::IntType;

        let discriminant = match self.int {
            Some(IntType::SignedInt(ty)) => Some(match ty {
                IntTy::Isize => ffi::DiscriminantType::ISize,
                IntTy::I128 => ffi::DiscriminantType::I128,
                IntTy::I64 => ffi::DiscriminantType::I64,
                IntTy::I32 => ffi::DiscriminantType::I32,
                IntTy::I16 => ffi::DiscriminantType::I16,
                IntTy::I8 => ffi::DiscriminantType::I8,
            }),
            Some(IntType::UnsignedInt(ty)) => Some(match ty {
                UintTy::Usize => ffi::DiscriminantType::USize,
                UintTy::U128 => ffi::DiscriminantType::U128,
                UintTy::U64 => ffi::DiscriminantType::U64,
                UintTy::U32 => ffi::DiscriminantType::U32,
                UintTy::U16 => ffi::DiscriminantType::U16,
                UintTy::U8 => ffi::DiscriminantType::U8,
            }),
            None => None,
        };

        Ok(ffi::Repr {
            discriminant,
            min_pack: self.pack,
            max_align: self.align,
            transparent: self.transparent(),
            simd: self.simd(),
        })
    }
}

impl<'a, 'tcx: 'a> Gather<'a, 'tcx> for DefId {
    type Output = (usize, ffi::Item);

    fn gather(self, cx: &mut GatherContext<'a, 'tcx>) -> Self::Output {
        let ty = cx.tcx.type_of(self);

        match ty.sty {
            TyKind::Adt(def, subst) => {
                let ffi_name = cx.tcx.absolute_item_path_str(def.did);
                let ffi_index = cx.index_alloc.allocate(def.did);

                if ty.flags.intersects(TypeFlags::NEEDS_SUBST) {
                    return (
                        ffi_index,
                        ffi::Item {
                            name: ffi_name,
                            node: ffi::ItemKind::Opaque(ffi::Reason::IsGeneric),
                        },
                    );
                }

                let repr = match def.repr.gather(cx) {
                    Ok(repr) => repr,
                    Err(reason) => {
                        return (
                            ffi_index,
                            ffi::Item {
                                name: ffi_name,
                                node: ffi::ItemKind::Opaque(reason),
                            },
                        )
                    }
                };

                match def.adt_kind() {
                    AdtKind::Struct | AdtKind::Union => {
                        assert!(def.variants.len() == 1);
                        let variant = &def.variants[0_usize.into()];
                        let last_discriminant = isize::max_value();

                        match (variant, last_discriminant, subst).gather(cx) {
                            Ok(ffi::Variant {
                                name: _,
                                node,
                                discriminant: _,
                            }) => {
                                let node = if def.is_struct() {
                                    match node {
                                        ffi::VariantKind::Unit => ffi::ItemKind::Unit,
                                        ffi::VariantKind::Tuple(ffi::VariantTuple { fields }) => {
                                            ffi::ItemKind::Tuple(ffi::Tuple { fields, repr })
                                        }
                                        ffi::VariantKind::Struct(ffi::VariantStruct { fields }) => {
                                            ffi::ItemKind::Struct(ffi::Struct { fields, repr })
                                        }
                                    }
                                } else {
                                    match node {
                                        ffi::VariantKind::Tuple(..) | ffi::VariantKind::Unit => {
                                            unreachable!()
                                        }
                                        ffi::VariantKind::Struct(ffi::VariantStruct { fields }) => {
                                            ffi::ItemKind::Union(ffi::Union { fields, repr })
                                        }
                                    }
                                };

                                (
                                    ffi_index,
                                    ffi::Item {
                                        name: ffi_name,
                                        node,
                                    },
                                )
                            }
                            Err(reason) => (
                                ffi_index,
                                ffi::Item {
                                    name: ffi_name,
                                    node: ffi::ItemKind::Opaque(reason),
                                },
                            ),
                        }
                    }
                    AdtKind::Enum => {
                        let mut variants = Vec::new();
                        let mut last_discriminant = isize::max_value();

                        for variant in &def.variants {
                            match (variant, last_discriminant, subst).gather(cx) {
                                Ok(variant) => {
                                    last_discriminant = variant.discriminant;
                                    variants.push(variant);
                                }
                                Err(reason) => {
                                    return (
                                        ffi_index,
                                        ffi::Item {
                                            name: ffi_name,
                                            node: ffi::ItemKind::Opaque(reason),
                                        },
                                    );
                                }
                            }
                        }

                        let node = ffi::ItemKind::Enum(ffi::Enum { variants, repr });

                        (
                            ffi_index,
                            ffi::Item {
                                name: ffi_name,
                                node,
                            },
                        )
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

// TODO: Make this work with the Gather trait
fn gather_alias<'a, 'tcx: 'a>(
    def_id: DefId,
    cx: &mut GatherContext<'a, 'tcx>,
) -> (usize, ffi::Item) {
    let ffi_index = cx.index_alloc.allocate(def_id);
    let ffi_name = cx.tcx.absolute_item_path_str(def_id);

    (
        ffi_index,
        match cx.tcx.type_of(def_id).gather(cx) {
            Ok(ty) => ffi::Item {
                name: ffi_name,
                node: ffi::ItemKind::Alias { ty },
            },
            Err(reason) => ffi::Item {
                name: ffi_name,
                node: ffi::ItemKind::Opaque(reason),
            },
        },
    )
}

struct Visitor<'a, 'tcx: 'a> {
    cx: &'a mut GatherContext<'a, 'tcx>,
    visited_mods: HashSet<DefId>,
    local_items: HashMap<usize, ffi::Item>,
    extern_items: HashMap<usize, ffi::Item>,
    functions: Vec<ffi::Function>,
    invalid_functions: Vec<ffi::InvalidFunction>,
}

impl<'a, 'tcx: 'a> Visitor<'a, 'tcx> {
    fn new(cx: &'a mut GatherContext<'a, 'tcx>) -> Visitor<'a, 'tcx> {
        Visitor {
            cx,
            visited_mods: HashSet::new(),
            local_items: HashMap::new(),
            extern_items: HashMap::new(),
            functions: Vec::new(),
            invalid_functions: Vec::new(),
        }
    }
}

impl<'a, 'tcx: 'a> ItemLikeVisitor<'a> for Visitor<'a, 'tcx> {
    fn visit_item(&mut self, item: &'a Item) {
        if !self.cx.access_levels.is_reachable(item.id) {
            return;
        }

        match &item.node {
            ItemKind::Struct(_, _) | ItemKind::Enum(_, _) | ItemKind::Union(_, _) => {
                let def_id = self.cx.tcx.hir().local_def_id(item.id);

                let (ffi_index, ffi_item) = def_id.gather(self.cx);

                let duplicate = self.local_items.insert(ffi_index, ffi_item).is_some();
                assert!(!duplicate);
            }
            ItemKind::Ty(_, _) => {
                let def_id = self.cx.tcx.hir().local_def_id(item.id);
                let (ffi_index, ffi_item) = gather_alias(def_id, self.cx);

                let duplicate = self.local_items.insert(ffi_index, ffi_item).is_some();
                assert!(!duplicate);
            }
            ItemKind::Fn(_, _, _, _) => {
                let def_id = self.cx.tcx.hir().local_def_id(item.id);
                let ty = self.cx.tcx.type_of(def_id);
                let poly_sig = ty.fn_sig(self.cx.tcx);
                let sig = poly_sig.skip_binder();
                let no_mangle = item.attrs.iter().any(|x| attribute_is_word(x, "no_mangle"));

                match sig.gather(self.cx) {
                    Ok(ffi_signature) => {
                        if no_mangle {
                            self.functions
                                .push(ffi::Function::new(item.ident.name.to_string(), ffi_signature));
                        } else {
                            self.invalid_functions.push(ffi::InvalidFunction::new(
                                item.ident.name.to_string(),
                                ffi::Reason::NotNoMangle,
                            ));
                        }
                    }
                    Err(reason) => match reason {
                        ffi::Reason::InvalidAbi { .. } => {}
                        _ => {
                            self.invalid_functions
                                .push(ffi::InvalidFunction::new(item.ident.name.to_string(), reason));
                        }
                    },
                }
            }
            _ => {}
        }
    }

    fn visit_trait_item(&mut self, _item: &'a TraitItem) {}

    fn visit_impl_item(&mut self, _item: &'a ImplItem) {}
}

impl<'a, 'tcx: 'a> Visitor<'a, 'tcx> {
    fn visit_extern_crates(&mut self) {
        for cnum in self.cx.tcx.crates().iter() {
            self.visit_crate(*cnum);
        }
    }

    fn visit_crate(&mut self, cnum: CrateNum) {
        assert!(cnum != LOCAL_CRATE);
        let did = DefId {
            krate: cnum,
            index: CRATE_DEF_INDEX,
        };
        self.visit_mod(did);
    }

    fn visit_mod(&mut self, def_id: DefId) {
        if !self.visited_mods.insert(def_id) {
            return;
        }

        for item in self.cx.tcx.item_children(def_id).iter() {
            if self
                .cx
                .tcx
                .def_key(item.def.def_id())
                .parent
                .map_or(false, |d| d == def_id.index)
            {
                self.visit_item(item.def);
            }
        }
    }

    fn visit_item(&mut self, def: Def) {
        let def_id = def.def_id();

        match def {
            Def::Mod(..) => {
                self.visit_mod(def_id);
            }
            Def::Struct(..) | Def::Union(..) | Def::Enum(..) => {
                let (ffi_index, ffi_item) = def_id.gather(self.cx);
                let duplicate = self.extern_items.insert(ffi_index, ffi_item).is_some();
                assert!(!duplicate);
            }
            Def::TyAlias(def_id) => {
                let (ffi_index, ffi_item) = gather_alias(def_id, self.cx);

                let duplicate = self.extern_items.insert(ffi_index, ffi_item).is_some();
                assert!(!duplicate);
            }
            _ => {}
        }
    }
}
