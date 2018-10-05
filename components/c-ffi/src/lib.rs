extern crate c_ast as c;
extern crate metadata_ffi as ffi;

use c::ast;
use ffi::TopologicalVisit;

use std::collections::HashSet;
use std::fs;
use std::io::Write;

const VERSION: &'static str =
    concat!("Generated with c-ffi version ", env!("CARGO_PKG_VERSION"));
const C_STANDARD_INCLUDES: &[&str] = &["stdint.h", "stdlib.h", "stdbool.h"];
const CXX_STANDARD_INCLUDES: &[&str] = &["cstdint", "cstdlib"];

#[derive(Debug, Clone, PartialEq)]
pub enum Language {
    Cxx,
    C,
}

#[derive(Debug, Clone)]
pub struct Config {
    /// Optional text to output at the beginning of the file
    pub header: Option<String>,
    /// Optional text to output at the end of the file
    pub trailer: Option<String>,
    /// Optional name to use for an include guard
    pub include_guard: Option<String>,
    /// A list of additional includes to put at the beginning of the generated header
    pub includes: Vec<String>,
    /// A list of additional system includes to put at the beginning of the generated header
    pub sys_includes: Vec<String>,
    /// Optional text to output at major sections to deter manual editing
    pub autogen_warning: Option<String>,
    /// Include a comment with the version of cbindgen used to generate the file
    pub include_version: bool,
    /// The language to output bindings for
    pub language: Language,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            header: None,
            trailer: None,
            include_guard: None,
            includes: Vec::new(),
            sys_includes: Vec::new(),
            autogen_warning: None,
            include_version: true,
            language: Language::Cxx,
        }
    }
}

struct Name {
    segmented: Vec<String>,
    key: c::ty::IdentifierKey,
}

impl Name {
    fn name(&self) -> &String {
        (&self.segmented).split_last().unwrap().0
    }

    fn namespaces(&self) -> &[String] {
        (&self.segmented).split_last().unwrap().1
    }
}

struct Context {
    lib: ffi::Crate,
    config: Config,
    names: Vec<Name>,
}

trait Convert {
    type Out;
    fn convert(&self, ctx: &Context) -> Self::Out;
}

impl Convert for ffi::Type {
    type Out = c::ty::Type;

    fn convert(&self, ctx: &Context) -> Self::Out {
        match self {
            &ffi::Type::Void => c::ty::ident_str("void"),
            &ffi::Type::Bool => c::ty::ident_str("bool"),
            &ffi::Type::Char => c::ty::ident_str("char"),
            &ffi::Type::I8 => c::ty::ident_str("int8_t"),
            &ffi::Type::I16 => c::ty::ident_str("int16_t"),
            &ffi::Type::I32 => c::ty::ident_str("int32_t"),
            &ffi::Type::I64 => c::ty::ident_str("int64_t"),
            &ffi::Type::I128 => c::ty::ident_str("int128_t"),
            &ffi::Type::ISize => c::ty::ident_str("intptr_t"),
            &ffi::Type::U8 => c::ty::ident_str("uint8_t"),
            &ffi::Type::U16 => c::ty::ident_str("uint16_t"),
            &ffi::Type::U32 => c::ty::ident_str("uint32_t"),
            &ffi::Type::U64 => c::ty::ident_str("uint64_t"),
            &ffi::Type::U128 => c::ty::ident_str("uint128_t"),
            &ffi::Type::USize => c::ty::ident_str("uintptr_t"),
            &ffi::Type::F32 => c::ty::ident_str("float"),
            &ffi::Type::F64 => c::ty::ident_str("double"),
            &ffi::Type::RawPtr(ref pointer) => {
                if pointer.mutability.is_const() {
                    c::ty::const_ptr(pointer.referenced.convert(ctx))
                } else {
                    c::ty::ptr(pointer.referenced.convert(ctx))
                }
            }
            &ffi::Type::RefPtr(ref pointer) => {
                if pointer.mutability.is_const() {
                    c::ty::const_ptr(pointer.referenced.convert(ctx))
                } else {
                    c::ty::ptr(pointer.referenced.convert(ctx))
                }
            }
            &ffi::Type::FnPtr(ref signature) => c::ty::fn_ptr(
                signature.output.convert(ctx),
                &signature
                    .inputs
                    .iter()
                    .map(|x| x.convert(ctx))
                    .collect::<Vec<_>>(),
            ),
            &ffi::Type::Array(ref array) => {
                c::ty::array(array.referenced.convert(ctx), Some(array.size))
            }
            &ffi::Type::Path(ref path) => {
                let name = &ctx.names[path.index];
                let is_cxx = ctx.config.language == Language::Cxx;
                assert!(is_cxx || name.segmented.len() == 1);

                c::ty::ident_key(name.key, c::qualified_ident(is_cxx, &name.segmented))
            }
        }
    }
}

impl Convert for ffi::Field {
    type Out = ast::Declaration;

    fn convert(&self, ctx: &Context) -> Self::Out {
        c::var::decl(&[], self.ty.convert(ctx), c::ident(&self.name))
    }
}

trait Item {
    fn to_declaration(&self, name: &str, ctx: &Context) -> ast::Declaration;
    fn to_definition(&self, name: &str, ctx: &Context) -> ast::Declaration;
}

impl Item for ffi::Struct {
    fn to_declaration(&self, name: &str, _ctx: &Context) -> ast::Declaration {
        c::class::decl(ast::ClassKey::Struct, c::ident(&name))
    }

    fn to_definition(&self, name: &str, ctx: &Context) -> ast::Declaration {
        let mut members = Vec::new();

        for field in &self.fields {
            members.push(c::class::member_decl(field.convert(ctx)));
        }

        c::class::def(
            ast::ClassKey::Struct,
            Some(c::ident(&name)),
            &[],
            &members,
            None,
        )
    }
}

impl Item for ffi::Tuple {
    fn to_declaration(&self, name: &str, _ctx: &Context) -> ast::Declaration {
        c::class::decl(ast::ClassKey::Struct, c::ident(&name))
    }

    fn to_definition(&self, name: &str, ctx: &Context) -> ast::Declaration {
        let mut members = Vec::new();

        for (i, ty) in self.fields.iter().enumerate() {
            let name = format!("_{}", i);
            members.push(c::class::member_decl(c::var::decl(
                &[],
                ty.convert(ctx),
                c::ident(&name),
            )));
        }

        c::class::def(
            ast::ClassKey::Struct,
            Some(c::ident(name)),
            &[],
            &members,
            None,
        )
    }
}

impl Item for ffi::Union {
    fn to_declaration(&self, name: &str, _ctx: &Context) -> ast::Declaration {
        c::class::decl(ast::ClassKey::Union, c::ident(&name))
    }

    fn to_definition(&self, name: &str, ctx: &Context) -> ast::Declaration {
        let mut members = Vec::new();

        for field in &self.fields {
            members.push(c::class::member_decl(field.convert(ctx)));
        }

        c::class::def(
            ast::ClassKey::Union,
            Some(c::ident(name)),
            &[],
            &members,
            None,
        )
    }
}

fn is_c_like(enumeration: &ffi::Enum) -> bool {
    if enumeration.variants.is_empty() {
        return true;
    }
    enumeration.variants.iter().all(|x| x.node.is_unit())
}

fn gen_enum_tag(name: &str, enumeration: &ffi::Enum) -> ast::Declaration {
    let mut enumerators = Vec::new();
    for variant in &enumeration.variants {
        enumerators.push(c::enumeration::enumerator(
            &variant.name,
            Some(variant.discriminant),
        ))
    }
    c::enumeration::def(true, Some(c::ident(name)), &enumerators, None)
}

impl Item for ffi::Enum {
    fn to_declaration(&self, name: &str, _ctx: &Context) -> ast::Declaration {
        if is_c_like(self) {
            c::enumeration::decl(c::ident(name))
        } else {
            c::class::decl(ast::ClassKey::Struct, c::ident(name))
        }
    }

    fn to_definition(&self, name: &str, _ctx: &Context) -> ast::Declaration {
        if is_c_like(self) {
            gen_enum_tag(name, self)
        } else {
            // TODO: Handle `enum`'s containing fields
            c::class::decl(ast::ClassKey::Struct, c::ident(name))
        }
    }
}

#[derive(Debug)]
enum Output {
    Declaration(ffi::Path),
    Definition(ffi::Path),
}

fn compute_order(lib: &ffi::Crate) -> Vec<Output> {
    let mut visited_declarations = HashSet::new();
    let mut order = Vec::new();

    lib.visit_items_reverse_topological(&mut |path, cycle_paths| {
        for mut cycle in cycle_paths {
            let declaration_needed = cycle.pop().unwrap().path;

            if visited_declarations.insert(declaration_needed) && declaration_needed != path {
                order.push(Output::Declaration(declaration_needed));
            }
        }
        order.push(Output::Definition(path));
    });

    order
}

fn parse_name(name: &str) -> Vec<String> {
    name.split("::").map(|x| x.to_owned()).collect()
}

fn mangle_name(name: &str) -> String {
    name.replace("::", "_")
}

fn compute_names(lib: &ffi::Crate, config: &Config) -> Vec<Name> {
    let mut names = Vec::new();
    for item in &lib.items {
        let key = match &item.node {
            &ffi::ItemKind::Unit => c::ty::IdentifierKey::Struct,
            &ffi::ItemKind::Tuple(..) => c::ty::IdentifierKey::Struct,
            &ffi::ItemKind::Struct(..) => c::ty::IdentifierKey::Struct,
            &ffi::ItemKind::Union(..) => c::ty::IdentifierKey::Union,
            &ffi::ItemKind::Enum(..) => c::ty::IdentifierKey::Enum,
            &ffi::ItemKind::Alias { .. } => c::ty::IdentifierKey::None,
            &ffi::ItemKind::Opaque(..) => c::ty::IdentifierKey::Struct,
        };

        names.push(if config.language == Language::Cxx {
            Name {
                segmented: parse_name(&item.name),
                key,
            }
        } else {
            Name {
                segmented: vec![mangle_name(&item.name)],
                key,
            }
        });
    }
    names
}

pub fn generate(lib: ffi::Crate, config: Config, mut out: fs::File) {
    let order = compute_order(&lib);
    let names = compute_names(&lib, &config);
    let ctx = Context { lib, config, names };

    let mut pushed_namespaces = Vec::new();

    let mut ast = c::new();

    if ctx.config.include_version {
        ast.append(c::global::comment(VERSION));
    }

    if let Some(header) = ctx.config.header.as_ref() {
        ast.append(c::global::comment(header));
    }

    if let Some(include_guard) = ctx.config.include_guard.as_ref() {
        ast.enter_conditional(c::conditional::not(c::conditional::defined(
            include_guard.to_owned(),
        )));
        ast.append(c::global::define(include_guard, None));
    }

    let standard_includes = if ctx.config.language == Language::Cxx {
        CXX_STANDARD_INCLUDES
    } else {
        C_STANDARD_INCLUDES
    };
    for include in standard_includes {
        ast.append(c::global::include(true, include));
    }
    for include in &ctx.config.sys_includes {
        ast.append(c::global::include(true, include));
    }
    for include in &ctx.config.includes {
        ast.append(c::global::include(false, include));
    }

    if let Some(autogen_warning) = ctx.config.autogen_warning.as_ref() {
        ast.append(c::global::comment(autogen_warning));
    }

    // Items
    for output in order {
        match output {
            Output::Declaration(path) => {
                let item = &ctx.lib.items[path.index];
                let name = &ctx.names[path.index];

                if ctx.config.language == Language::Cxx {
                    let in_common = pushed_namespaces
                        .iter()
                        .zip(name.namespaces())
                        .position(|(x, y)| x != y)
                        .unwrap_or(pushed_namespaces.len());
                    let adjustment = pushed_namespaces.len() - in_common;
                    for _ in 0..adjustment {
                        pushed_namespaces.pop();
                        ast.leave();
                    }

                    for namespace in name.namespaces().iter().skip(in_common) {
                        ast.enter_namespace(Some(namespace));
                        pushed_namespaces.push(namespace.to_owned());
                    }
                }

                match &item.node {
                    &ffi::ItemKind::Unit => {
                        let decl = c::class::decl(ast::ClassKey::Struct, c::ident(&name.name()));
                        ast.append(c::global::decl(decl));
                    }
                    &ffi::ItemKind::Struct(ref structure) => {
                        ast.append(c::global::decl(
                            structure.to_declaration(&name.name(), &ctx),
                        ));
                    }
                    &ffi::ItemKind::Tuple(ref tuple) => {
                        ast.append(c::global::decl(tuple.to_declaration(&name.name(), &ctx)));
                    }
                    &ffi::ItemKind::Union(ref union) => {
                        ast.append(c::global::decl(union.to_declaration(&name.name(), &ctx)));
                    }
                    &ffi::ItemKind::Enum(ref enumeration) => {
                        ast.append(c::global::decl(
                            enumeration.to_declaration(&name.name(), &ctx),
                        ));
                    }
                    &ffi::ItemKind::Alias { .. } => {
                        // An alias cannot be part of a cycle and need to be forward declared
                        unreachable!();
                    }
                    &ffi::ItemKind::Opaque(..) => {
                        let decl = c::class::decl(ast::ClassKey::Struct, c::ident(&name.name()));
                        ast.append(c::global::decl(decl));
                    }
                }
            }
            Output::Definition(path) => {
                let item = &ctx.lib.items[path.index];
                let name = &ctx.names[path.index];

                if ctx.config.language == Language::Cxx {
                    let in_common = pushed_namespaces
                        .iter()
                        .zip(name.namespaces())
                        .position(|(x, y)| x != y)
                        .unwrap_or(pushed_namespaces.len());
                    let adjustment = pushed_namespaces.len() - in_common;
                    for _ in 0..adjustment {
                        pushed_namespaces.pop();
                        ast.leave();
                    }

                    for namespace in name.namespaces().iter().skip(in_common) {
                        ast.enter_namespace(Some(namespace));
                        pushed_namespaces.push(namespace.to_owned());
                    }
                }

                match &item.node {
                    &ffi::ItemKind::Unit => {
                        let decl = c::class::decl(ast::ClassKey::Struct, c::ident(&name.name()));
                        ast.append(c::global::decl(decl));
                    }
                    &ffi::ItemKind::Struct(ref structure) => {
                        ast.append(c::global::decl(structure.to_definition(&name.name(), &ctx)));
                    }
                    &ffi::ItemKind::Tuple(ref tuple) => {
                        ast.append(c::global::decl(tuple.to_definition(&name.name(), &ctx)));
                    }
                    &ffi::ItemKind::Union(ref union) => {
                        ast.append(c::global::decl(union.to_definition(&name.name(), &ctx)));
                    }
                    &ffi::ItemKind::Enum(ref enumeration) => {
                        ast.append(c::global::decl(
                            enumeration.to_definition(&name.name(), &ctx),
                        ));
                    }
                    &ffi::ItemKind::Alias { ref ty } => {
                        let def = c::var::decl(&[], ty.convert(&ctx), c::ident(&name.name()));
                        ast.append(c::global::typedef(def));
                    }
                    &ffi::ItemKind::Opaque(..) => {
                        let decl = c::class::decl(ast::ClassKey::Struct, c::ident(&name.name()));
                        ast.append(c::global::decl(decl));
                    }
                }
            }
        }
    }
    if ctx.config.language == Language::Cxx {
        for _ in 0..pushed_namespaces.len() {
            pushed_namespaces.pop();
            ast.leave();
        }
    }

    if let Some(autogen_warning) = ctx.config.autogen_warning.as_ref() {
        ast.append(c::global::comment(autogen_warning));
    }

    // Functions
    if ctx.config.language == Language::Cxx {
        ast.enter_extern_c();
    }
    for function in &ctx.lib.functions {
        let decl = c::func::decl(
            None,
            &[],
            function.signature.output.convert(&ctx),
            c::ident(&function.name),
            &function
                .signature
                .inputs
                .iter()
                .map(|x| (None, x.convert(&ctx)))
                .collect::<Vec<_>>(),
            None,
            ast::CvQualifier::None,
        );
        ast.append(c::global::decl(decl));
    }
    if ctx.config.language == Language::Cxx {
        ast.leave();
    }

    if ctx.config.include_guard.is_some() {
        ast.leave();
    }

    if let Some(trailer) = ctx.config.trailer.as_ref() {
        ast.append(c::global::comment(trailer));
    }

    write!(out, "{}", ast.to_string()).unwrap();
}
