pub mod ast;
mod gen;
mod mutate;
mod write;
mod writer;

pub use gen::*;

use std::io;

fn main() {
    let file = file(&[
        global::comment("hello world"),
        global::include(true, "cstdio.h"),
        global::include(false, "foobar.h"),
        global::define("FOO", Some("5")),
        global::ws(2),
        global::conditional(
            conditional::not(conditional::defined("XP_WIN")),
            &[global::comment("whoah")],
        ),
        global::ws(2),
        global::conditional(
            conditional::and(
                conditional::or(
                    conditional::defined("XP_WIN"),
                    conditional::defined("XP_OSX"),
                ),
                conditional::not(conditional::constant("SKIP")),
            ),
            &[global::comment("whoah")],
        ),
        global::ws(2),
        global::namespace("mozilla", &[global::include(true, "hello")]),
        global::ws(2),
        global::using(ident("hello")),
        global::using_namespace(qualified_ident(true, &["world"])),
        global::ws(2),
        global::decl(var::decl(
            &[],
            ty::ptr(ty::ident_class(ident("Point"))),
            ident("coord"),
        )),
        global::decl(var::decl(
            &[ast::Specifier::Static],
            ty::fn_ptr(ty::array(ty::ident(ident("Point")), None), &[ty::char()]),
            ident("coord"),
        )),
        global::typedef(var::decl(
            &[],
            ty::fn_ptr(
                ty::array(ty::ident(ident("Point")), None),
                &[ty::ident(ident("int"))],
            ),
            ident("coord"),
        )),
        global::def(var::def(
            &[ast::Specifier::ThreadLocal],
            ty::array(ty::ident(ident("Point")), None),
            ident("coord"),
            expr::multiply(
                expr::add(expr::ident(ident("FOO")), expr::constant("5")),
                expr::member_access(expr::ident(ident("FOO")), ident("member")),
            ),
        )),
        global::def(var::def(
            &[ast::Specifier::ThreadLocal],
            ty::array(
                ty::ident(qualified_template_ident(
                    false,
                    &["Point", "Init"],
                    &[
                        ast::TemplateSpecifier::Simple(ident("F")),
                        ast::TemplateSpecifier::Simple(ident("E")),
                    ],
                )),
                None,
            ),
            ident("coord"),
            expr::multiply(
                expr::add(expr::ident(ident("FOO")), expr::constant("5")),
                expr::member_access(expr::ident(ident("FOO")), ident("member")),
            ),
        )),
        global::decl(enumeration::def(
            true,
            Some(ident("CompileOptions")),
            &[
                enumeration::enumerator("Fast", Some(1)),
                enumeration::enumerator("Fun", None),
            ],
            None,
        )),
        global::ws(1),
        global::decl(class::def(
            ast::ClassKey::Class,
            Some(ident("Point3D")),
            &[],
            &[
                class::member_decl(var::decl(&[], ty::int(), ident("x"))),
                class::member_decl(var::decl(&[], ty::int(), ident("y"))),
                class::member_decl(var::decl(&[], ty::int(), ident("z"))),
            ],
            None,
        )),
        global::ws(1),
        global::decl(func::decl(
            None,
            &[ast::Specifier::Static],
            ty::void(),
            ident("do_work"),
            &[(Some(ident("hello")), ty::void())],
            None,
            ast::CvQualifier::None,
        )),
        global::ws(2),
    ]);

    file.write(io::stdout());
}
