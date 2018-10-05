use ast::*;

pub fn new() -> File {
    File {
        scope: GlobalSyntax { items: Vec::new() },
        nested_builders: Vec::new(),
    }
}

pub fn file(items: &[GlobalSyntaxItem]) -> File {
    File {
        scope: GlobalSyntax {
            items: items.to_vec(),
        },
        nested_builders: Vec::new(),
    }
}

pub fn ident<S: AsRef<str>>(name: S) -> QualifiedIdentifier {
    QualifiedIdentifier {
        global: false,
        segments: vec![IdentifierSegment {
            name: name.as_ref().to_owned(),
            template_args: Vec::new(),
        }],
    }
}

pub fn qualified_ident<S: AsRef<str>>(global: bool, names: &[S]) -> QualifiedIdentifier {
    QualifiedIdentifier {
        global,
        segments: names
            .iter()
            .map(|x| IdentifierSegment {
                name: x.as_ref().to_owned(),
                template_args: Vec::new(),
            }).collect(),
    }
}

pub fn qualified_template_ident<S: AsRef<str>>(
    global: bool,
    names: &[S],
    args: &[TemplateSpecifier],
) -> QualifiedIdentifier {
    assert!(!names.is_empty());

    let mut segments: Vec<_> = names
        .iter()
        .map(|x| IdentifierSegment {
            name: x.as_ref().to_owned(),
            template_args: Vec::new(),
        }).collect();
    segments.last_mut().unwrap().template_args = args.to_vec();

    QualifiedIdentifier { global, segments }
}

pub mod global {
    use ast::*;

    pub fn scope(items: &[GlobalSyntaxItem]) -> GlobalSyntax {
        GlobalSyntax {
            items: items.to_vec(),
        }
    }

    pub fn ws(lines: u32) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Whitespace(lines)
    }

    pub fn verbatim<S: AsRef<str>>(text: S) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Verbatim(text.as_ref().to_owned())
    }

    pub fn comment<S: AsRef<str>>(comment: S) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Comment(Comment(comment.as_ref().to_owned()))
    }

    pub fn include<S: AsRef<str>>(system: bool, path: S) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Include(Include {
            system,
            path: path.as_ref().to_owned(),
        })
    }

    pub fn define<S: AsRef<str>>(name: S, value: Option<S>) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Define(Define {
            name: name.as_ref().to_owned(),
            value: value.map(|x| x.as_ref().to_owned()),
        })
    }

    pub fn conditional(
        conditional: ConditionalExpression,
        items: &[GlobalSyntaxItem],
    ) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Conditional(GlobalConditional {
            cases: vec![(conditional, scope(items))],
            fallthrough: None,
        })
    }

    pub fn complex_conditional(
        cases: &[(ConditionalExpression, GlobalSyntax)],
        fallthrough: Option<GlobalSyntax>,
    ) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Conditional(GlobalConditional {
            cases: cases.to_vec(),
            fallthrough: fallthrough,
        })
    }

    pub fn using(ident: QualifiedIdentifier) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Using(Using {
            namespace: false,
            ident,
        })
    }

    pub fn using_namespace(ident: QualifiedIdentifier) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Using(Using {
            namespace: true,
            ident,
        })
    }

    pub fn typedef(decl: Declaration) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Typedef(decl)
    }

    pub fn decl(decl: Declaration) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Declaration(decl)
    }

    pub fn def(def: Definition) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Definition(def)
    }

    pub fn extern_c(items: &[GlobalSyntaxItem]) -> GlobalSyntaxItem {
        GlobalSyntaxItem::ExternC(scope(items))
    }

    pub fn namespace<S: AsRef<str>>(name: S, items: &[GlobalSyntaxItem]) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Namespace(Namespace {
            name: Some(name.as_ref().to_owned()),
            scope: scope(items),
        })
    }

    pub fn anonymous_namespace(items: &[GlobalSyntaxItem]) -> GlobalSyntaxItem {
        GlobalSyntaxItem::Namespace(Namespace {
            name: None,
            scope: scope(items),
        })
    }
}

pub mod func {
    use ast::*;
    use gen::ty;

    pub fn decl(
        prefix: Option<String>,
        specifiers: &[Specifier],
        ret: ty::Type,
        ident: QualifiedIdentifier,
        args: &[(Option<QualifiedIdentifier>, ty::Type)],
        postfix: Option<String>,
        cv: CvQualifier,
    ) -> Declaration {
        ty::to_function_declaration(prefix, specifiers, &ret, ident, args, postfix, cv)
    }

    pub fn def(
        specifiers: &[Specifier],
        ret: ty::Type,
        ident: QualifiedIdentifier,
        args: &[(Option<QualifiedIdentifier>, ty::Type)],
        items: &[FunctionSyntaxItem],
    ) -> Definition {
        Definition {
            decl: ty::to_function_declaration(
                None,
                specifiers,
                &ret,
                ident,
                args,
                None,
                CvQualifier::None,
            ),
            init: Initialization::FunctionBody(FunctionSyntax {
                items: items.to_vec(),
            }),
        }
    }

    pub fn scope(items: &[FunctionSyntaxItem]) -> FunctionSyntax {
        FunctionSyntax {
            items: items.to_vec(),
        }
    }

    pub fn ws(lines: u32) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Whitespace(lines)
    }

    pub fn verbatim<S: AsRef<str>>(text: S) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Verbatim(text.as_ref().to_owned())
    }

    pub fn comment<S: AsRef<str>>(comment: S) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Comment(Comment(comment.as_ref().to_owned()))
    }

    pub fn include<S: AsRef<str>>(system: bool, path: S) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Include(Include {
            system,
            path: path.as_ref().to_owned(),
        })
    }

    pub fn define<S: AsRef<str>>(name: S, value: Option<S>) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Define(Define {
            name: name.as_ref().to_owned(),
            value: value.map(|x| x.as_ref().to_owned()),
        })
    }

    pub fn conditional(
        conditional: ConditionalExpression,
        items: &[FunctionSyntaxItem],
    ) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Conditional(FunctionConditional {
            cases: vec![(conditional, scope(items))],
            fallthrough: None,
        })
    }

    pub fn complex_conditional(
        cases: &[(ConditionalExpression, FunctionSyntax)],
        fallthrough: Option<FunctionSyntax>,
    ) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Conditional(FunctionConditional {
            cases: cases.to_vec(),
            fallthrough: fallthrough,
        })
    }

    pub fn using(ident: QualifiedIdentifier) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Using(Using {
            namespace: false,
            ident,
        })
    }

    pub fn using_namespace(ident: QualifiedIdentifier) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Using(Using {
            namespace: true,
            ident,
        })
    }

    pub fn stmt_typedef(decl: Declaration) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Typedef(decl)
    }

    pub fn stmt_decl(decl: Declaration) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Declaration(decl)
    }

    pub fn stmt_def(def: Definition) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Definition(def)
    }

    pub fn stmt_expr(expr: Expression) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Expression(expr)
    }

    pub fn stmt_if(
        expr: Expression,
        true_case: &[FunctionSyntaxItem],
        false_case: Option<&[FunctionSyntaxItem]>,
    ) -> FunctionSyntaxItem {
        FunctionSyntaxItem::If(expr, scope(true_case), false_case.map(|x| scope(x)))
    }

    pub fn stmt_for(
        init: Definition,
        condition: Expression,
        increment: Expression,
        body: &[FunctionSyntaxItem],
    ) -> FunctionSyntaxItem {
        FunctionSyntaxItem::For(init, condition, increment, scope(body))
    }

    pub fn stmt_while(condition: Expression, body: &[FunctionSyntaxItem]) -> FunctionSyntaxItem {
        FunctionSyntaxItem::While(condition, scope(body))
    }

    pub fn stmt_ret(expr: Expression) -> FunctionSyntaxItem {
        FunctionSyntaxItem::Return(expr)
    }
}

pub mod class {
    use ast::*;

    pub fn decl(key: ClassKey, tag: QualifiedIdentifier) -> Declaration {
        Declaration {
            prefix: None,
            specifiers: Vec::new(),
            ty_specifier: TypeSpecifier::Class(key, tag),
            declarator: Declarator::None,
            postfix: None,
        }
    }

    pub fn def(
        key: ClassKey,
        tag: Option<QualifiedIdentifier>,
        template_args: &[TemplateArgument],
        items: &[ClassSyntaxItem],
        name: Option<QualifiedIdentifier>,
    ) -> Declaration {
        let class = Class {
            key,
            tag,
            template_args: template_args.to_vec(),
            scope: scope(items),
        };

        Declaration {
            prefix: None,
            specifiers: Vec::new(),
            ty_specifier: TypeSpecifier::ElaboratedClass(class),
            declarator: name
                .map(|x| Declarator::Identifier(x))
                .unwrap_or(Declarator::None),
            postfix: None,
        }
    }

    pub fn scope(items: &[ClassSyntaxItem]) -> ClassSyntax {
        ClassSyntax {
            items: items.to_vec(),
        }
    }

    pub fn ws(lines: u32) -> ClassSyntaxItem {
        ClassSyntaxItem::Whitespace(lines)
    }

    pub fn verbatim<S: AsRef<str>>(text: S) -> ClassSyntaxItem {
        ClassSyntaxItem::Verbatim(text.as_ref().to_owned())
    }

    pub fn comment<S: AsRef<str>>(comment: S) -> ClassSyntaxItem {
        ClassSyntaxItem::Comment(Comment(comment.as_ref().to_owned()))
    }

    pub fn include<S: AsRef<str>>(system: bool, path: S) -> ClassSyntaxItem {
        ClassSyntaxItem::Include(Include {
            system,
            path: path.as_ref().to_owned(),
        })
    }

    pub fn define<S: AsRef<str>>(name: S, value: Option<S>) -> ClassSyntaxItem {
        ClassSyntaxItem::Define(Define {
            name: name.as_ref().to_owned(),
            value: value.map(|x| x.as_ref().to_owned()),
        })
    }

    pub fn conditional(
        conditional: ConditionalExpression,
        items: &[ClassSyntaxItem],
    ) -> ClassSyntaxItem {
        ClassSyntaxItem::Conditional(ClassConditional {
            cases: vec![(conditional, scope(items))],
            fallthrough: None,
        })
    }

    pub fn complex_conditional(
        cases: &[(ConditionalExpression, ClassSyntax)],
        fallthrough: Option<ClassSyntax>,
    ) -> ClassSyntaxItem {
        ClassSyntaxItem::Conditional(ClassConditional {
            cases: cases.to_vec(),
            fallthrough: fallthrough,
        })
    }

    pub fn using(ident: QualifiedIdentifier) -> ClassSyntaxItem {
        ClassSyntaxItem::Using(Using {
            namespace: false,
            ident,
        })
    }

    pub fn using_namespace(ident: QualifiedIdentifier) -> ClassSyntaxItem {
        ClassSyntaxItem::Using(Using {
            namespace: true,
            ident,
        })
    }

    pub fn member_typedef(decl: Declaration) -> ClassSyntaxItem {
        ClassSyntaxItem::Typedef(decl)
    }

    pub fn member_decl(decl: Declaration) -> ClassSyntaxItem {
        ClassSyntaxItem::Declaration(decl)
    }

    pub fn member_def(def: Definition) -> ClassSyntaxItem {
        ClassSyntaxItem::Definition(def)
    }

    pub fn member_public() -> ClassSyntaxItem {
        ClassSyntaxItem::MemberSpecification(MemberSpecification::Public)
    }

    pub fn member_private() -> ClassSyntaxItem {
        ClassSyntaxItem::MemberSpecification(MemberSpecification::Private)
    }

    pub fn member_protected() -> ClassSyntaxItem {
        ClassSyntaxItem::MemberSpecification(MemberSpecification::Protected)
    }
}

pub mod enumeration {
    use ast::*;

    pub fn decl(tag: QualifiedIdentifier) -> Declaration {
        Declaration {
            prefix: None,
            specifiers: Vec::new(),
            ty_specifier: TypeSpecifier::Enum(tag),
            declarator: Declarator::None,
            postfix: None,
        }
    }

    pub fn def(
        scoped: bool,
        tag: Option<QualifiedIdentifier>,
        items: &[EnumSyntaxItem],
        name: Option<QualifiedIdentifier>,
    ) -> Declaration {
        let enumeration = Enum {
            scoped,
            tag,
            scope: scope(items),
        };

        Declaration {
            prefix: None,
            specifiers: Vec::new(),
            ty_specifier: TypeSpecifier::ElaboratedEnum(enumeration),
            declarator: name
                .map(|x| Declarator::Identifier(x))
                .unwrap_or(Declarator::None),
            postfix: None,
        }
    }

    pub fn scope(items: &[EnumSyntaxItem]) -> EnumSyntax {
        EnumSyntax {
            items: items.to_vec(),
        }
    }

    pub fn ws(lines: u32) -> EnumSyntaxItem {
        EnumSyntaxItem::Whitespace(lines)
    }

    pub fn verbatim<S: AsRef<str>>(text: S) -> EnumSyntaxItem {
        EnumSyntaxItem::Verbatim(text.as_ref().to_owned())
    }

    pub fn comment<S: AsRef<str>>(comment: S) -> EnumSyntaxItem {
        EnumSyntaxItem::Comment(Comment(comment.as_ref().to_owned()))
    }

    pub fn include<S: AsRef<str>>(system: bool, path: S) -> EnumSyntaxItem {
        EnumSyntaxItem::Include(Include {
            system,
            path: path.as_ref().to_owned(),
        })
    }

    pub fn define<S: AsRef<str>>(name: S, value: Option<S>) -> EnumSyntaxItem {
        EnumSyntaxItem::Define(Define {
            name: name.as_ref().to_owned(),
            value: value.map(|x| x.as_ref().to_owned()),
        })
    }

    pub fn conditional(
        conditional: ConditionalExpression,
        items: &[EnumSyntaxItem],
    ) -> EnumSyntaxItem {
        EnumSyntaxItem::Conditional(EnumConditional {
            cases: vec![(conditional, scope(items))],
            fallthrough: None,
        })
    }

    pub fn complex_conditional(
        cases: &[(ConditionalExpression, EnumSyntax)],
        fallthrough: Option<EnumSyntax>,
    ) -> EnumSyntaxItem {
        EnumSyntaxItem::Conditional(EnumConditional {
            cases: cases.to_vec(),
            fallthrough: fallthrough,
        })
    }

    pub fn enumerator<S: AsRef<str>>(name: S, value: Option<isize>) -> EnumSyntaxItem {
        EnumSyntaxItem::Enumerator(Enumerator {
            name: name.as_ref().to_owned(),
            value,
        })
    }
}

pub mod conditional {
    use ast::*;

    pub fn defined<S: AsRef<str>>(define: S) -> ConditionalExpression {
        ConditionalExpression::Defined(define.as_ref().to_owned())
    }

    pub fn constant<S: AsRef<str>>(constant: S) -> ConditionalExpression {
        ConditionalExpression::Constant(constant.as_ref().to_owned())
    }

    pub fn and(x: ConditionalExpression, y: ConditionalExpression) -> ConditionalExpression {
        ConditionalExpression::And(Box::new(x), Box::new(y))
    }

    pub fn or(x: ConditionalExpression, y: ConditionalExpression) -> ConditionalExpression {
        ConditionalExpression::Or(Box::new(x), Box::new(y))
    }

    pub fn not(x: ConditionalExpression) -> ConditionalExpression {
        ConditionalExpression::Not(Box::new(x))
    }
}

pub mod var {
    use ast::*;
    use gen::ty;

    pub fn decl(specifiers: &[Specifier], ty: ty::Type, ident: QualifiedIdentifier) -> Declaration {
        ty::to_named_declaration(specifiers, &ty, ident)
    }

    pub fn def(
        specifiers: &[Specifier],
        ty: ty::Type,
        ident: QualifiedIdentifier,
        expr: Expression,
    ) -> Definition {
        Definition {
            decl: ty::to_named_declaration(specifiers, &ty, ident),
            init: Initialization::Expression(expr),
        }
    }
}

pub mod ty {
    use ast::*;
    use gen;

    #[derive(Clone, Copy, Debug)]
    pub enum IdentifierKey {
        None,
        Class,
        Struct,
        Union,
        Enum,
    }

    #[derive(Clone, Debug)]
    pub enum Type {
        Identifier(IdentifierKey, QualifiedIdentifier),
        Pointer(Box<Type>),
        ConstPointer(Box<Type>),
        Reference(Box<Type>),
        MoveReference(Box<Type>),
        Array(Box<Type>, Option<usize>),
        FunctionPointer(Box<Type>, Vec<Type>),
    }

    macro_rules! primitive {
        ($name:tt) => {
            pub fn $name() -> Type {
                Type::Identifier(IdentifierKey::None, gen::ident(stringify!($name)))
            }
        };
    }

    primitive!(void);
    primitive!(short);
    primitive!(int);
    primitive!(long);
    primitive!(char);
    primitive!(float);
    primitive!(double);

    pub fn ident_class(ident: QualifiedIdentifier) -> Type {
        Type::Identifier(IdentifierKey::Class, ident)
    }

    pub fn ident_union(ident: QualifiedIdentifier) -> Type {
        Type::Identifier(IdentifierKey::Union, ident)
    }

    pub fn ident_struct(ident: QualifiedIdentifier) -> Type {
        Type::Identifier(IdentifierKey::Struct, ident)
    }

    pub fn ident_enum(ident: QualifiedIdentifier) -> Type {
        Type::Identifier(IdentifierKey::Enum, ident)
    }

    pub fn ident(ident: QualifiedIdentifier) -> Type {
        Type::Identifier(IdentifierKey::None, ident)
    }

    pub fn ident_key(key: IdentifierKey, ident: QualifiedIdentifier) -> Type {
        Type::Identifier(key, ident)
    }

    pub fn ident_str<S: AsRef<str>>(value: S) -> Type {
        Type::Identifier(IdentifierKey::None, gen::ident(value.as_ref().to_owned()))
    }

    pub fn ptr(ty: Type) -> Type {
        Type::Pointer(Box::new(ty))
    }

    pub fn const_ptr(ty: Type) -> Type {
        Type::ConstPointer(Box::new(ty))
    }

    pub fn reference(ty: Type) -> Type {
        Type::Reference(Box::new(ty))
    }

    pub fn move_reference(ty: Type) -> Type {
        Type::MoveReference(Box::new(ty))
    }

    pub fn array(ty: Type, size: Option<usize>) -> Type {
        Type::Array(Box::new(ty), size)
    }

    pub fn fn_ptr(ret: Type, args: &[Type]) -> Type {
        Type::FunctionPointer(Box::new(ret), args.to_vec())
    }

    pub(crate) fn to_function_declaration(
        prefix: Option<String>,
        specifiers: &[Specifier],
        ret: &Type,
        ident: QualifiedIdentifier,
        args: &[(Option<QualifiedIdentifier>, Type)],
        postfix: Option<String>,
        cv: CvQualifier,
    ) -> Declaration {
        let base = Declarator::Function(
            Box::new(Declarator::Identifier(ident)),
            args.iter()
                .map(|&(ref ident, ref ty)| {
                    if let &Some(ref ident) = ident {
                        to_named_declaration(&[], ty, ident.clone())
                    } else {
                        to_unnamed_declaration(&[], ty)
                    }
                }).collect(),
            cv,
        );

        let ty_spec = get_ty_specifier(ret);
        let declarator = build_declarator(ret, base);
        let specs = specifiers.to_vec();

        Declaration {
            prefix,
            specifiers: specs,
            ty_specifier: ty_spec,
            declarator,
            postfix,
        }
    }

    pub(crate) fn to_named_declaration(
        specifiers: &[Specifier],
        ty: &Type,
        ident: QualifiedIdentifier,
    ) -> Declaration {
        let ty_spec = get_ty_specifier(ty);
        let declarator = build_declarator(ty, Declarator::Identifier(ident));
        let specs = specifiers.to_vec();

        Declaration {
            prefix: None,
            specifiers: specs,
            ty_specifier: ty_spec,
            declarator,
            postfix: None,
        }
    }

    pub(crate) fn to_unnamed_declaration(specifiers: &[Specifier], ty: &Type) -> Declaration {
        let ty_spec = get_ty_specifier(ty);
        let declarator = build_declarator(ty, Declarator::None);
        let specs = specifiers.to_vec();

        Declaration {
            prefix: None,
            specifiers: specs,
            ty_specifier: ty_spec,
            declarator,
            postfix: None,
        }
    }

    pub(crate) fn get_ty_specifier(ty: &Type) -> TypeSpecifier {
        match ty {
            &Type::Identifier(ref key, ref ident) => match key {
                &IdentifierKey::Class => TypeSpecifier::Class(ClassKey::Class, ident.clone()),
                &IdentifierKey::Struct => TypeSpecifier::Class(ClassKey::Struct, ident.clone()),
                &IdentifierKey::Union => TypeSpecifier::Class(ClassKey::Union, ident.clone()),
                &IdentifierKey::Enum => TypeSpecifier::Enum(ident.clone()),
                &IdentifierKey::None => TypeSpecifier::Simple(ident.clone()),
            },
            &Type::Pointer(ref ty) => get_ty_specifier(ty),
            &Type::ConstPointer(ref ty) => get_ty_specifier(ty),
            &Type::Reference(ref ty) => get_ty_specifier(ty),
            &Type::MoveReference(ref ty) => get_ty_specifier(ty),
            &Type::Array(ref ty, _) => get_ty_specifier(ty),
            &Type::FunctionPointer(ref ret, _) => get_ty_specifier(ret),
        }
    }

    pub(crate) fn build_declarator(ty: &Type, base_declarator: Declarator) -> Declarator {
        match ty {
            &Type::Identifier(..) => base_declarator,
            &Type::Pointer(ref ty) => {
                Declarator::Pointer(Box::new(build_declarator(ty, base_declarator)))
            }
            &Type::ConstPointer(ref ty) => Declarator::Cv(
                CvQualifier::Const,
                Box::new(Declarator::Pointer(Box::new(build_declarator(
                    ty,
                    base_declarator,
                )))),
            ),
            &Type::Reference(ref ty) => {
                Declarator::Reference(Box::new(build_declarator(ty, base_declarator)))
            }
            &Type::MoveReference(ref ty) => {
                Declarator::MoveReference(Box::new(build_declarator(ty, base_declarator)))
            }
            &Type::Array(ref ty, size) => {
                Declarator::Array(Box::new(build_declarator(ty, base_declarator)), size)
            }
            &Type::FunctionPointer(ref ret, ref args) => build_declarator(
                ret,
                Declarator::Function(
                    Box::new(Declarator::Pointer(Box::new(base_declarator))),
                    args.iter()
                        .map(|x| to_unnamed_declaration(&[], x))
                        .collect(),
                    CvQualifier::None,
                ),
            ),
        }
    }
}

pub mod expr {
    use ast::*;

    pub fn ident(ident: QualifiedIdentifier) -> Expression {
        Expression::Identifier(ident)
    }

    pub fn constant<S: AsRef<str>>(value: S) -> Expression {
        Expression::Constant(value.as_ref().to_owned())
    }

    pub fn and(left: Expression, right: Expression) -> Expression {
        Expression::And(Box::new(left), Box::new(right))
    }

    pub fn or(left: Expression, right: Expression) -> Expression {
        Expression::Or(Box::new(left), Box::new(right))
    }

    pub fn not(expr: Expression) -> Expression {
        Expression::Not(Box::new(expr))
    }

    pub fn eq(left: Expression, right: Expression) -> Expression {
        Expression::Equal(Box::new(left), Box::new(right))
    }

    pub fn neq(left: Expression, right: Expression) -> Expression {
        Expression::NotEqual(Box::new(left), Box::new(right))
    }

    pub fn lt(left: Expression, right: Expression) -> Expression {
        Expression::LessThan(Box::new(left), Box::new(right))
    }

    pub fn lte(left: Expression, right: Expression) -> Expression {
        Expression::LessThanEqual(Box::new(left), Box::new(right))
    }

    pub fn gt(left: Expression, right: Expression) -> Expression {
        Expression::GreaterThan(Box::new(left), Box::new(right))
    }

    pub fn gte(left: Expression, right: Expression) -> Expression {
        Expression::GreaterThanEqual(Box::new(left), Box::new(right))
    }

    pub fn add(left: Expression, right: Expression) -> Expression {
        Expression::Add(Box::new(left), Box::new(right))
    }

    pub fn subtract(left: Expression, right: Expression) -> Expression {
        Expression::Subtract(Box::new(left), Box::new(right))
    }

    pub fn multiply(left: Expression, right: Expression) -> Expression {
        Expression::Multiply(Box::new(left), Box::new(right))
    }

    pub fn divide(left: Expression, right: Expression) -> Expression {
        Expression::Divide(Box::new(left), Box::new(right))
    }

    pub fn assign(left: Expression, right: Expression) -> Expression {
        Expression::Assign(Box::new(left), Box::new(right))
    }

    pub fn post_increment(expr: Expression) -> Expression {
        Expression::PostIncrement(Box::new(expr))
    }

    pub fn pre_increment(expr: Expression) -> Expression {
        Expression::PreIncrement(Box::new(expr))
    }

    pub fn member_access(expr: Expression, member: QualifiedIdentifier) -> Expression {
        Expression::MemberAccess(Box::new(expr), member)
    }

    pub fn ptr_member_access(expr: Expression, member: QualifiedIdentifier) -> Expression {
        Expression::PointerMemberAccess(Box::new(expr), member)
    }

    pub fn subscript(left: Expression, right: Expression) -> Expression {
        Expression::Subscript(Box::new(left), Box::new(right))
    }

    pub fn address_of(expr: Expression) -> Expression {
        Expression::AddressOf(Box::new(expr))
    }

    pub fn call(expr: Expression, args: &[Expression]) -> Expression {
        Expression::Call(Box::new(expr), args.to_vec())
    }
}
