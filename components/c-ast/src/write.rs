use std::io::Write;

use ast::*;
use writer::Writer;

impl File {
    pub fn to_string(&self) -> String {
        let mut out = Vec::new();
        self.write(&mut out);
        String::from_utf8(out).unwrap()
    }

    pub fn write<W: Write>(&self, out: W) {
        assert!(self.nested_builders.is_empty());
        let mut writer = Writer::new(out);
        self.scope.write(&mut writer);
    }
}

impl QualifiedIdentifier {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        let mut first = true;
        if self.global {
            first = false;
        }
        for segment in &self.segments {
            if first {
                write!(out, "{}", segment.name);
                first = false;
            } else {
                write!(out, "::{}", segment.name);
            }

            if !segment.template_args.is_empty() {
                let mut first_arg = true;

                write!(out, "<");
                for arg in &segment.template_args {
                    if first_arg {
                        first_arg = false;
                    } else {
                        write!(out, ", ");
                    }

                    match arg {
                        &TemplateSpecifier::Simple(ref x) => {
                            write!(out, "typename ");
                            x.write(out);
                        }
                        &TemplateSpecifier::Class(key, ref x) => {
                            key.write(out);
                            x.write(out);
                        }
                        &TemplateSpecifier::Enum(ref x) => {
                            write!(out, "enum ");
                            x.write(out);
                        }
                    }
                }
                write!(out, ">");
            }
        }
    }
}

impl Comment {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        for line in self.0.lines() {
            writeln!(out, "// {}", line);
        }
    }
}

impl Include {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if self.system {
            writeln!(out, "#include <{}>", self.path);
        } else {
            writeln!(out, "#include \"{}\"", self.path);
        }
    }
}

impl Define {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if let Some(value) = self.value.as_ref() {
            writeln!(out, "#define {} {}", self.name, value);
        } else {
            writeln!(out, "#define {}", self.name);
        }
    }
}

impl ConditionalExpression {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &ConditionalExpression::Defined(ref x) => {
                write!(out, "defined({})", x);
            }
            &ConditionalExpression::Constant(ref x) => {
                write!(out, "{}", x);
            }
            &ConditionalExpression::Not(ref x) => {
                write!(out, "!");

                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }
            }
            &ConditionalExpression::And(ref x, ref y) => {
                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }

                write!(out, " && ");

                if self.precedence() > y.precedence() {
                    write!(out, "(");
                    y.write(out);
                    write!(out, ")");
                } else {
                    y.write(out);
                }
            }
            &ConditionalExpression::Or(ref x, ref y) => {
                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }

                write!(out, " || ");

                if self.precedence() > y.precedence() {
                    write!(out, "(");
                    y.write(out);
                    write!(out, ")");
                } else {
                    y.write(out);
                }
            }
        }
    }

    fn precedence(&self) -> usize {
        match self {
            &ConditionalExpression::Defined(..) => 3,
            &ConditionalExpression::Constant(..) => 3,
            &ConditionalExpression::Not(..) => 3,
            &ConditionalExpression::And(..) => 2,
            &ConditionalExpression::Or(..) => 1,
        }
    }
}

impl GlobalConditional {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        assert!(!self.cases.is_empty());

        let mut first = true;

        for case in &self.cases {
            if first {
                write!(out, "#if ");
                case.0.write(out);
                writeln!(out);
                first = false;
            } else {
                writeln!(out, "#elif ");
                case.0.write(out);
                writeln!(out);
            }
            case.1.write(out);
        }

        if let Some(fallthrough) = self.fallthrough.as_ref() {
            writeln!(out, "#else");
            fallthrough.write(out);
        }
        writeln!(out, "#endif");
    }
}

impl FunctionConditional {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        assert!(!self.cases.is_empty());

        let mut first = true;

        for case in &self.cases {
            if first {
                write!(out, "#if ");
                case.0.write(out);
                writeln!(out);
                first = false;
            } else {
                writeln!(out, "#elif ");
                case.0.write(out);
                writeln!(out);
            }
            case.1.write(out);
        }

        if let Some(fallthrough) = self.fallthrough.as_ref() {
            writeln!(out, "#else");
            fallthrough.write(out);
        }
        writeln!(out, "#endif");
    }
}

impl ClassConditional {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        assert!(!self.cases.is_empty());

        let mut first = true;

        for case in &self.cases {
            if first {
                write!(out, "#if ");
                case.0.write(out);
                writeln!(out);
                first = false;
            } else {
                writeln!(out, "#elif ");
                case.0.write(out);
                writeln!(out);
            }
            case.1.write(out);
        }

        if let Some(fallthrough) = self.fallthrough.as_ref() {
            writeln!(out, "#else");
            fallthrough.write(out);
        }
        writeln!(out, "#endif");
    }
}

impl EnumConditional {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        assert!(!self.cases.is_empty());

        let mut first = true;

        for case in &self.cases {
            if first {
                write!(out, "#if ");
                case.0.write(out);
                writeln!(out);
                first = false;
            } else {
                writeln!(out, "#elif ");
                case.0.write(out);
                writeln!(out);
            }
            case.1.write(out);
        }

        if let Some(fallthrough) = self.fallthrough.as_ref() {
            writeln!(out, "#else");
            fallthrough.write(out);
        }
        writeln!(out, "#endif");
    }
}

impl Using {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if self.namespace {
            write!(out, "using namespace ");
        } else {
            write!(out, "using ");
        }
        self.ident.write(out);
        writeln!(out, ";");
    }
}

impl Declaration {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if let Some(prefix) = self.prefix.as_ref() {
            write!(out, "{} ", prefix);
        }
        for specifier in &self.specifiers {
            specifier.write(out);
            write!(out, " ");
        }
        self.ty_specifier.write(out);
        if self.has_name() {
            write!(out, " ");
        }
        self.declarator.write(out);
        if let Some(postfix) = self.postfix.as_ref() {
            write!(out, " {}", postfix);
        }
    }

    fn has_name(&self) -> bool {
        let mut declarator = &self.declarator;
        loop {
            match declarator {
                &Declarator::None => {
                    return false;
                }
                &Declarator::Identifier(..) => {
                    return true;
                }
                &Declarator::Cv(_, ref decl) => {
                    declarator = decl;
                }
                &Declarator::Pointer(ref decl) => {
                    declarator = decl;
                }
                &Declarator::Reference(ref decl) => {
                    declarator = decl;
                }
                &Declarator::MoveReference(ref decl) => {
                    declarator = decl;
                }
                &Declarator::Array(ref decl, _) => {
                    declarator = decl;
                }
                &Declarator::Function(ref ret, _, _) => {
                    declarator = ret;
                }
            }
        }
    }
}

impl Specifier {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &Specifier::Static => write!(out, "static"),
            &Specifier::ThreadLocal => write!(out, "thread_local"),
            &Specifier::Extern => write!(out, "extern"),
            &Specifier::Mutable => write!(out, "mutable"),
            &Specifier::Const => write!(out, "const"),
            &Specifier::Volatile => write!(out, "volatile"),
        }
    }
}

impl TypeSpecifier {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &TypeSpecifier::Simple(ref x) => {
                x.write(out);
            }
            &TypeSpecifier::Class(ref key, ref x) => {
                key.write(out);
                write!(out, " ");
                x.write(out);
            }
            &TypeSpecifier::Enum(ref x) => {
                write!(out, "enum ");
                x.write(out);
            }
            &TypeSpecifier::ElaboratedClass(ref x) => {
                x.write(out);
            }
            &TypeSpecifier::ElaboratedEnum(ref x) => {
                x.write(out);
            }
        }
    }
}

impl Declarator {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &Declarator::None => {}
            &Declarator::Identifier(ref x) => {
                x.write(out);
            }
            &Declarator::Cv(ref cv, ref x) => {
                assert!(*cv != CvQualifier::None);
                if *cv != CvQualifier::None {
                    cv.write(out);
                    write!(out, " ");
                }
                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }
            }
            &Declarator::Pointer(ref x) => {
                write!(out, "*");
                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }
            }
            &Declarator::Reference(ref x) => {
                write!(out, "&");

                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }
            }
            &Declarator::MoveReference(ref x) => {
                write!(out, "&&");

                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }
            }
            &Declarator::Array(ref x, ref size) => {
                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }

                if let Some(size) = *size {
                    write!(out, "[{}]", size);
                } else {
                    write!(out, "[]");
                }
            }
            &Declarator::Function(ref x, ref args, ref cv) => {
                if self.precedence() > x.precedence() {
                    write!(out, "(");
                    x.write(out);
                    write!(out, ")");
                } else {
                    x.write(out);
                }

                write!(out, "(");
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(out, ", ");
                    }
                    first = false;

                    arg.write(out);
                }
                write!(out, ")");

                if *cv != CvQualifier::None {
                    write!(out, " ");
                    cv.write(out);
                }
            }
        }
    }

    fn precedence(&self) -> usize {
        match self {
            &Declarator::None => 3,
            &Declarator::Identifier(..) => 3,
            &Declarator::Cv(..) => 1,
            &Declarator::Pointer(..) => 1,
            &Declarator::Reference(..) => 1,
            &Declarator::MoveReference(..) => 1,
            &Declarator::Array(..) => 2,
            &Declarator::Function(..) => 2,
        }
    }
}

impl CvQualifier {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &CvQualifier::None => (),
            &CvQualifier::Const => write!(out, "const"),
            &CvQualifier::Volatile => write!(out, "volatile"),
        }
    }
}

impl Definition {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        self.decl.write(out);
        self.init.write(out);
    }
}

impl Initialization {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &Initialization::Expression(ref x) => {
                write!(out, " = ");
                x.write(out);
                writeln!(out, ";");
            }
            &Initialization::FunctionBody(ref x) => {
                write!(out, " ");
                x.write(out);
            }
        }
    }
}

impl Expression {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        macro_rules! sub_expr {
            ($x:expr) => {
                if self.precedence() > $x.precedence() {
                    write!(out, "(");
                    $x.write(out);
                    write!(out, ")");
                } else {
                    $x.write(out);
                }
            };
        }

        match self {
            &Expression::Identifier(ref x) => {
                x.write(out);
            }
            &Expression::Constant(ref x) => {
                write!(out, "{}", x);
            }
            &Expression::And(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " && ");
                sub_expr!(y);
            }
            &Expression::Or(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " || ");
                sub_expr!(y);
            }
            &Expression::Not(ref x) => {
                write!(out, "!");
                sub_expr!(x);
            }
            &Expression::Equal(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " == ");
                sub_expr!(y);
            }
            &Expression::NotEqual(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " != ");
                sub_expr!(y);
            }
            &Expression::LessThan(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " < ");
                sub_expr!(y);
            }
            &Expression::LessThanEqual(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " <= ");
                sub_expr!(y);
            }
            &Expression::GreaterThan(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " > ");
                sub_expr!(y);
            }
            &Expression::GreaterThanEqual(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " >= ");
                sub_expr!(y);
            }
            &Expression::Add(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " + ");
                sub_expr!(y);
            }
            &Expression::Subtract(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " - ");
                sub_expr!(y);
            }
            &Expression::Multiply(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " * ");
                sub_expr!(y);
            }
            &Expression::Divide(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " / ");
                sub_expr!(y);
            }
            &Expression::Assign(ref x, ref y) => {
                sub_expr!(x);
                write!(out, " = ");
                sub_expr!(y);
            }
            &Expression::PostIncrement(ref x) => {
                sub_expr!(x);
                write!(out, "++");
            }
            &Expression::PreIncrement(ref x) => {
                write!(out, "++");
                sub_expr!(x);
            }
            &Expression::MemberAccess(ref x, ref ident) => {
                sub_expr!(x);
                write!(out, ".");
                ident.write(out);
            }
            &Expression::PointerMemberAccess(ref x, ref ident) => {
                sub_expr!(x);
                write!(out, "->");
                ident.write(out);
            }
            &Expression::Subscript(ref x, ref y) => {
                sub_expr!(x);
                write!(out, "[");
                y.write(out);
                write!(out, "]");
            }
            &Expression::AddressOf(ref x) => {
                write!(out, "&");
                sub_expr!(x);
            }
            &Expression::Call(ref x, ref args) => {
                sub_expr!(x);
                write!(out, "(");
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(out, ", ");
                    }
                    first = false;

                    arg.write(out);
                }
                write!(out, ")");
            }
        }
    }

    fn precedence(&self) -> usize {
        // Reference:
        // https://en.cppreference.com/w/cpp/language/operator_precedence
        match self {
            &Expression::Identifier(..) => 9,
            &Expression::Constant(..) => 9,
            &Expression::And(..) => 2,
            &Expression::Or(..) => 2,
            &Expression::Not(..) => 7,
            &Expression::Equal(..) => 3,
            &Expression::NotEqual(..) => 3,
            &Expression::LessThan(..) => 4,
            &Expression::LessThanEqual(..) => 4,
            &Expression::GreaterThan(..) => 4,
            &Expression::GreaterThanEqual(..) => 4,
            &Expression::Add(..) => 5,
            &Expression::Subtract(..) => 5,
            &Expression::Multiply(..) => 6,
            &Expression::Divide(..) => 7,
            &Expression::Assign(..) => 1,
            &Expression::PostIncrement(..) => 8,
            &Expression::PreIncrement(..) => 7,
            &Expression::MemberAccess(..) => 8,
            &Expression::PointerMemberAccess(..) => 8,
            &Expression::Subscript(..) => 8,
            &Expression::AddressOf(..) => 7,
            &Expression::Call(..) => 8,
        }
    }
}

impl GlobalSyntax {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        for item in &self.items {
            item.write(out);
        }
    }
}

impl GlobalSyntaxItem {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &GlobalSyntaxItem::Whitespace(lines) => out.new_lines(lines),
            &GlobalSyntaxItem::Verbatim(ref x) => write!(out, "{}", x),
            &GlobalSyntaxItem::Comment(ref x) => x.write(out),
            &GlobalSyntaxItem::Include(ref x) => x.write(out),
            &GlobalSyntaxItem::Define(ref x) => x.write(out),
            &GlobalSyntaxItem::Conditional(ref x) => x.write(out),
            &GlobalSyntaxItem::Using(ref x) => x.write(out),
            &GlobalSyntaxItem::Typedef(ref x) => {
                write!(out, "typedef ");
                x.write(out);
                writeln!(out, ";");
            }
            &GlobalSyntaxItem::Declaration(ref x) => {
                x.write(out);
                writeln!(out, ";");
            }
            &GlobalSyntaxItem::Definition(ref x) => x.write(out),

            &GlobalSyntaxItem::ExternC(ref x) => {
                write!(out, "extern \"C\"");
                out.open_brace();
                x.write(out);
                out.close_brace();
                out.new_line();
            }
            &GlobalSyntaxItem::Namespace(ref x) => x.write(out),
        }
    }
}

impl Namespace {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if let Some(name) = self.name.as_ref() {
            write!(out, "namespace {}", name);
        } else {
            write!(out, "namespace");
        }
        out.open_brace();
        self.scope.write(out);
        out.close_brace();
        out.new_line();
    }
}

impl FunctionSyntax {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        for item in &self.items {
            item.write(out);
        }
    }
}

impl FunctionSyntaxItem {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &FunctionSyntaxItem::Whitespace(lines) => out.new_lines(lines),
            &FunctionSyntaxItem::Verbatim(ref x) => write!(out, "{}", x),
            &FunctionSyntaxItem::Comment(ref x) => x.write(out),
            &FunctionSyntaxItem::Include(ref x) => x.write(out),
            &FunctionSyntaxItem::Define(ref x) => x.write(out),
            &FunctionSyntaxItem::Conditional(ref x) => x.write(out),
            &FunctionSyntaxItem::Using(ref x) => x.write(out),
            &FunctionSyntaxItem::Typedef(ref x) => {
                write!(out, "typedef ");
                x.write(out);
                writeln!(out, ";");
            }
            &FunctionSyntaxItem::Declaration(ref x) => {
                x.write(out);
                writeln!(out, ";");
            }
            &FunctionSyntaxItem::Definition(ref x) => x.write(out),

            &FunctionSyntaxItem::Block(ref x) => {
                out.open_brace();
                x.write(out);
                out.close_brace();
                out.new_line();
            }
            &FunctionSyntaxItem::Expression(ref expr) => {
                expr.write(out);
                out.new_line();
            }
            &FunctionSyntaxItem::If(ref condition, ref truth, ref falth) => {
                write!(out, "if (");
                condition.write(out);
                write!(out, ") ");

                out.open_brace();
                truth.write(out);
                out.close_brace();
                out.new_line();

                if let Some(falth) = falth.as_ref() {
                    write!(out, " else ");
                    out.open_brace();
                    falth.write(out);
                    out.close_brace();
                    out.new_line();
                }
            }
            &FunctionSyntaxItem::For(ref init, ref condition, ref increment, ref body) => {
                write!(out, "for (");
                init.write(out);
                write!(out, "; ");
                condition.write(out);
                write!(out, "; ");
                increment.write(out);
                write!(out, ") ");

                out.open_brace();
                body.write(out);
                out.close_brace();
                out.new_line();
            }
            &FunctionSyntaxItem::While(ref condition, ref body) => {
                write!(out, "while (");
                condition.write(out);
                write!(out, ") ");

                out.open_brace();
                body.write(out);
                out.close_brace();
                out.new_line();
            }
            &FunctionSyntaxItem::Return(ref x) => {
                write!(out, "return ");
                x.write(out);
                writeln!(out, ";");
            }
        }
    }
}

impl Class {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        self.key.write(out);
        write!(out, " ");
        if let Some(tag) = self.tag.as_ref() {
            tag.write(out);
        }
        out.open_brace();
        self.scope.write(out);
        out.close_brace();
    }
}

impl ClassKey {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &ClassKey::Struct => write!(out, "struct"),
            &ClassKey::Class => write!(out, "class"),
            &ClassKey::Union => write!(out, "union"),
        }
    }
}

impl ClassSyntax {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        for item in &self.items {
            item.write(out);
        }
    }
}

impl ClassSyntaxItem {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &ClassSyntaxItem::Whitespace(lines) => out.new_lines(lines),
            &ClassSyntaxItem::Verbatim(ref x) => write!(out, "{}", x),
            &ClassSyntaxItem::Comment(ref x) => x.write(out),
            &ClassSyntaxItem::Include(ref x) => x.write(out),
            &ClassSyntaxItem::Define(ref x) => x.write(out),
            &ClassSyntaxItem::Conditional(ref x) => x.write(out),
            &ClassSyntaxItem::Using(ref x) => x.write(out),
            &ClassSyntaxItem::Typedef(ref x) => {
                write!(out, "typedef ");
                x.write(out);
                writeln!(out, ";");
            }
            &ClassSyntaxItem::Declaration(ref x) => {
                x.write(out);
                writeln!(out, ";");
            }
            &ClassSyntaxItem::Definition(ref x) => x.write(out),

            &ClassSyntaxItem::MemberSpecification(ref x) => x.write(out),
        }
    }
}

impl MemberSpecification {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &MemberSpecification::Public => writeln!(out, "public:"),
            &MemberSpecification::Protected => writeln!(out, "protected:"),
            &MemberSpecification::Private => writeln!(out, "private:"),
        }
    }
}

impl Enum {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if self.scoped {
            write!(out, "enum class ");
        } else {
            write!(out, "enum ");
        }
        if let Some(tag) = self.tag.as_ref() {
            tag.write(out);
        }
        out.open_brace();
        self.scope.write(out);
        out.close_brace();
    }
}

impl EnumSyntax {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        for item in &self.items {
            item.write(out);
        }
    }
}

impl EnumSyntaxItem {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        match self {
            &EnumSyntaxItem::Whitespace(lines) => out.new_lines(lines),
            &EnumSyntaxItem::Verbatim(ref x) => write!(out, "{}", x),
            &EnumSyntaxItem::Comment(ref x) => x.write(out),
            &EnumSyntaxItem::Include(ref x) => x.write(out),
            &EnumSyntaxItem::Define(ref x) => x.write(out),
            &EnumSyntaxItem::Conditional(ref x) => x.write(out),

            &EnumSyntaxItem::Enumerator(ref x) => x.write(out),
        }
    }
}

impl Enumerator {
    pub(crate) fn write<F: Write>(&self, out: &mut Writer<F>) {
        if let Some(value) = self.value {
            writeln!(out, "{} = {},", self.name, value);
        } else {
            writeln!(out, "{},", self.name);
        }
    }
}
