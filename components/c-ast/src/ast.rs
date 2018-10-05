#[derive(Clone, Debug)]
pub struct File {
    pub(crate) scope: GlobalSyntax,
    pub(crate) nested_builders: Vec<(NestedGlobalSyntax, GlobalSyntax)>,
}

#[derive(Clone, Debug)]
pub struct Comment(pub String);

#[derive(Clone, Debug)]
pub struct Include {
    pub system: bool,
    pub path: String,
}

#[derive(Clone, Debug)]
pub struct Define {
    pub name: String,
    pub value: Option<String>,
}

#[derive(Clone, Debug)]
pub enum ConditionalExpression {
    Defined(String),
    Constant(String),
    Not(Box<ConditionalExpression>),
    And(Box<ConditionalExpression>, Box<ConditionalExpression>),
    Or(Box<ConditionalExpression>, Box<ConditionalExpression>),
}

#[derive(Clone, Debug)]
pub struct GlobalConditional {
    pub cases: Vec<(ConditionalExpression, GlobalSyntax)>,
    pub fallthrough: Option<GlobalSyntax>,
}

#[derive(Clone, Debug)]
pub struct FunctionConditional {
    pub cases: Vec<(ConditionalExpression, FunctionSyntax)>,
    pub fallthrough: Option<FunctionSyntax>,
}

#[derive(Clone, Debug)]
pub struct ClassConditional {
    pub cases: Vec<(ConditionalExpression, ClassSyntax)>,
    pub fallthrough: Option<ClassSyntax>,
}

#[derive(Clone, Debug)]
pub struct EnumConditional {
    pub cases: Vec<(ConditionalExpression, EnumSyntax)>,
    pub fallthrough: Option<EnumSyntax>,
}

#[derive(Clone, Debug)]
pub enum TemplateArgument {
    Simple(String),
    Class(ClassKey, String),
    Enum(String),
}

#[derive(Clone, Debug)]
pub enum TemplateSpecifier {
    Simple(QualifiedIdentifier),
    Class(ClassKey, QualifiedIdentifier),
    Enum(QualifiedIdentifier),
}

#[derive(Clone, Debug)]
pub struct IdentifierSegment {
    pub name: String,
    pub template_args: Vec<TemplateSpecifier>,
}

#[derive(Clone, Debug)]
pub struct QualifiedIdentifier {
    pub segments: Vec<IdentifierSegment>,
    pub global: bool,
}

#[derive(Clone, Debug)]
pub struct Using {
    pub namespace: bool,
    pub ident: QualifiedIdentifier,
}

#[derive(Clone, Debug)]
pub struct Declaration {
    pub prefix: Option<String>,
    pub specifiers: Vec<Specifier>,
    pub ty_specifier: TypeSpecifier,
    pub declarator: Declarator,
    pub postfix: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Specifier {
    Static,
    ThreadLocal,
    Extern,
    Mutable,
    Const,
    Volatile,
}

#[derive(Clone, Debug)]
pub enum TypeSpecifier {
    Simple(QualifiedIdentifier),
    Class(ClassKey, QualifiedIdentifier),
    Enum(QualifiedIdentifier),
    ElaboratedClass(Class),
    ElaboratedEnum(Enum),
}

#[derive(Clone, Debug)]
pub enum Declarator {
    None,
    Identifier(QualifiedIdentifier),
    Cv(CvQualifier, Box<Declarator>),
    Pointer(Box<Declarator>),
    Reference(Box<Declarator>),
    MoveReference(Box<Declarator>),
    Array(Box<Declarator>, Option<usize>),
    Function(Box<Declarator>, Vec<Declaration>, CvQualifier),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CvQualifier {
    None,
    Const,
    Volatile,
}

#[derive(Clone, Debug)]
pub struct Definition {
    pub decl: Declaration,
    pub init: Initialization,
}

#[derive(Clone, Debug)]
pub enum Initialization {
    Expression(Expression),
    FunctionBody(FunctionSyntax),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(QualifiedIdentifier),
    Constant(String),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessThanEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanEqual(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Assign(Box<Expression>, Box<Expression>),
    PostIncrement(Box<Expression>),
    PreIncrement(Box<Expression>),
    MemberAccess(Box<Expression>, QualifiedIdentifier),
    PointerMemberAccess(Box<Expression>, QualifiedIdentifier),
    Subscript(Box<Expression>, Box<Expression>),
    AddressOf(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
}

#[derive(Clone, Debug)]
pub struct GlobalSyntax {
    pub items: Vec<GlobalSyntaxItem>,
}

#[derive(Clone, Debug)]
pub enum GlobalSyntaxItem {
    Whitespace(u32),
    Verbatim(String),
    Comment(Comment),
    Include(Include),
    Define(Define),
    Conditional(GlobalConditional),
    Using(Using),
    Typedef(Declaration),
    Declaration(Declaration),
    Definition(Definition),

    ExternC(GlobalSyntax),
    Namespace(Namespace),
}

#[derive(Clone, Debug)]
pub(crate) enum NestedGlobalSyntax {
    Conditional(ConditionalExpression),
    ExternC,
    Namespace(Option<String>),
}

#[derive(Clone, Debug)]
pub struct Namespace {
    pub name: Option<String>,
    pub scope: GlobalSyntax,
}

#[derive(Clone, Debug)]
pub struct FunctionSyntax {
    pub items: Vec<FunctionSyntaxItem>,
}

#[derive(Clone, Debug)]
pub enum FunctionSyntaxItem {
    Whitespace(u32),
    Verbatim(String),
    Comment(Comment),
    Include(Include),
    Define(Define),
    Conditional(FunctionConditional),
    Using(Using),
    Typedef(Declaration),
    Declaration(Declaration),
    Definition(Definition),

    Block(FunctionSyntax),
    Expression(Expression),
    If(Expression, FunctionSyntax, Option<FunctionSyntax>),
    For(Definition, Expression, Expression, FunctionSyntax),
    While(Expression, FunctionSyntax),
    Return(Expression),
}

#[derive(Clone, Debug)]
pub struct Class {
    pub key: ClassKey,
    pub tag: Option<QualifiedIdentifier>,
    pub template_args: Vec<TemplateArgument>,
    pub scope: ClassSyntax,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClassKey {
    Struct,
    Class,
    Union,
}

#[derive(Clone, Debug)]
pub struct ClassSyntax {
    pub items: Vec<ClassSyntaxItem>,
}

#[derive(Clone, Debug)]
pub enum ClassSyntaxItem {
    Whitespace(u32),
    Verbatim(String),
    Comment(Comment),
    Include(Include),
    Define(Define),
    Conditional(ClassConditional),
    Using(Using),
    Typedef(Declaration),
    Declaration(Declaration),
    Definition(Definition),

    MemberSpecification(MemberSpecification),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemberSpecification {
    Public,
    Protected,
    Private,
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub scoped: bool,
    pub tag: Option<QualifiedIdentifier>,
    pub scope: EnumSyntax,
}

#[derive(Clone, Debug)]
pub struct EnumSyntax {
    pub items: Vec<EnumSyntaxItem>,
}

#[derive(Clone, Debug)]
pub enum EnumSyntaxItem {
    Whitespace(u32),
    Verbatim(String),
    Comment(Comment),
    Include(Include),
    Define(Define),
    Conditional(EnumConditional),

    Enumerator(Enumerator),
}

#[derive(Clone, Debug)]
pub struct Enumerator {
    pub name: String,
    pub value: Option<isize>,
}
