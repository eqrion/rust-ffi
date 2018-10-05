use std::ops::{Deref, DerefMut};

use ast::*;

impl File {
    pub fn enter_conditional(&mut self, conditional: ConditionalExpression) {
        self.nested_builders.push((
            NestedGlobalSyntax::Conditional(conditional),
            GlobalSyntax { items: Vec::new() },
        ))
    }

    pub fn enter_extern_c(&mut self) {
        self.nested_builders.push((
            NestedGlobalSyntax::ExternC,
            GlobalSyntax { items: Vec::new() },
        ))
    }

    pub fn enter_namespace<S: AsRef<str>>(&mut self, name: Option<S>) {
        self.nested_builders.push((
            NestedGlobalSyntax::Namespace(name.map(|x| x.as_ref().to_owned())),
            GlobalSyntax { items: Vec::new() },
        ))
    }

    pub fn leave(&mut self) {
        assert!(!self.nested_builders.is_empty());

        let (builder, scope) = self.nested_builders.pop().unwrap();

        match builder {
            NestedGlobalSyntax::Conditional(expression) => {
                self.append(GlobalSyntaxItem::Conditional(GlobalConditional {
                    cases: vec![(expression, scope)],
                    fallthrough: None,
                }));
            }
            NestedGlobalSyntax::ExternC => {
                self.append(GlobalSyntaxItem::ExternC(scope));
            }
            NestedGlobalSyntax::Namespace(name) => {
                self.append(GlobalSyntaxItem::Namespace(Namespace { name, scope }));
            }
        }
    }
}

impl Deref for File {
    type Target = GlobalSyntax;

    fn deref(&self) -> &Self::Target {
        if self.nested_builders.is_empty() {
            &self.scope
        } else {
            &self.nested_builders.last().unwrap().1
        }
    }
}

impl DerefMut for File {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if self.nested_builders.is_empty() {
            &mut self.scope
        } else {
            &mut self.nested_builders.last_mut().unwrap().1
        }
    }
}

impl GlobalSyntax {
    pub fn append(&mut self, item: GlobalSyntaxItem) -> &mut GlobalSyntax {
        self.items.push(item);
        self
    }
}

impl FunctionSyntax {
    pub fn append(&mut self, item: FunctionSyntaxItem) -> &mut FunctionSyntax {
        self.items.push(item);
        self
    }
}

impl Deref for Class {
    type Target = ClassSyntax;

    fn deref(&self) -> &Self::Target {
        &self.scope
    }
}

impl DerefMut for Class {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.scope
    }
}

impl ClassSyntax {
    pub fn append(&mut self, item: ClassSyntaxItem) -> &mut ClassSyntax {
        self.items.push(item);
        self
    }
}

impl Deref for Enum {
    type Target = EnumSyntax;

    fn deref(&self) -> &Self::Target {
        &self.scope
    }
}

impl DerefMut for Enum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.scope
    }
}

impl EnumSyntax {
    pub fn append(&mut self, item: EnumSyntaxItem) -> &mut EnumSyntax {
        self.items.push(item);
        self
    }
}
