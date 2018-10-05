extern crate easy;

#[repr(C)]
pub struct GenericStruct<T> {
    pub inner: T
}

pub mod foo {
    pub use bar1::*;
    use easy::*;

    #[repr(C)]
    pub struct Foo {
        x: *const Bar,
        y: [f32; 2],
        z: ForeignStruct,
    }

    #[repr(C)]
    pub struct UnitStruct;
}

mod bar1 {
    #[repr(C)]
    pub struct Bar {
        x: bool,
    }

    pub struct BadBar {
        x: bool,
    }
}

mod bar2 {
    #[repr(C)]
    pub struct Bar {
        x: i32,
    }
}

#[repr(C)]
pub enum FieldlessEnumeration {
    A,
    B = 10 - 2,
    C,
    D,
}

#[repr(C)]
pub enum Enumeration {
    A(i32),
    B(f32),
    C,
    D { name: i32 },
}

#[repr(transparent)]
pub struct NewType(f32);

pub type Alias = NewType;

#[no_mangle]
pub extern "C" fn root(_: foo::Foo) { }
