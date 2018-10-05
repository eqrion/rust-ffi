#[repr(C)]
pub struct A {
    pub ptr: *const B,
}

#[repr(C)]
pub struct B {
    pub ptr: *const A,
}

#[repr(C)]
pub struct C {
    pub ptr: *const C,
}

#[repr(C)]
pub struct D {
    pub ptr: *const E,
}

#[repr(C)]
pub struct E {
    pub ptr: *const F,
}

#[repr(C)]
pub struct F {
    pub ptr: *const D,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub enum Options {
    A,
    B,
    C
}

#[repr(C)]
pub union Union {
    a: Options,
    b: f32,
}

#[repr(C)]
pub struct NewType(f32);

pub type Alias = NewType;

pub struct Opaque {
    lol: i32,
}

#[no_mangle]
pub extern "C" fn root() -> NewType {
    NewType(1.)
}
