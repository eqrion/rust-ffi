// Generated with c-ffi version 0.1.0
#include <cstdint>
#include <cstdlib>
namespace medium {
  struct A;
  struct B {
    struct ::medium::A const *ptr;
  };
  struct A {
    struct ::medium::B const *ptr;
  };
  struct NewType {
    float _0;
  };
  typedef struct ::medium::NewType Alias;
  struct C {
    struct ::medium::C const *ptr;
  };
  struct D;
  struct F {
    struct ::medium::D const *ptr;
  };
  struct E {
    struct ::medium::F const *ptr;
  };
  struct D {
    struct ::medium::E const *ptr;
  };
  struct Opaque;
  enum class Options {
    A = 0,
    B = 1,
    C = 2,
  };
  union Union {
    enum ::medium::Options a;
    float b;
  };
}
extern "C" {
  struct ::medium::NewType root();
}
