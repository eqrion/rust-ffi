// Generated with c-ffi version 0.1.0
#include <cstdint>
#include <cstdlib>
namespace easy {
  struct ForeignStruct {
    float x;
    int32_t y;
  };
}
namespace hard {
  struct NewType {
    float _0;
  };
  typedef struct ::hard::NewType Alias;
  struct Enumeration;
  enum class FieldlessEnumeration {
    A = 0,
    B = 8,
    C = 9,
    D = 10,
  };
  struct GenericStruct;
  namespace bar1 {
    struct BadBar;
    struct Bar {
      bool x;
    };
  }
  namespace foo {
    struct Foo {
      struct ::hard::bar1::Bar const *x;
      float y[2];
      struct ::easy::ForeignStruct z;
    };
    struct UnitStruct;
  }
}
extern "C" {
  void root(struct ::hard::foo::Foo);
}
