void foo();
inline static int bar();
inline static int baz() { bar(); }

#ifdef A
    int foo() {
        bar();
    }
#endif