void foo() { }
void bar() { }
void baz() { }

int main() {
#ifdef B
  void (*fp)();
  foo();
  #ifdef A
    fp = &bar;
  #else
    fp = &baz;
  #endif
#endif
(*fp)();
}