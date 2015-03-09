void foo() { }
void bar() { }
void baz() { }

int main() {
#ifdef B
  void (*fp)();
  foo();
  #ifdef A
    fp= &bar;
    (*fp)();
  #else
    fp= &baz;
  #endif
  a; // bug with parser ? nested ifdefs supported?
#endif
}