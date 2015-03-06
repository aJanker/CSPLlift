void foo() { }
void bar() { }
void baz() { }

int main() {
#if defined(B)
  void (*fp)();
  foo();
  #if defined(A)
    fp= &bar;
  #else
    fp= &baz;
    (*fp)();
  #endif
  a;
#endif
}