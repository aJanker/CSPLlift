void foo() { }
void bar() { }
void baz() { }

int main() {
#if defined(B)
  void (*fp)();
  foo();
  #if defined(A)
    fp= &bar;
    (*fp)();
  #else
    fp= &baz;
  #endif
  a;
#endif
}