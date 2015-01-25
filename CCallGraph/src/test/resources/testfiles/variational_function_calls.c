void foo() { }
void bar() { }
void baz() { }

int main() {
#ifdef B
    void (*fp)();
#ifdef A
    fp= &bar;
#else
    fp= &baz;
#endif
    (*fp)();
#endif
}