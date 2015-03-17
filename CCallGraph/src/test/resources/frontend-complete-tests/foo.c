#ifdef C
void bar() {
}
#endif

void baz() {
}

// -------------

#ifdef A
void foo() {
#ifdef B
    bar();
#endif
    baz();
}
#endif
