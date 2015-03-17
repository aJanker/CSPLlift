void bar() {
}

void baz() {
}

// -------------

void foo() {
#ifdef X
    bar();
#endif
#ifdef Y    
    baz();
#endif
}
