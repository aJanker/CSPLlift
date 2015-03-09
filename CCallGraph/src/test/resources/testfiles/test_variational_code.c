void bar() {}

void foo() {
    #ifdef A
        bar();
    #endif
    #ifdef B
        bar();
    #endif
}