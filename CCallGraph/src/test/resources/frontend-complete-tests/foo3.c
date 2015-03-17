void bar() {
}

#ifdef AA
void baz() {
}
#endif

// -------------

#ifdef Z
void foo() {
#ifdef X1
    bar();
#endif
#ifdef X2
	bar();
#endif
#ifdef Y    
    baz();
#endif
}
#endif
