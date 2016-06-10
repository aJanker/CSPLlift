int sink2;

int foo(int i) {
    return i;
}

void foo2(int i2) {
    sink2 = i2;

    return;
}

int bar(int j) {
#ifdef A
    foo2(j);
#endif
    return foo(j);
}


void main() {
    int x = 4;
    int sink = bar(x);
    x = sink2;
    return;
}