int sink2 = 0;

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
if (j < 0) {
    int r = foo(j);
} else {
    r = 0;
}
    return r;
}


int main() {
    int x = 4;

    int y;
    int sink = bar(x);

    y = sink;
    x = sink2;

    return y;
}