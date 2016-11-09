int foo(int p) {
#ifdef A
    p = 0;
#endif
#ifdef B
    return p;
#else
    return 0;
#endif
}

int main() {
    int defaultValue = 0;
    int secret = 666;

    int x, y, sink;

    x = defaultValue;
    y = defaultValue;

#ifdef E
    x = secret;
#endif

#ifdef C
    y = foo(x);
#endif

    int merge;
    sink = y; // Sink of secret @ B&C&E
    return 0;
}