int *id(int *x) {
    #ifdef A
    int *a = NULL;
    return a;
    #else
    return x;
    #endif
}

int main() {
    int i, j
    #ifdef B
    , k
    #endif
    ;
    int *a, *b;
    a = id(&i);
    #ifdef B
    a = id (&k);
    #endif
    b = id(&j);
}