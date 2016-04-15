int loopfunction(int inner) {
    int x = inner;
    return x;
}


int main() {
    int foo, sink;

    foo = 1;
    int bound = 10;
    int z = 0;
    int low = 1;

#ifdef A
    int i;

    for (i =
#ifdef B
    loopfunction(low)
#else
     1
#endif
     ; i < bound; i++) {
        foo = z;
    }

#endif
    if (1)
    bound = foo;
    else
    bound = 2;


    sink = bound; // sink 1 (true) bound = 2; sink 2 (true) bound = foo; sink 3 condition A bound = foo = z;

    return sink;
}