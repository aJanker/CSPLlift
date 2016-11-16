int loopfunction(int inner) {
    int x = inner;
    return x;
}


int main() {
    int foo, sink;

    int start = 1;

    foo = start;

    int sink2 = 0;
    int bound = 10;
    int z = 0;
    int low = 1;

#ifdef A
    int i;

    for (i =
#ifdef B
    loopfunction(low)
#else
     start
#endif
     ; i < bound; i++) {
        sink = i;
     #ifdef B
        foo = z;
        #endif
        sink2 = i;
    }

#endif
    if (1)
    bound = foo;
    else
    bound = low;


    sink = bound; // sink 1 (true) bound = 2; sink 2 (true) bound = foo; sink 3 condition A bound = foo = z;

    return sink;
}