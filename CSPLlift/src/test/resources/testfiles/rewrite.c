int foo(int i, long j) {
    long result;

    result = i + j;

    return result;
}

int bar(int z) {
    return bar(z);
}

int inner(int value) {
    int res = value + 1;
    return res;
}

int testing() {
    int value = 5;
    return value;
}

int main() {
    int point;
    int mi = 5;
    int test = mi;


    if (5 > 2) {
          test = 2;
      }

    #ifdef A
        test = 3;
    #endif

    int branch = test;

    int bla2 = foo(mi, 2);
    int bla = foo(mi, 5);

    int mj = foo(sizeof(point), mi);
    long li = 10l;

    int between = 2;

    point = foo(
    #ifdef A
    bar(
    #endif
    inner(mi)
    #ifdef A
    )
    #endif
    , testing());

    #ifdef A
    int bla3 = inner(5);
    #endif

    int res2 = bla2;
    int res = bla;

    int last = between;

    return 0;
}