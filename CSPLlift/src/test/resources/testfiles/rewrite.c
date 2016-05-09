long foo(int i, long j) {
    long result;

    result = i + j;

    return result;
}

int bar(int z) {
    int list = bar(z);
    return bar(z);
}

int inner(int value) {
    int res = value + 1;
    return res;
}


int main() {
    int *point;
    int mi = 5;
    int mj = 2;
    long li = 10l;

    long test = foo(
    #ifdef A
    bar(
    #endif
    inner(mi)
    #ifdef A
    )
    #endif
    , li);

    return 0;
}