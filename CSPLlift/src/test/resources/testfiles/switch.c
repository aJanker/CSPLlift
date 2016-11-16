#ifdef A
#define mbedtls_free       bar1
#else
#define mbedtls_free       bar2
#endif

int bar1(int b1) {
    int b1r = b1;
    return b1;
}

int bar2(int b2) {
    int b2r = b2;
    return b2;
}

int foo( int j, int s )
{
    int x = 6;
    switch( j )
    {
#ifdef B
        case 1:
            return bar1(s);
#endif

#ifdef A
        case 2:
            return bar2(j);
#endif

        default:
            return x;
    }
}



int main() {
    int secret = 6;
    int j2 = 2;
    int test;

    /*#ifndef A
    test = bar1(j2);
    #endif

    int func = foo(j,secret);

    int sink = func;

    #ifdef A
    test = bar1(j2);
    #endif */

    int stmt;
    int buf;

    if (j2 + 1 == 0 ||
        ( *test = (mbedtls_free( secret ) ) == 0 )) {
            int some = 2;
            return;
    } else {
        int some = 3;
    }


    int stmt2;

    int final = test;

    return 0;

}