#define function(x, y) do { if (bar(x, y)) goto jump; } while( 0 );
#define function2(x, y) do { if (bar(x, y)) ;} while( 0 )

void bar(int v, int n) {
    int p = v; while( n-- ) p = n;
    return;
}

int foo(int value) {
   int i = 20;
   int j = 1000;
   int trace = 0;
   int trace2 = 0;

   function(i, j);

   int res = 0;
   int sink;

   secret = i;

   #ifdef B
     j = value;
   #endif

   #ifdef A
     i = j;
   #endif

    if (i < 10) {
        #ifdef C
            res = i;
        #else
            res = 5;
        #endif
    }

    sink = res;

   return sink;
};

int main() {
    int res_m = 0;
    int trace = 0;
    int trace2 = 0;
    int sink_m;
    int secret = 666;
    int sec2 = secret;

#ifdef A
    function(trace, trace2);
    function(trace, sec2);
    function(trace, trace2);
#endif

    #ifdef F
      res_m = foo(secret);
    #endif

    int ret;

    sink_m = res_m;

    jump:

    printf("%i\n", secret);

    printf("%i\n", sink_m);

    if ((ret = foo(5 + foo(secret))) == 0) {
    #ifdef T
        trace2 = 3;
    #endif
    #ifdef TT
        function(trace, trace2);
     #endif
        int sink5000 = foo(trace2);
        printf("%i\n", foo(secret));
    }

    return 0;
};


