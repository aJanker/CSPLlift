int secret = 666;

#define function(x, y) do { if (bar(x, y)) goto jump; } while( 0 )
#define function2(x, y) do { if (bar(x, y)) ;} while( 0 )

void bar(int v, int n) {
    int p = v; while( n-- ) p;
}

int foo(int value) {
   int i = 20;
   int j = 1000;

   function(i, trace2);

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

    function(trace, trace2);
    function2(trace, trace2);

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
        function2(trace, trace2);
        int sink5000 = foo(trace2);
        printf("%i\n", foo(secret));
    }

    return 0;
};


