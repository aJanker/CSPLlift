int secret = 666;

#define function(x) foo(x)

int foo(int value) {
   int i = 20;
   int j = 1000;

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

    int merge = 0;

    sink = res;

   return sink;
};

int main() {
    int res_m = 0;
    int trace = 0;
    int sink_m;

    function(trace);
    function(trace);

    #ifdef F
      res_m = foo(secret);
    #endif

    int ret;

    sink_m = res_m;

    printf("%i\n", secret);

    printf("%i\n", sink_m);

    if ((ret = foo(5 + foo(secret))) == 0) {
        printf("%i\n", foo(secret));
    }

    return 0;
};


