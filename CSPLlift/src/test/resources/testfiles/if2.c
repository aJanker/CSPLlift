int foo(int value) {
   int i = 20;
   int j = 1000;

   int res = 0;
   int sink;

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
   int secret = 30;
    int res_m = 0;
    int sink_m;

    #ifdef F
      res_m = foo(secret);
    #endif

    sink_m = res_m;

    return 0;
};


/*int main()  {
  int secret =  30;
  int i =  20;
  int j =  1000;
  int res =  0;
  int sink;

  #ifdef A
  (j = secret);
  #endif

  (i = j);
  if ((i < 10)) {
   res =
    #ifdef B
    i
    #else
    secret
    #endif
    ;

int merge;
  }

  (sink = res);
  return sink;
}
; */


