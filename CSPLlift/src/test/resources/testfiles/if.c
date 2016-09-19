/*
int main() {
   int i = 20;
   int j = 30;
   int res = 0;
   int x = 1000;
   int dbg;

   #ifdef B
     i = x;
   #endif


    if (i < 10) {
        #ifdef C
            res = i + x;
        #else
            res = x;
        #endif
    }


    dbg = res;

   return res;
}; */



int main()  {
  int res =  0;
  int x = 2;
  int y =  1000;
  int dbg;

  //x = y;

  if ((x < 10)) {
    (res = (x + y));
  }


  (dbg = res);
  return res;
}
;