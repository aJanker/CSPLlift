/*int secret = 30;

int main() {
   int i = 20;
   int j = 1000;

   int res = 0;
   int sink;

   #ifdef B
     j = secret;
   #endif

   #ifdef A
     i = j;
   #endif

    if (i < 10) {
        #ifdef C
            res = i + j;
        //#else
        //    res = j;
        #endif
    }

    int merge = 0;

    sink = res;

   return sink;
}; */


int main()  {
  int secret =  30;
  int i =  20;
  int j =  1000;
  int res =  0;
  int sink;
  (j = secret);
  (i = j);
  if ((i < 10)) {
    (res = (i + j));
  }

  (sink = res);
  return sink;
}
;


