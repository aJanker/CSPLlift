
int main() {
   int secret = 30;


   int i = 20;
   int j = 1000;

   int value = 0;
   int id = 0;
   int res = value;
   int sink;

   #ifdef B
     id = secret;
   #endif

    if (i < 10) {
            res = i + j
        #ifdef C
             + id
        #endif
        ;
    }


    sink = res;

   return sink;
};


