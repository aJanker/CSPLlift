int secret = 30;

int main() {
   int i = 20;
   int j = 1000;

   int res = 0;
   int sink;

   #ifdef B
     j = secret;
   #endif

    if (i < 10) {
            res = i + j
        #ifdef C
             + secret
        #endif
        ;
    }


    sink = res;

   return sink;
};


