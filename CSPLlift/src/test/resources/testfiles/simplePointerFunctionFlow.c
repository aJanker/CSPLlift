//#include <stdlib.h>
//#include <stdio.h>

/*void bar(int b) {
    while(b < 0) {
    int sink = b;

    }

        int emtpyStatement;
        int emtpyStatement2;
} */

/*int foo(int f) {
    int res = 0;
    #ifndef C
    res = f;
    #endif
    return res;
    }


int main() {

    int secret = 1;
    #ifdef A
        secret = 2;
    #endif

    int merge;
    int final2 = foo(secret);
    int final = final2;
    // bar(secret);
    return 0;
}
*/

#ifdef G
int secret = 1;
#endif


int foo(int f) {
    int foofoo = 0;
#ifdef C
    foofoo = f;
#endif

    return foofoo;
    }

int cipher1(int i) {
   int res, x = 0;
   int y = 10;
   int dbg = res;

   foo(i);

   #ifdef C
      x = y;
   #endif

   int a = 5;
   int b = 6;

   #ifdef C
     int res2 = x;
   #endif

   int c = 7;

    if (i < 10) {
        #ifdef C
            res = x;
        #else
            res = i >> x;
        #endif
    }

    dbg = x;
    dbg = res;

   return res;
};

int cipher2(int j) {
#ifdef B
  j = 0;
#endif
    return j;
};

struct cipher_ctx{
    int (*cipherfun)(int);
};

void cipher_init(struct cipher_ctx *c, int (*f)(int)) {
    c->cipherfun = (int (*)(int)) f;
    return;
}

int cipher_do(struct cipher_ctx *c, int value) {
    int result;
    result = (*c->cipherfun)(value);
    return result;
}

int main() {
   struct cipher_ctx *c = malloc(sizeof(struct cipher_ctx));

#ifdef A
    cipher_init(c, &cipher1);
#else
    cipher_init(c, &cipher2);
#endif

    int secret = 2;


    #ifdef S
     secret = 3;
    #endif

    int ctmp = 3;

    int sink = cipher_do(c, secret);

    #ifdef S
        foo(ctmp);
    #endif


    printf("%i\n", sink);

    #ifdef S
    printf("%i\n", secret);
    #endif

    return 0;
}