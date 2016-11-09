//#include <stdlib.h>
//#include <stdio.h>

int cipher1(int i) {
   int res, x = 0;
   int y = 10;
   int dbg = res;

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

    int secret = 1;
    int value = 5;

    #ifdef D
    secret = 666;
    #endif

    int value2000 = 2;

#ifdef D
    value2000 = secret;
#endif
    int sink = cipher_do(c, secret);

    printf("%i\n", sink);

    #ifdef E
    printf("%i\n", value2000);
    #endif


    return 0;
}