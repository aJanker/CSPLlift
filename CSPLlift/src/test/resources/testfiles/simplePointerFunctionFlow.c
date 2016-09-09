//#include <stdlib.h>
//#include <stdio.h>

int cipher1(int i) {
   int res = 0;
   int x = 1000;
   int dbg;

   #ifdef B
     i = x;
   #endif


    if (i < 10) {
        #ifdef C
            res = i >> x;
        #else
            res = x;
        #endif
    }


    dbg = res;

   return res;
};

int cipher2(int j) {
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

    int secret;
    secret = 666;

    int sink = cipher_do(c, secret);

    printf("%i\n", sink);

    return 0;
}