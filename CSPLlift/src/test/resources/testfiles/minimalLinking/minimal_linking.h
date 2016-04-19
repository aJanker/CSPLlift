int cipher1(int i);

int cipher2(int i);

struct cipher_ctx{
    int (*cipherfun)(int);
};

void cipher_init(struct cipher_ctx *c, int (*f)(int));

int cipher_do(struct cipher_ctx *c, int value);