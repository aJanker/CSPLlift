int cipher1(int i) {
       int res;
       int x;
       res = i + x;
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

void cipher_do(struct cipher_ctx *c, int value) {
        int result;

        result = (*c->cipherfun)(value);

        return;
}

int main() {
    struct cipher_ctx c_init;
    struct cipher_ctx* c;

#ifdef A
    cipher_init(c, &cipher1);
#else
    cipher_init(c, &cipher2);
#endif

    int secret = 666;

    cipher_do(c, secret);

    return 0;
}