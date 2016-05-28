struct enc {
    int **fun;
};

struct fun2 {
    int (*cipherfun2)(int);
};


int cipher2(int j) {
    int res;
    #ifdef A
    int value = 4;
    #else
    int value = j;
    #endif
    res = j + value;
    return res;
};

struct cipher_ctx{
    struct enc *enc;
    struct fun2 *f2;
    struct enc *(*fun)(int);
    int (*cipherfun)(int);
};

void init(struct cipher_ctx *ci, struct fun2 *di) {
    ci->f2 = di;
}

void foo_fun(struct cipher_ctx *c, int (*f)(int)) {
    c->cipherfun = (int (*)(int)) f;
}

void foo3(struct cipher_ctx *c) {
    int foo_value = 222;
    int sinkInFoo = c->f2->cipherfun2(foo_value);

    return;
}

void foo2(struct cipher_ctx *c) {
    foo3(c);

    return;
}

void foo1(struct cipher_ctx *c) {
    foo2(c);

    return;
}

int main() {
    struct cipher_ctx* c;
    struct fun2* d;

    d->cipherfun2 = &cipher2;

    init(c, d);
    foo_fun(c, &cipher2);

    foo1(c);

    int secret = 666;

    int sink = c->cipherfun(secret);
    int sink2 = c->f2->cipherfun2(secret);

    return 0;
}