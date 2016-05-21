struct enc {
    int **fun;
};

struct fun2 {
    int (*cipherfun2)(int);
};


int cipher2(int j) {
    int res;
    res = j + j;
    return res;
};

struct cipher_ctx{
    struct enc *enc;
    struct fun2 *f2;
    struct enc *(*fun)(int);
    int (*cipherfun)(int);
};



void foo_fun(struct cipher_ctx *c, int (*f)(int)) {
    c->cipherfun = (int (*)(int)) f;
    c->f2->cipherfun2 = (int (*)(int)) f;
}

int main() {
    struct cipher_ctx* c;

    foo_fun(c, &cipher2);

    int secret = 666;

    int sink = c->cipherfun(secret);
    int sink2 = c->f2->cipherfun2(secret);

    return 0;
}