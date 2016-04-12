struct enc {
    int **fun;
};

int cipher1(int i) {
       int res;
       res = i * i;
       return res;
};

int cipher2(int j) {
    int res;
    res = j + j;
    return res;
};

struct cipher_ctx{
    struct enc *enc;
    struct enc *(*fun)(int);
    int (*cipherfun)(int);
};

void bar_fun(struct cipher_ctx *c, int (*f)(int)) {
    c->fun = (struct enc *(*)(int)) f;
 }

void foo_fun(struct cipher_ctx *c, int (*f)(int)) {
    c->cipherfun = (int (*)(int)) f;
}

int main() {
    struct cipher_ctx c_init;
    struct cipher_ctx* c;

    bar_fun(c, &cipher1);
    foo_fun(c, &cipher2);

    int secret = 666;

    int sink = (*c->cipherfun)(secret);
}