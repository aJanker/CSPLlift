#include <minimal_linking.h>

void cipher_init(struct cipher_ctx *c, int (*f)(int)) {
    c->cipherfun = (int (*)(int)) f;
    return;
}