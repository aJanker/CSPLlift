#include <minimal_linking.h>

int cipher_do(struct cipher_ctx *c, int value) {
    int result;
    result = (*c->cipherfun)(value);
    return result;
}