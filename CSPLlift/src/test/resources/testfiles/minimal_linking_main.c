#include <minimal_linking.h>

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