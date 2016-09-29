 typedef struct{
        int nr;
        int *rk;
        int buf[68];
    } aes_context;

void init(aes_context *ctx2, int *key, int length) {
    ctx2->nr = length;
    ctx2->rk = key;
    return;
}

int main() {
    aes_context *ctx;

    int key = 5;
    int keyb = 3;
    int length = 3;
    int sink;

#ifdef A
    init(ctx, &key, length);
#else
    init(ctx, &keyb, length);
#endif

    int y = 0;
    sink = *(ctx->rk) + y;

    return 0;
}