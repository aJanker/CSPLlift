
struct _chunck {
    void **mem;
};

void xfree();
void xmalloc();

struct obstack{
    struct _chunck *chunk;
    struct _chunck *(*chunkfun)();
    void (*freefun)();
};

void chunck_fun(struct obstack *h, void *f) {
    h->chunkfun = (struct _chunck *(*)()) f;
    (*h->chunkfun)();
}

void free_fun(struct obstack *h, void *f) {
    h->freefun = (void (*)()) f;
    (*h->freefun)();
}

int main() {
    struct obstack h;
    struct obstack* c;
    chunck_fun(&h, &xmalloc);
    free_fun(c, &xfree);
    (*c->freefun)();
}