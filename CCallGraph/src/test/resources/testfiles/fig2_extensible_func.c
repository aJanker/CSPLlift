
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
}

void free_fun(struct obstack *h, void *f) {
    h->freefun = (void (*)()) f;
}

int main() {
    struct obstack h;
    chunck_fun(&h, &xmalloc);
    free_fun(&h, &xfree);

}