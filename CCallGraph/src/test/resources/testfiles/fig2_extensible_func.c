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

struct obstack* c;


/*void chunck_fun(struct obstack *h, void *f) {
    h->chunkfun = (struct _chunck *(*)()) f;
    (h->chunkfun)();
} */

void free_fun(struct obstack *h, void *f) {
    h->freefun = (void (*)()) f;
    //(*h->freefun)();
    //(h->freefun)();
}

int main() {
    struct obstack h;
    struct obstack b;
    free_fun(&h, &xfree);
    c = &h;
    //(*c->freefun)();
    #ifdef A

    free_fun(&b, &xmalloc);
    c = &b;

    #endif

    foo();

    //(c->freefun)();

    //h.freefun();
    //b = c->freefun;
}

void foo() {
    (c->freefun)();
}