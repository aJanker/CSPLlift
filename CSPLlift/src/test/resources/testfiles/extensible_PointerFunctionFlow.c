/*struct _chunck {
    void **mem;
};

void xmalloc();

void xfree();

void foo() {
    (c->freefun)();
}

struct obstack{
    struct _chunck *chunk;
    struct _chunck *(*chunkfun)();
    void (*freefun)();
};

struct obstack* c;

*/
/*void chunck_fun(struct obstack *h, void *f) {
    h->chunkfun = (struct _chunck *(*)()) f;
    (h->chunkfun)();
} */
/*
void free_fun(struct obstack *h, void *f) {
    h->freefun = f;
    //(*h->freefun)();
    //(h->freefun)();
} */


int foo(int value) {
        int i = 0;
        int y = 2;
        int x = 1;

    #ifdef A
            x =
           #if (defined(B) && defined(C)) || defined(S)
             value;
            #elif defined(B)
               i;
            #else
               y;
            #endif
        #endif


        #ifdef B
           y = x;
        #endif



    return y;
};

int main() {
    /*struct obstack h;
    struct obstack b;

    free_fun(&h, &xfree);
    c = &h;
    #ifdef A

    free_fun(&b, &xmalloc);
    c = &b;

    #endif

    foo(); */

    int secret = 6;
    int non = 3;
    int j1 = 1;
    int j2 = 2;
    int sink;

#ifdef J
    if (secret > 6) {
        secret = j1;
    } else {
        secret = j2;
    }

    //goto DST;

#endif

 /*   int mergeStmt;

    int statement = 1;

    secret = statement;

    int sink2 = secret;

    return 1;

DST: */

    #ifdef X
        secret = non;
    #endif

    int platz;

    platz =
        #ifdef Z
        foo(secret);
        #else
        secret;
        #endif

     int blabla;

     sink = platz;

     return 0;

}