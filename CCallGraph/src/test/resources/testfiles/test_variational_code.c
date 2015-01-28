int *id(int *x
#ifdef Y
, int y
#endif
)
{
    #ifdef A
    int *a = NULL
    return a;
    #else
    return x;
    #endif
}

int main() {
    #ifdef B
    int i, j, k;
    #ifdef Y
    int w, c;
    c = id(&w);
    #endif
    int *a, *b;
    a = id(&i);
    #else
    a = id (&k);
    #endif
    b = id(&j);
}