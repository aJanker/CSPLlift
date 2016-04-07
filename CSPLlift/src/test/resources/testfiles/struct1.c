typedef struct {
    int x;
    int y;
} POINT;

struct punkt {
    int x;
    int py;
    POINT innerp;
};

int foo(struct punkt pfoo) {
    int res = pfoo.x;

    return res;
}

struct punkt f(struct punkt bar)
{
 struct punkt r = bar;
 return r;
}

int main()
{
    int x = 0;
    int y = 0;
    int x2 = 2;
    int y2 = 2;

    POINT p1;
    p1.x = x2;

    y2 = p1.x;

    p1.y = 10;
    p1.x = y2;

    struct punkt p;

    p.
#ifdef A
    innerp.
#endif
    x = y2;

#ifdef A
    p.
#endif
    x = y;

    p.innerp = p1;

    int z = p1.x;
    p1.x = y;

    p1.y = 20;

    POINT p2;
    p2.x = p1.x;
    p2.y = p1.x;

#ifdef A
    p1 = p2;
#endif

#ifdef B
    p1.y = 30;
 #endif

    struct punkt func = f(p);

    int sink = p1.y + z;  // Test for correct reach

    z = func.x; // Test for correct reach

    return sink;
 }