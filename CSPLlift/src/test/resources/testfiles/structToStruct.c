typedef struct {
    int x;
    int y;
} POINT;

struct punkt {
    int x;
    int py;
    POINT innerp;
};


int main() {

    int sink;
    POINT p1;
    POINT p2;

    int foo = 2;
    int bar = 3;

    p1.x = foo;
    p1.y = foo;

    p2.x = bar;
    p2.y = bar;

#ifdef A
    p2 = p1;
#endif
    sink = p2.x;
    return sink;

}