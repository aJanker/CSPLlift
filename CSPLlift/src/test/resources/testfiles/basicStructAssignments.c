typedef struct {
    int x;
    int y;
} POINT;

struct punkt {
    int px;
    int py;
    POINT innerp;
};

int main()
{
    int sink;
    int mx = 0;
    int my = 0;
    int mx2 =
    #ifdef A
    mx;
    #else
    my;
    #endif

    int my2 = my;

    struct punkt pp;

    POINT p1;
    POINT p2;
    p1.x = 5;
    p1.x = mx2; // Test for Reach mx2 (17); mx (15) at condition true

    p2 = p1;

    my2 = p1.x; // Test for Reach

    pp.innerp.x = my2;


    sink = pp.innerp.x;

    return sink;
}