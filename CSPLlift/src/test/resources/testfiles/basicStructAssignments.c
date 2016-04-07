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
    int mx2 = mx;
    int my2 = my;

    POINT p1;
    p1.x = mx2; // Test for Reach mx2 (17); mx (15) at condition true

    my2 = p1.x; // Test for Reach

    return sink;
}