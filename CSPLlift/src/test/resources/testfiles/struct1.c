

typedef int test;

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

    int x = 0;
    int y = 0;
    int x2 = 2;
    int y2 = 2;

    POINT p1;
    p1.x = x2;
    p1.y = 2;

    struct punkt p;

    p.
    #ifdef A
    innerp.
    #endif
    x = 1;

        #ifdef A
        p.
        #endif
        x = 1;


    p.innerp = p1;

    int z = p1.x;
    p1.x = y;

     POINT p2;
     p2.x = p1.x;
     p2.y = p1.x;

#ifdef A
     p1 = p2;
#endif

     int sink = p1.y + z;  // reach from x = 0, y = 0; or def A x = 0;


    return sink;
}