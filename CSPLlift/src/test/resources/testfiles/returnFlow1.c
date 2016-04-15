/**
* Testfile 1 for returnFlow Assignments. The sink location is sink = y. In function foo we have different exit statements
* under different conditions. At the sink location  should be 4 different possible sink detected.
*       sink1 : True, (y, True)
*       sink2 : A&B&C, (y, A&B&C) , (p, A)
*       sink3 : B&C, (y, B&C) , (p, C), (x, True)
*       sink4 : !B&C, (y, !B&C)
*/

int foo(int p) {
#ifdef A
    p = 0;
#endif
#ifdef B
    return p;
#else
    return 0;
#endif
}


int main() {
    int x, y, sink;
    x = 1;
    y = 0;
#ifdef C
    y = foo(x);
#endif
    sink = y;
    return 0;
}