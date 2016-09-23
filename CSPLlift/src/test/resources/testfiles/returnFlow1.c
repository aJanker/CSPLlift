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
    int defaultValue = 0;
    int secret = 666;

    int x, y, sink;

    x = defaultValue;
    y = defaultValue;

#ifdef E
    x = secret;
#endif

#ifdef C
    y = foo(x);
#endif

    sink = y;
    return 0;
}