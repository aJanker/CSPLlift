void foo(int p, int n) {
    while ( n-- ) p++ = 0;
    return;
    }


void bar (int i)
  {
    int sink = i;
    int dbg = sink;
    foo(i, sink);
    return;
    }

#ifdef A
int main() {

    int i = 5;

if (i < 1) {
    i = 9;
    goto exit;
}

    bar(i);

exit:

    int sink = i;

    sink = i;

    i = 7;

    sink = i;
    return 0;
}

#endif
