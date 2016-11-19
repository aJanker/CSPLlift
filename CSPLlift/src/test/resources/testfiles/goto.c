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
   #ifdef B
    goto exit;
    #endif
}

    bar(i);

exit:

    int sinkG = i;

    int sinkG2 = i;

    i = 7;

    int sinkG3 = i;
    return 0;
}

#endif
