int foo(int i, long j) {
    long result;

    result = i + j;

    return result;
}

int bar(int z) {
    return bar(z);
}

int inner(int value) {
    int res = value + 1;
    return res;
}

int testing() {
    int value = 5;
    return value;
}

int main() {

 int mi = 66;

 int point = foo(

     #ifdef A
     bar(
     #endif
     inner(mi)
     #ifdef A
     )
     #endif
     , testing());

     int sink  = point;

     return 0;
}

