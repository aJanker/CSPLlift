int foo(int i, long j) {
    long result;

    result = i + j;

    return result;
}

int bar(int z) {
    return bar(z);
}

int inner(int ivalue) {
    int res = ivalue + 1;
    return res;
}

int testing() {
    int tvalue = 5;
    return tvalue;
}

int main() {

 int mi = 66;
 int mj = 22;
  int i = foo(mi, mj);
  int j = foo(mi, mj);

 int point = foo(

     #ifdef A
     bar(
     #endif
     inner(i + j)
     #ifdef A
     )
     #endif
     , testing());

     int sink  = point;

     return 0;
}

