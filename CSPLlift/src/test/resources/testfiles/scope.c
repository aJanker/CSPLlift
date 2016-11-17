#ifdef B
int secret = 0;
#else
int secret = 10;
#endif

int foo() {

    int fooValue = secret;

    return fooValue;
}


int main() {

#ifdef A
    int secret = 2;
#endif

    int dafuq;

    int sink1 = foo();
    int sink2 = secret;


    return;
}