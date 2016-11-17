int secret = 0;

int foo() {

    int fooValue = secret;

    return fooValue;
}


int main() {

    secret = 3;

#ifdef A
    int secret = 2;
#endif

    int dafuq;

    int sink1 = foo();
    int sink2 = secret;


    return;
}