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

    int dafuq = 3;

    int key = dafuq;

    int value = dafuq;

    int sink1 = foo();
    int sink2 = secret;

    if (dafuq) {
        int x = pseudo(key);
    }

    return;
}