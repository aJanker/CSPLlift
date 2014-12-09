int foo() {
}

typedef int* pInt;

int main() {
    float b;
    int (*fp)() = &foo;
    (*fp)();
}


