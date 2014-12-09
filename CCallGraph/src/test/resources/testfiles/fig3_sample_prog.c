int *id(int *x) {
    return x;
}

int main() {
    int i, j;
    int *a, *b;
    a = id(&i);
    b = id(&j);
}