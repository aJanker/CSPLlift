void foo();
inline int bar() { foo(); }
static void baz(int x, int y) { int a; }

int main() {
    bar();
}