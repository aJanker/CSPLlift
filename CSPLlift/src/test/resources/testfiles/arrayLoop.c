

int main() {

    int i;
    static int a[5] = { 0, 0, 0, 0, 0 };

    int sink = 5;

    for(i = 0; i < sizeof(a); i++) {
        sink = a[i];
    }

    int dbg = sink;

    return dbg;

}