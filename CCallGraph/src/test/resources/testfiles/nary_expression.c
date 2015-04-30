void foo();

int main() {
    if (!0 && foo() != 7) {
        return;
    }
}