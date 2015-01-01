typedef int (*PBF)();

struct parse_table {
    char *name;
    PBF func;
};

int func1() {
    return 1;
}

int func2() {
    return 1;
}

struct parse_table table[2];

void init() {
    table[0].name = "name1";
    table[1].name = "name2";
    table[0].func = &func1;
    table[1].func = &func2;
}

PBF find_p_func(char *s) {
    int i;
    int num_func;
    for(i=0; i < num_func; i++) {
        if (strcmp(table[i].name, s) == 0) {
            return table[i].func;
        }
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    int i;
    PBF parse_func = find_p_func(argv[1]);
    if (parse_func) {
        (*parse_func)();
    }
}

