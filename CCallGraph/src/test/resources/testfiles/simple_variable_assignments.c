
int main() {
   p = &x;
   p->f = &z;
   p->f->g.c = a;
   &p->f->g.c = *a;
   p->f = aaa;
   p->f->g();
   &*&p->f->g.c = a;
   t.m->k = &p;
}
