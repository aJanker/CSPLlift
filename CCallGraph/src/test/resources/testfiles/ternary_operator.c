 void foo();
 void bar();

 void main() {
   if (b == 0 || (a == 0 ? foo : bar)() == 1) {
    // do nothing
   }
 }