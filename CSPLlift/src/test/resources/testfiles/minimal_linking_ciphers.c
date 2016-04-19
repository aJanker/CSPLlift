#include <minimal_linking.h>

int cipher1(int i) {
   int res;
   int x = 1000;

#ifdef B
   res = i + x;
#else
   res = x;
#endif

   return res;
};

int cipher2(int j) {
    return j;
};