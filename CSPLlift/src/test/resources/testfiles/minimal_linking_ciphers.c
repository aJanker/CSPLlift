#include <minimal_linking.h>

int cipher1(int i) {
   int res, x = 0;
#ifdef A
   x = 10;
#endif

   if (i < 0) {

    res =
#ifdef B
      i + x;
#else
      x;
#endif

  }

   return res;
};

int cipher2(int j) {
    return j;
};