#include <minimal_linking.h>

int cipher1(int i) {
   int res = 0;
   int x = 10;

   if (i < 0) {
#ifdef B
   res = i + x;
#else
   res = x;
#endif
  }

  return res;
};

int cipher2(int j) {
    return j;
};