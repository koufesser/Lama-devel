#include <stdio.h>

void myputc(int x) {
  printf("%d", x);
  fflush(stdout);
}
