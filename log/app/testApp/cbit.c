#include <stdio.h>
#include "cbit.h"

void callTest () {
  printf("hello there!!\n");
  fflush(stdout);
}

void callTest2 (void (*f) ()) {
  printf("callTest2:\n");
  f();
  fflush(stdout);
}