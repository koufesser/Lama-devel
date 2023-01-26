#include <stdio.h>

void myputc(int x) {
  printf("%d", x);
  fflush(stdout);
}

typedef void*(*fun0)(void);
typedef void*(*fun1)(void*);
typedef void*(*fun2)(void*, void*);

void* lama_apply0(void* f ) {
  fun0 foo = (fun0) f ;
  return foo();
}

void* lama_apply1(void* f, void* arg1 ) {
  fun1 foo = (fun1) f ;
  return foo(arg1);
}

void* lama_apply2(void* f, void* arg1, void* arg2 ) 
{
  fun2 foo = (fun2) f ;
  return foo(arg1, arg2);
}
