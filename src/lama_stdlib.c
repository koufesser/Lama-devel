#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdbool.h>
#include <errno.h>

/* #define clean_errno() (errno == 0 ? "None" : strerror(errno))
#define log_error(M, ...) fprintf(stderr, "[ERROR] (%s:%d: errno: %s) " M "\n", __FILE__, __LINE__, clean_errno(), ##__VA_ARGS__)
#define assertf(A, M, ...)       \
  if (!(A))                      \
  {                              \
    log_error(M, ##__VA_ARGS__); \
    fflush(stderr);              \
    assert(A);                   \
  } */

void myputc(int x)
{
  printf("%d", x);
  fflush(stdout);
}

typedef void *(*fun0)(void);
typedef void *(*fun1)(void *);
typedef void *(*fun2)(void *, void *);

void *lama_apply0(void *f)
{
  fun0 foo = (fun0)f;
  return foo();
}

void *lama_apply1(void *f, void *arg1)
{
  fun1 foo = (fun1)f;
  return foo(arg1);
}

void *lama_apply2(void *f, void *arg1, void *arg2)
{
  fun2 foo = (fun2)f;
  return foo(arg1, arg2);
}
typedef struct
{
  void *code;
  int32_t argsc; // TODO(Kakadu): uint32_t or byte ???
  int32_t args_received;
  void *args[0];
} lama_closure;

lama_closure *copy_closure(lama_closure *src)
{
  size_t size = sizeof(lama_closure) + sizeof(void *) * src->argsc;
  lama_closure *dst = (lama_closure *)malloc(size);
  return memcpy(dst, src, size);
}

void *lama_alloc_closure(void *func, int32_t argsc)
{
  // printf("%s argc = %u\n", __func__, argsc);
  fflush(stdout);
  lama_closure *ans = (lama_closure *)malloc(sizeof(lama_closure) + sizeof(void *) * argsc);
  //  { .code = func, .argsc = argsc }
  ans->code = func;
  ans->argsc = argsc;
  ans->args_received = 0;
  memset(ans->args, 0, argsc * sizeof(void *));
  return ans;
}

void *lama_applyN(void *f, int32_t argc, ...)
{
  setbuf(stdout, NULL);
  // printf("%s argc = %u, closure = %p\n", __func__, argc, f);
  fflush(stdout);

  va_list argp;
  va_start(argp, argc);
  lama_closure *f_closure = copy_closure((lama_closure *)f);
  // printf("f->arg_received = %u\n", f_closure->args_received);
  //  printf("%d\n", __LINE__);
  assert(f_closure->args_received + argc <= f_closure->argsc);

  for (size_t i = 0; i < argc; i++)
  {
    // printf("%d\n", __LINE__);
    void *arg1 = va_arg(argp, void *);
    // printf("arg[%lu] = %p, ", i, arg1);
    fflush(stdout);
    f_closure->args[f_closure->args_received++] = arg1;
  }
  /* printf("\n");
  printf("f->arg_received = %u, f->argc = %u\n",
         f_closure->args_received,
         f_closure->argsc);
  fflush(stdout); */
  va_end(argp);
  if (f_closure->argsc == f_closure->args_received)
  {
    switch (f_closure->argsc)
    {
    case 0:
      return lama_apply0(f_closure->code);
      break;
    case 1:
      return lama_apply1(f_closure->code, f_closure->args[0]);
      break;
    case 2:
      return lama_apply2(f_closure->code, f_closure->args[0], f_closure->args[1]);
      break;
    default:
      printf("FUCK\n");
      assert(false);
    }
  }
  return f_closure;
}
