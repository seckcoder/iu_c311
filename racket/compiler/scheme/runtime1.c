#include <stdio.h>

#define bool_f 0x2F
#define bool_t 0x6F
#define fx_mask 0x03
#define fx_tag 0x00
#define fx_shift 2


typedef unsigned int ptr;

int scheme_entry();

static void print_ptr(ptr x) {
  printf("%d\n", (x & fx_mask));
  if ((x & fx_mask) == fx_tag) {
    printf("%d", ((int) x) >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}


int main(int argc, const char *argv[])
{
  print_ptr(scheme_entry());
  return 0;
}
