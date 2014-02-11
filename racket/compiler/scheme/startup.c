#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>


#define fxshift 2
#define fxmask 0x03
#define bool_f 0x2F
#define bool_t 0x6F
#define null_v 0x3F
#define wordsize 4
#define fx_tag 0x00
#define char_tag 0x0F
#define charmask 0xFF
#define charshift 8

typedef unsigned int ptr;

int is_fixnum(ptr x) {
  return (x & fxmask) == fx_tag;
}

int is_char(ptr x) {
  return (x & charmask) == char_tag;
}

int to_fixnum(ptr x) {
  return ((int)x) >> fxshift;
}

char to_char(ptr x) {
  return (char)((int)x >> charshift);
}

char beautify_temp[10];
char* beautify(char c) {
  if (c == '\t') {
    return "#\\tab";
  } else if (c == '\n') {
    return "#\\newline";
  } else if (c == '\r'){
    return "#\\return";
  } else if (c == ' ') {
    return "#\\space";
  } else {
    sprintf(beautify_temp, "#\\%c", c);
    return beautify_temp;
  }
}

void print_ptr(ptr x) {
  if (is_fixnum(x)) {
    printf("%d", to_fixnum(x));
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == null_v) {
    printf("()");
  } else if (is_char(x)) {
    printf("%s", beautify(to_char(x)));
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}


static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
      PROT_READ | PROT_WRITE,
      MAP_ANONYMOUS | MAP_PRIVATE,
      0, 0);
  if (p == MAP_FAILED) { perror("map"); exit(1); }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  return (p + page);
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) { perror("munmap"); exit(status); }
}


int scheme_entry();
int main(int argc, char** argv){
  int stack_size = 16 * 4096; // 16K byte
  char* stack_top = allocate_protected_space(stack_size);
  char *stack_base = stack_top + stack_size;
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
