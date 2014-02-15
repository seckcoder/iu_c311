
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
#define pair_tag 0x01
#define pairmask 0x03
#define atommask 0x03
#define atomtag 0x03
#define atomshift 3

typedef unsigned int ptr;

// used to store temporary register value
typedef struct {
  void* eax; /* 0 scratch */
  void* ebx; /* 4 preserve */
  void* ecx; /* 8 scratch */
  void* edx; /* 12 scratch */
  void* esi; /* 16 preserve */
  void* edi; /*20 preserve*/
  void* ebp; /*24 preserve*/
  void* esp; /*28 preserve*/
} context;


typedef struct {
  ptr car;
  ptr cdr;
} pair;
