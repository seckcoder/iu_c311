   .section	__TEXT,__text,regular,pure_instructions
   .globl	_scheme_entry
   .align	4, 0x90
_scheme_entry:
   movl $47, %eax
   andb $3, %al
   cmpb $0, %al
   sete %al
   movzbl %al, %eax
   sal $6, %al
   or $47, %al
   cmpw $47, %ax
   sete %al
   movzbl %al, %eax
   sal $6, %al
   or $47, %al
   ret
