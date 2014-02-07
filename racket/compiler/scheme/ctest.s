	.section	__TEXT,__text,regular,pure_instructions
	.globl	_scheme_entry
	.align	4, 0x90
_scheme_entry:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$7, -8(%ebp)
	movl	-8(%ebp), %eax
	movl	%eax, -4(%ebp)
	movl	-4(%ebp), %eax
	addl	$8, %esp
	popl	%ebp
	ret


.subsections_via_symbols
