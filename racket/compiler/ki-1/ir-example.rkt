We have:
$sp(stack pointer) $fp(frame pointer) $ra(return address) $a0(accumulator) $t1(a temporary register)

cgen(c) -> li $a0 c (load constant to accumulator)

cgen(+ e1 e2) ->

cgen(e1)
sw $a0 0($sp) (store value of accumulator to stack)
addiu $sp $sp -4 (add -4 to stack pointer, ie, push stack)
cgen(e2)
lw $t1 4($sp) (load $sp+4 in stack, i,e,., value of e1 to a temporary register)
add $a0 $t1 $a0
addiu $sp $sp 4 (pop stack)

beq reg1 reg2 label : Branch to label if reg1 = reg2
b label : jump to label

if e1 = e2 then else
cgen(e1)
sw $a0 0$(sp)
addiu $sp $sp -4
cgen(e2)
lw $t1 4($sp)
addiu $sp $sp 4
beq $t1 $a0 true_branch
false_branch:
  cgen(else)
  b end_if
true_branch:
  cgen(then)
end_if:


jal label
  jump to label, save address of next instruction in $ra
jr reg
  jump to register
  
(f e1 e2 ... en)

sw $fp 0($sp)
addiu $sp $sp -4
cgen(en)
sw $a0 0($sp)
addiu $sp $sp -4
...
cgen(e1)
sw $a0 0$(sp)
addiu $sp $sp -4
jal func_f

def f(x1,x2,...xn) -> e
func_f:
  move $fp $sp
  sw $ra 0($sp)
  addiu $sp $sp -4
  cgen(e)
  lw $ra 4($sp)
  addiu $sp $sp z (with z = 4*n + 8)
  lw $fp 0($sp)
  jr $ra
