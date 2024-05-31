.data
x: .word 2

.text
lw $a0, x
li $t0, 6

bge $a0, $t0, verificato
li $v0, 1
syscall
j exitmain

exitmain:
li $v0, 10
syscall

verificato:
move $a0, $t0
li $v0, 1
syscall