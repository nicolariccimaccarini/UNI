.data 
i: .word 33
j: .word 66

.text
lw $s0, i
lw $s1, j

# salta se la condizione e' falsa
slt $t0, $s0, $s1 	#$t0 vale 1 se (i<j)
beq $t0, $zero, ramofalse
li $s2, 1

ramofalse:
move $a0, $s2
li $v0, 1
syscall

li $v0, 10
syscall