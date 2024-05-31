.data
var: .word 5

.text
lw $s0, var

whileloop:
bgt $s0, 10, exitloop
move $a0, $s0
li $v0, 1
syscall

beq $s0, $zero, exitloop
addi $s0, $s0, -1
j whileloop

exitloop:
li $v0, 10
syscall