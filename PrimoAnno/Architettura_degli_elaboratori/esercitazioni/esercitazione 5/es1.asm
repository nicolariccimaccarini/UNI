.data
x: .word 41
stampa: .asciiz "You succeed!"

.text
lw $s0, x
addi $s0, $s0, 1
li $t0, 42
bne $s0, $t0, notsucceed
li $v0, 4
la $a0, stampa
syscall

notsucceed: 
li $v0, 10
syscall
