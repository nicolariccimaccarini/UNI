.data
a: .word 6
b: .word 8
c: .word 190
bingostring: .asciiz "bingo!"
nostring: .asciiz "no!!!"

.text
lw $s0, a
lw $s1, b
lw $s2, c

li $t0, 1
beq $s0, $t0, other_checks
li $t0, 2
beq $s1, $t0, other_checks
li $t0, 4
beq $s2, $t0, other_checks
li $t0, 6
bne $s0, $t0, no

other_checks:
li $t0, 8
bne $s1, $t0, no
li $t0, 10
bne $s2, $t0, no
li $v0, 4
la $a0, bingostring
syscall
j exit

no:
li $v0, 4
la $a0, nostring
syscall

exit:
li $v0, 10
syscall