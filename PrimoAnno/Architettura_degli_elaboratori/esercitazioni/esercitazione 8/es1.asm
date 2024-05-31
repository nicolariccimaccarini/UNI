.data
inputstring: .asciiz "entera value to check divisibility with 6: \n"
divisable: .asciiz "the number is divisible for  by 6 \n"
notdivisable: .asciiz "the number is not divisible by 6 \n"

.text
la $a0, inputstring
li %v0, 4 		# stampo inputstring
syscall	
li $v0, 5
syscall
move $s1, $v0		# $s1 = a = numero preso in input
jal div_6
j exit

exit:
li $v0, 10
syscall

div_6:
# $ra viene sovrascritto
addi $sp, $sp, -4
sw $ra, 0($sp)
li $t0, 3
div $a0, $t0 		# a%3
mfhi $t0 		# metto il resto della divisione in $t0
bne $t0, $zero, else
#se faccio qui li $t0, 1 perdo $t0
jal div_2
# qui ho perso $a0, che mi serve dopo
li $t0, 1
bne $v0, $t0, else
li $v0, 4
la $a0, divisable
syscall
j exitif

else:
li $v0, 4
la $a0, notdivisable
syscall

exitif:
lw $ra, 0($sp)
addi $sp, $sp, 4
jr $ra

div_2:
li $t0, 2
div $a0, $t0
mfhi $t0
beq $t0, $zero, zero
li $v0, 0
j outif

zero:
li $v0, 1

outif:
jr $ra