.data
a: .word 0
divisibile: .asciiz "the number is divisible by 6"
non_divisibile: .asciiz "the number is not divisible by 6"
value: .asciiz "enter a value to check divisibility with 6"

.text
lw $a1, a
li $v0, 4
la $a0, value 	#stampa value 
syscall

li $v0, 5
syscall 	#scanf

move $a1, $v0 	#input di a

jal div_6 

div_6:
addi $sp, $sp, -4
sw $ra, 0($sp)

#corpo della procedura

lw $ra, 0($sp)
addi $sp, $sp, 4
jr $ra