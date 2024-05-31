.data
stringa: .asciiz "Enter a value to check divisibility by 6\n"
number: .asciiz "The number "
divisable: .asciiz " is divisable by 6.\n"
not_divisable: .asciiz " is NOT divisable by 6.\n"

.text
li $v0, 4
la $a0, stringa
syscall
li $v0, 5
syscall
move $a0, $v0
jal div_6
li $v0, 10
syscall

div_6:
#a0 viene sovrascritto
#ra viene sovrascritto
addi $sp, $sp, -8
sw $ra, 4($sp)

whileloop:
beq $a0, $zero, outwhile
#Se lo faccio qui, perdo $t0
sw $a0, 0($sp)
jal div_2
# qui ho perso $a0, che mi serve dopo
lw $a0, 0($sp)
li $t0, 1
bne $v0, $t0, else
li $t0, 3
div $a0, $t0 #y div 3
mfhi $t0 # resto in $t0
bne $t0, $zero, else
li $v0, 4
sw $a0, 0($sp)
la $a0, number
syscall
li $v0, 1
lw $a0, 0($sp)
syscall
li $v0, 4
la $a0, divisable
syscall
lw $a0, 0($sp)
j exitif
else: 
li $v0, 4
sw $a0, 0($sp)
la $a0, number
syscall
li $v0, 1
lw $a0, 0($sp)
syscall
li $v0, 4
la $a0, not_divisable
syscall
lw $a0, 0($sp)
exitif:
addi $a0, $a0, -1
j whileloop
outwhile:
lw $ra, 4($sp)
addi $sp, $sp, 8
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
