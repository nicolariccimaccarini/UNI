.data
cubestring: .asciiz "Cube value of "
isstring: .asciiz " is "
capo: .asciiz "\n"

.text
jal display
j exit

exit:
li $v0, 10
syscall

display:  
# gestisco "i" su $s0
addi $sp, $sp, -12
sw $s0, 8($sp)
sw $ra, 4($sp) 
li $s0, 1
forloop:
bgt $s0, 10, exitfor
li $v0, 4
la $a0, cubestring
syscall
li $v0, 1
move $a0, $s0
syscall
li $v0, 4
la $a0, isstring
syscall
move $a0, $s0
sw $t0, 0($sp)
jal cube 
lw $t0, 0($sp)
move $a0, $v0
li $v0, 1
syscall
li $v0, 4
la $a0, capo
syscall
addi $s0, $s0, 1
j forloop

exitfor:
lw $ra, 4($sp)
lw $s0, 8($sp)
addi $sp, $sp, 12
jr $ra

cube:
mul $t0, $a0, $a0
mul $v0, $t0, $a0
jr $ra
