.data
num: .word 7
valueoutput: .asciiz "The value is: "
valuegreaterthan1: .asciiz "The value is greater than 1"
valuegreaterthan10: .asciiz "The value is greater than 10"

.text
lw $a0, num
la $t0, 10

blt $a0, $t0, numless10
la $a0, valuegreaterthan10
li $v0, 4
syscall
j exitmain

exitmain:
li $v0, 10
syscall

numless10:
la $t0, 1
beq $a0, $t0, numequal1
la $a0, valuegreaterthan1
li $v0, 4
syscall
j exitmain

numequal1:
la $a0, valueoutput
li $v0, 4
syscall

lw $a0, num
li $v0, 1
syscall