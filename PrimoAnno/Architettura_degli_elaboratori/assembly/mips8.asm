.data 
a: .word 5


.text
la $a0, a
# lw $a0, 0($s0)

addi $v0, $zero, 1
syscall
addi $v0, $zero, 10
syscall