.data 
a: .word 5


.text

lw $a0, a
addi $v0, $zero, 1
syscall
addi $v0, $zero, 10