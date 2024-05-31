.data 
a: .word 5

.text
# STAMPARE 5 A VIDEO
addi $v0, $zero, 1
li $s0, 0x10010000	# addi $s0, $zero, 0x10010000 
lw $a0, 0($s0)
syscall

addi $v0, $zero, 10
syscall