.data 
a: .byte 8
Stringa: .asciiz "AB"
b: .byte 9
c: .byte 10, 11, 12, 13

.text
li $v0, 1	# addi $v0, $zero, 1
li $a0, 42	# addi $a0, $zero, 42
syscall

addi $v0, $zero, 10 
syscall 