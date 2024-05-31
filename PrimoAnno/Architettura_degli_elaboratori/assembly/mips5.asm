.data
a: .byte 8
Stringa: .asciiz "AB"
b: .byte 9
c: .byte 10, 11, 12, 13

.text
addi $s0, $zero, 0x10010004	
lb $t0, 0($s0) 
addi $v0, $zero, 10
syscall
