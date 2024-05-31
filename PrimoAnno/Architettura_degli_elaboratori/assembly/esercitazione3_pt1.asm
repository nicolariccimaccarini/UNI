.data
# max valore positivo rappresentabile in C2
var: .word -2147483648
#negativo piu' piccolo -2.147.483.648

.text
Main:
#li $a0, 55
lw $a0, var
jal neg
move $s0, $v0
li $v0, 1
syscall

neg:
subu $t0, $zero, $a0
# stampa del valore
move $a0, $t0
li $v0, 1
syscall
move $v0, $t0
jr $ra