.data 
A: .word 15
B: .word 10
C: .word 7
D: .word 2
E: .word 18
F: .word -3
Z: .space 4
# Z puo' essere gestito direttamente sui registri del processore. 
teto: .asciiz "il risultato vale "

.text
lw $a0, A
lw $a1, B
lw $a2, C
lw $a3, D
lw $s0, E
lw $s1, F

addi $sp, $sp, -8
sw $s0, 4($sp)	# E sullo stack
sw $s1, 0($sp)	# F sullo stack
jal do_math
addi $sp, $sp, +8
move $t0, $v0
la $a0, testo
li $v0, 4
syscall
move $a0, $t0
li $v0, 1
syscall
li $v0, 10
syscall

do_math:
# gestiamo z direttamente nei registri del processore
# alloco un frame per la push di $s0 e $s1, che
# usero' per le variabili E ed F, nonche' per la push del frame pointer

addi $sp, $sp, -12
sw $fp, 8($sp)
sw $s0, 4($sp)
sw $s1, 0($sp)


addi $t0, $a0, $a1
sub $t1, $a2, $a3
lw $s0, 8($fp)	# E in $s0
lw $s1, 4($fp)	# F in $s1
add $t2 $s0, $s1 	# E + F
sub $t3, $a0, $a2 	# A - C

add $t0, $t0, $t1
add $t0, $t0, $t2
sub $v0, $t0, $t3

lw $s1, 0($sp)
lw $s0, 4($sp)	# faccio gli offset in ordine inverso --> POP
lw $fp, 8($sp)
addi $sp, $sop, +12
jr $ra
