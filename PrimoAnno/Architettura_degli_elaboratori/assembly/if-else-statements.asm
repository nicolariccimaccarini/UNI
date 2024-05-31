.data
i: .word 3
j: .word 6

.text
#Prepara la condizione
lw $s0, i
lw $s1, j
#Verifica della condizione, con «branch»
slt $t0, $s0, $s1 #$t0 vale 1 se (i<j)
bne $t0, $zero, ramothen
#Ramo «else»
li $s0, 1
j Exit
#Ramo «then»
ramothen:
li $s0, 0
#Uscita dallo statement condizionale
Exit:
move $a0, $s0
li $v0, 1
syscall
li $v0, 10
syscall