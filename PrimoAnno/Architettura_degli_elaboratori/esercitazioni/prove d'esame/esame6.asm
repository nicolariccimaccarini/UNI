.data
X: .word 1 2 3 4 5 
Y: .word 1 2 3 4 55
lenght: .word 5
string1: .asciiz "Viene "
space: .asciiz " "
endl: .asciiz "\n"

.text
la $s0, X	# carico l'indirizzo di X in $s0
la $s1, Y	# carico l'indirizzo di Y in $s1
lw $s2, lenght 	# carico il valore di lenght in $s2

li $v0, 4
la $a0, string1
syscall

move $a0, $s0 	# sposto X in $a0
move $a1, $s1 	# sposto Y in $a1
move $a2, $s2 	# sposto lenght in $a2

jal correlationCoefficiente

move $s0, $v0

li $v0, 4
la $a0, string1
syscall

li $v0, 1
move $a0, $s0
syscall

j exitmain

exitmain: 
li $v0, 10
syscall

correlationCoefficiente:
# i = $t0
# sum_x = $t1
# sum_y = $t2
# sum_xy = $t3

# inizializzo a 0 i registri che andro' ad utilizzare 
li $t0, 0	# i
li $t1, 0	# sum_x
li $t2, 0	# sum_y
li $t3, 0	# sum_xy

forloop:
beq $t0, $a2, escifor 	# condizione di uscita del for

# metto in $t4, l'offset di [i]
sll $t4, $t0, 2

# $t8 --> X[i]
# $t9 --> Y[i]
move $t8, $a0	# carico l'indirizzo di X
move $t9, $a1	# carico l'indirizzo di Y

add $t8, $t8, $t4
lw $t8, 0($t8) 	# $t8 = X[i]

add $t9, $t9, $t4
lw $t9, 0($t9)	# $t9 = Y[i]

add $t1, $t1, $t8 	# sum_x += X[i]
add $t2, $t2, $t9	# sum_y += Y[i]

# prodotto XY = $t5
mul $t5, $t8, $t9
add $t3, $t3, $t5 	# sum_xy = sum_xy + X*Y

# incremento del loop
addi $t0, $t0, 1
j forloop

escifor:
li $v0, 1
move $a0, $t1 	# sum_x in $a0
syscall 	# stampo sum_x

li $v0, 4
la $a0, space
syscall

li $v0, 1
move $a0, $t2	# sum_y in $a0
syscall 	# stampo sum_y

li $v0, 4
la $a0, space
syscall

li $v0, 1
move $a0, $t3 	# sum_xy in $a0
syscall

li $v0, 4
la $a0, endl
syscall

# $t4 = molt di n * sum_xy
# $t5 = molt di sum_x * sum_y
mul $t4, $t3, $a2
mul $t5, $t1, $t2
sub $v0, $t4, $t5
jr $ra

