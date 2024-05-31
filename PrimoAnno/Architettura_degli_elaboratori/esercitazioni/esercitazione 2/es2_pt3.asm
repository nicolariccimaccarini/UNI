.data
A: .word 15
B: .word 10
C: .word 7
D: .word 2
E: .word 18
F: .word -3 
Z: .word 0
testo: .asciiz "Il risultato vale: "

.text
main:
lw $a0, A
lw $a1, B
lw $a2, C
lw $a3, D
lw $t0, E
lw $t1, F

jal do_match

sw $v0, Z 	#salvo il valore di ritorno nella variabile Z

#stampo il valore di ritorno
li $v0, 1
lw $a0, Z
syscall

#termino il programma
li $v0, 10
syscall

do_match:
addiu $sp, $sp, -4 	#creo spazio per la variabile locale Z
sw $ra, ($sp) 		# salvo il return address

#eseguo il calcolo: z = (a+b) + (c-d) + (e+f) - (a-c)
lw $t2, ($a0) 		#carico a in $t2
lw $t3, ($a1) 		#carico b in $t3
add $t4, $t2, $t3 	# a + b e la carico in $t4

lw $t2, ($a2) 		# carico c in $t2
lw $t3, ($a3) 		# carico d in $t3
sub $t5, $t2, $t3 	# c - d e la carico in $t5

lw $t2, ($t0) 		# carico e in $t2
lw $t3, ($t1) 		# carico f in $t3
add $t6, $t2, $t3	# e + f e la carico in $t6

lw $t2, ($a0) 		#carico a in $t2
lw $t3, ($a2) 		# carico c in $t2
sub $t7, $t2, $t3 	# a - c e la carico in $t7

# calcolo z
add $t4, $t4, $t5 	# (a+b) + (c-d) e salvo in $t4
add $t4, $t4, $t6 	# (a+b) + (c-d) + (e+f) e salvo in $t4
sub $t4, $t4, $t7	# (a+b) + (c-d) + (e+f) - (a-c) e salvo in $t4

# salvataggio del risultato
move $v0, $t4 		# copio il risultato il $v0

lw $ra, ($sp) 		# ripristino il return address
addiu $sp, $sp, 4 	# libero lo spazio della variabile locale