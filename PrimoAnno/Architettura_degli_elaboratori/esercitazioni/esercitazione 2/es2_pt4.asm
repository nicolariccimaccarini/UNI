.data
A: .word 15, 10, 7, 2, 18, -3
testo: .asciiz "Il risultato vale: "

.text
# inizializzazione del registro $t0 con l'indirizzo di A[0]
la $t0, A

# caricamento di A[0] in $t1
lw $t1, ($t0)

# caricamento di a[1] in $t2
lw $t2, 4($t0)

# caricamento di A[2] in $t3
lw $t3, 8($t0)

# caricamento di A[3] in $t4
lw $t4, 12($t0)

# caricamento di A[4] in $t5
lw $t5, 16($t0)

# caricamento di A[5] in $t6
lw $t6, 20($t0)

# calcolo della somma (A[0] + A[1])
add $t7, $t1, $t2

# Calcolo della differenza (A[2] - A[3])
sub $t8, $t3, $t4

# Calcolo della somma (A[4] + A[5])
add $t9, $t5, $t6

 # Calcolo della differenza (A[0] - A[2])
sub $s0, $t1, $t3

# calcolo del risultato finalle
add $s1, $t7, $t8
add $s1, $s1, $t9
sub $s1, $s1, $s0

# stampo testo
la $a0, testo
li $v0, 4
syscall

# stampo la somma Z
move $a0, $s1
li $v0, 1
syscall

#termino il programma
li $v0, 10
syscall
