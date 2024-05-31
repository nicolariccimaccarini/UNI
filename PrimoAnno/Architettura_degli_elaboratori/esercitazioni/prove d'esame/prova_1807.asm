.data
i: .word 1
j: .word 1
string1: .asciiz "Found critical condition..Now breaking \n"
string2: .asciiz "something wrong happened!"
capo: .asciiz "\n"
space: .asciiz " "

.text
# inizializzo le variabili
lw $s0, i
lw $s1, j

forloop1:
#condizione di uscita dal for
li $t0, 3
bgt $s0, $t0, exit(1) 		# se i>3 esco dal for

forloop2:
#condizione di uscita secondo for
bgt $s1, $t0, exitfor2

#stampo i e j
li $v0, 1
move $a0, $s0		# printf i
syscall

li $v0, 4
la $a0, space
syscall

li $v0, 1
move $a0, $s1 		# printf j	
syscall

li $v0, 4
la $a0, capo		# printf \n
syscall

# ciclo if
li $t0, 2
bne $s0, $t0, check_j
j exitif 	# esco dal programma

check_j:
beq $s1, $t0, dysplay_funct
j exitif	# esco dal programma 

exitif:
# incremento j e salto nel secondo for
addi $s1, $s1, 1
j forloop2

exitfor2:
#incremento i e salto a forloop1
addi $s0, $s0, 1
j forloop

exit(1):
li $v0, 10
syscall


dysplay_funct:
# Salvataggio dello stato del registro
addi $sp, $sp, -4   # Crea spazio nello stack per la variabile locale z
sw $ra, 0($sp)   # Salva il valore di $ra nello stack

# Allocazione delle variabili locali
move $s0, $a0   # Alloca ii (a0) nel registro $s0
move $s1, $a1   # Alloca jj (a1) nel registro $s1
sub $s0, $s0, $s1   # Calcola z = ii - jj

# Confronto tra z e 0
beq $s0, $zero, critical   # Salta alla label "critical" se z == 0

# Stampa del messaggio di errore
li $v0, 4   # Carica il codice di servizio 4 (stampa stringa)
la $a0, string2   # Carica l'indirizzo del messaggio di errore nella register $a0
syscall

# Uscita dal programma
li $v0, 10   # Carica il codice di servizio 10 (uscita dal programma)
syscall

critical:
# Stampa del messaggio di condizione critica
li $v0, 4   # Carica il codice di servizio 4 (stampa stringa)
la $a0, string1   # Carica l'indirizzo del messaggio di condizione critica nella register $a0
syscall

# Ripristino dello stato del registro e uscita dalla funzione
lw $ra, 0($sp)   # Ripristina il valore di $ra dallo stack
addi $sp, $sp, 4   # Ripristina lo stack pointer
jr $ra   # Salta all'indirizzo di ritorno salvato in $ra
