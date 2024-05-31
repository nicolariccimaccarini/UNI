.data
inputstring: .asciiz "Enter a number: "
reversestring: .asciiz "\nReverse = "
i: .word 0
count: .word 0
arr: .space 40	# array di 10 elementi interi  

.text
lw $s0, i	# $s0 = i
lw $s1, count	# $s1 = count
la $s2, arr	# carico l'indirizzo dell'array in $s2
# gestisto poi num nel registro $s3 e rem nel registro $s4

# stampo la stringa di input
li $v0, 4
la $a0, inputstring
syscall

# faccio la scanf di num
li $v0, 5
syscall 
move $s3, $v0	# sposto il valore preso in input nel registro $s3 (num)

# ciclo while
whilecycle:
beq $s3, $zero, exitwhile	# condizione di uscita dal while

li $s5, 10	# carico in $s5 la costante 10 che mi servira' per la divisione
div $s3, $s5	# num%10
mfhi $s4 	# metto il resto della divisione nel registro $s4 (rem) 

# carico in $s5 l'offset che mi servira' per muovermi nell'array
sll $s5, $s0, 2
add $s5, $s5, $s2	# ottengo arr[i]
sw $s4, 0($s5) 	# arr[i] = rem

mflo $s3	# carico in $s3 num/10

# incremento le variabili e jumpo a whilecycle
addi $s0, $s0, 1	# i++
addi $s1, $s1, 1	# count++
j whilecycle

exitwhile:
# stampo "\nReverse = "
li $v0, 4
la $a0, reversestring
syscall

move $a1, $s1	# salvo il valore attuale di count nel registro $a1 per passarlo alla funzione stampa
move $a2, $s2	# salvo il valore attuale di arr nel registro $a0 per passarlo alla funzione stampa
# PICCOLO APPUNTO
# ho usato a2 al posto di a0 perche' altrimenti nella riga 80 quando paso l'indirizzo della stringa da mandare in output a $a0 mi creava un loop infinito e mi crasha mars

jal stampa	# jumpo alla funzione stampa

li $v0, 10	# esco dal programma
syscall


stampa:
# preparo $sp per passarci dentro i valori che mi serviranno per la funzione
addi $sp, $sp, -12
sw $s0, 0($sp)	# preservo i nello stack frame
sw $s1, 4($sp)	# preservo arr nello stack frame
sw $s2, 8($sp)	# preservo count nello stack frame

move $s2, $a2 	# passo a $s2 l'indirizzo dell'array
li $s0, 0	# inzializzo la i che mi servira' nel ciclo for

forcycle:
beq $s0, $a1, exitfor	# condizione di uscita dal ciclo for

sll $s1, $s0, 2 #preparo l' offset per accedere all'array
add $s1, $s2, $s1 # offset + arr (in questo caso vec)
lw $s1, 0($s1) # carico in $s1 il valore di vec[i]

# faccio la printf di vec[i]
li $v0, 1
move $a0, $s1
syscall

# incremento la i del ciclo for e jumpo a forcycle
addi $s0, $s0, 1 
j forcycle

exitfor:
# ripristino il valore dei registri e di $sp
lw $s0, 0($sp)
lw $s1, 4($sp)
lw $s2, 8($sp)
addi $sp, $sp, 12 

jr $ra 

