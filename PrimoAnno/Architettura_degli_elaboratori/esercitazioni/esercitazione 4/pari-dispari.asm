.data
strinput: .asciiz "inserisci un numero: "
outputstringpari: .asciiz "il numero e' pari"
outputstringdispari: .asciiz "il numero e' dispari"

.text
li $v0, 4
la $a0, strinput   #stasmpo la stringa
syscall

li $v0, 5 	#prendo in input il numero da processare
syscall
move $a0, $v0

jal paridispari
beq $v0, $zer0, parimain	#se $v0 = 0 il numero e' pari
la $a0, outputstringdispari	#se $v0 =/ 0 il numero e' dispari
li $v0, 4
syscall
j exitmain

parimain:
la $a0, outputpari	#se il numero e' pari stampo outputpari
li $v0, 4
syscall

exitmain:
li $v0, 10 	#termino il programma
syscall

paridispari:
li $t0, 2	#carico l'intero 2 nel registro $t0
div $a0, $t0	#divido il numero preso in input per 2
mfhi $t0	#metto nel registro $t0 il resto della divisione
beq $t0, $zero, pari 	# se il resto = 0 allora il numero e' pari e quindi $v0 = 0
li $v0, 1	#setto $v0 = 1
j exit

pari:
li $v0, 0 	#setto $v0 = 0

exit: 
jr $ra
