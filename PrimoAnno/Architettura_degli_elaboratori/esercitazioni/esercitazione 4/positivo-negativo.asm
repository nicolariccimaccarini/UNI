.data
inputstring: .asciiz "inserisci un numero intero positivo o negativo: "
outputpositivo: .asciiz "il numero e' positivo"
outputnegativo: .asciiz "il numero e' negativo"

.text
#stampo inputstring:
li $v0, 4
la $a0, inputstring 	
syscall

#prendo in input il numero da processare
li $v0, 5
syscall
move $a0, $v0

jal positivonegativo
la $a0, outputnegativo
li $v0, 4 
syscall
j exitmain

exitmain:
li $v0, 10
syscall

positivonegativo:
bgt $a0, $zero, positivo
jr $ra

positivo:
la $a0, outputpositivo
li $v0, 4
syscall
