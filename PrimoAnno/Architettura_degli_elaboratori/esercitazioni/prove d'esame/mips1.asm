.data
array: .word 1 2 3 4 5 6
n: .word 6 	# dimensione dell'array
key: .word 3 	# chiave da cercare 
outputstring: .asciiz "Element found at position: "

.data
#main
la $a0, array 	# carico l'indirizzo base dell'array nel registro $a0
lw $a1, n	# carico la dimensione dell'array nel registro $a1
lw $a2, key	# carico la chiave da cercare nel registro $a2

jal findElement
j exitmain

exitmain:
li $v0, 10
syscall

findElement:
addi $sp, $sp, -4
sw $s0, 0($sp)
li $s0, 0
forloop:
beq $s0, $a1, escifor 	# condizione di uscita ciclo for 

sll $t0, $t0, 2
add $t0, $t0, $a0
lw $t0, 0($t0)		#arri[i]
bne $t0, $a2, esciif	# condizione di uscita ciclo if
li $v0, 4
la $a0, outputstring
syscall
li $v0, 1
addi $a0, $s0, 1
syscall
j escifor

esciif:
addi $s0, $s0, 1
j forloop

escifor:
lw $s0, 0($sp)
addi $sp, $sp, 4
jr $ra                                                                                                