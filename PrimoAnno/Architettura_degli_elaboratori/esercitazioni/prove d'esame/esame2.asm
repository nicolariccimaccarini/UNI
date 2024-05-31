.data
inputstring: .asciiz "Inserire due numeri interi separandoli mediante CARRIAGE RETURN"
outputstring: .asciiz "Il minimo comune multiplo vale: "
cr: .asciiz "\n" 	# carriage return

.text
li $v0, 4
la $a0, inputstring
syscall

li $v0, 4
la $a0, cr
syscall

li $v0, 5
syscall
move $s0, $v0	# num1 --> $s0

li $v0, 4
la $a0, cr
syscall

li $v0, 5
syscall
move $s1, $v0	# num2 --> $s1


jal minimoComuneMultiplo

# sposto il valore di ritorno della funzione minimoComuneMultiplo nella variabile $s0
move $s0, $v0 

# stampo mcm
li $v0, 4
la $a0, outputstring
syscall

li $v0, 1
move $a0, $s0
syscall

j exitmain

	
exitmain:
li $v0, 10
syscall	


massimoComunDivisore:
addi $sp, $sp, -12
sw $t1, 8($sp)
sw $t0, 4($sp)
sw $ra, 0($sp)
move $s0, $t0	# a = n1
move $s1, $t1 	# b = n2

# ciclo while
whilecycle:
ble $s1, $zero, exitwhile 	# condizione di uscita dal while
div $s0, $s1
mfhi $t0	# metto il resto della divisione nel registro $t1 --> resto = a % b
move $s0, $s1 	# a = b
move $s1, $t0	# b = resto
j whilecycle

exitwhile:
move $v0, $s0	# sposto a in $v0
lw $ra, 0($sp)
lw $t0, 4($sp)
lw $t1, 8($sp)
addi $sp $sp, 12
jr $ra

minimoComuneMultiplo:
addi $sp, $sp, -12
sw $s1, 8($sp)
sw $s0, 4($sp)
sw $ra, 0($sp)
move $t0, $s0 	# sposto n1 in un registro temporaneo
move $t1, $s1 	# sposto n2 in un registro temporaneo

jal massimoComunDivisore
move $t2, $v0 	# sposto il risultato di massimoComunDivisore in $t2

mul $t0, $t0, $t1	# n1 * n2
div $t0, $t2
mflo $v0
lw $ra, 0($sp)
lw $s0, 4($sp)
lw $s1, 8($sp)
addi $sp $sp, 12
jr $ra