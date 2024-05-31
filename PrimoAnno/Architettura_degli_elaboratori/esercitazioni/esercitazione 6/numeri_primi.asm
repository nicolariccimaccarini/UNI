.data
stringinput: .asciiz "Input a number larger than 1: "
notprime: .asciiz " is not a prime number"
isprime: .asciiz " is a prime number"

.text
# stampo la stringa di input
la $a0, stringinput
li $v0, 4
syscall

#scanf del numero intero
li $v0, 5
syscall
move $s0, $v0	# metto in $s1 il numero preso in input (x)

li $s1, 2 	# y
li $s2, 0 	# z

forloop:
beq $s1, $s0, escifor
div $s0, $s1 	#x%y
mfhi $t0 	# metto il resto della divisione nel registro $t0
bne $t0, $zero, nozero
li $v0, 4
la $a0, notprime
syscall
li $s2, 1 	# z = 1
j escifor

nozero:
addi $s1, $s1, 1 
j forloop

escifor:
bne $s2, $zero, fine 	# se z diverso da 0 esco dal ciclo
li $v0, 4
la $a0, isprime
syscall

fine:
li $v0, 10
syscall