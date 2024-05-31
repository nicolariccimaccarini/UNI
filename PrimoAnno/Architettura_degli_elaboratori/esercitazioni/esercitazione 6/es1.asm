.data
stringinput: .asciiz "Enter a positive integer:"
stringoutput: .asciiz "Sum = "

.text
li $s0, 0 	#sum
#stampo stringa di input
la $a0, stringinput
li $v0, 4	
syscall

#scanf del numero intero
li $v0, 5
syscall
move $s1, $v0 	#metto in $s1 il numero preso in input'
li $s2, 1 	#count in $s2

forloop:
bgt $s2, $s1, esciloop 		#se $s2 > $s1 --> esci loop
add $s0, $s0, $s2 		# sum += count
addi $s2, $s2, 1		#++count
j forloop

esciloop:
la $a0, stringoutput
li $v0, 4	#stampo la stringa di output
syscall
li $v0, 1
move $a0, $s0	#stampo sum
syscall
li $v0, 10 	#esco dal programma
syscall