.data
count: .word 10
stringoutput1: .asciiz "Total of the first "
stringoutput2: .asciiz " number is "

.text
lw $s0, count	#count
move $t0, $s0
li $s1, 0	#sum
li $s2, 0	#i

forloop: 
beq $t0, $zero, esciloop
add $s1, $s1, $t0	#sum += i
addi $t0, $t0, -1	#i--
j forloop

esciloop:
la $a0, stringoutput1
li $v0, 4
syscall
move $a0, $s0
li $v0, 1 	#stampo count
syscall
la $a0, stringoutput2
li $v0, 4
syscall
move $a0, $s1
li $v0, 1	# stampo sum
syscall

li $v0, 10
syscall