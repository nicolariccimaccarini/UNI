.data
magicnumber: .word 1325
inputstring: .asciiz "Enter your guess: "
right: .asciiz "RIGHT! "
right2: .asciiz " is the magic number. \n"
wrong: .asciiz "wrong... "
low: .asciiz " too low.\n"
high: .asciiz " too high.\n"

.text
li $s0, 0	# guess = 0
lw $s1, magicnumber	
li $s2, 0	# i = 0 (tentativi per indovinare il magic number)
li $t0, 10
forloop:
slt  $t1, $s2, $t0 	# t1 = 1 se i<10
sne $t2, $s0, $s1	# t2 = 1 se guess!=magic
and $t1, $t1, $t2 	# se t1 = 1, si itera
beq $t1, $zero, esci	# il programma esce se i>10 e guess=magic

la $a0, inputstring
li $v0, 4		# stampo stringa input
syscall 
li $v0, 5		# scanf del numero preso in input
syscall
move $s0, $v0		# guess = numero preso in input
beq $s0, $s1, indovinato 	# se guess=magic ho indovinato
la $a0, wrong
li $v0, 4
syscall
bgt $s0, $s1, greater 	# se guess > magic stampo to high
la $a0, low
li $v0, 4		# se guess < magic stampo to low
syscall

addi $s2, $s2, 1
j forloop

esci:
li $v0, 10
syscall

indovinato:
la $a0, right
li $v0, 4	# stampo "RIGHT"
syscall
move $a0, $s0
li $v0, 1	# stampo guess
syscall
la $a0, right2
li $v0, 4	# stampo  %d is the magic number.\n
syscall
j esci

greater:
la $a0, high
li $v0, 4	# stampo "too high"
syscall

