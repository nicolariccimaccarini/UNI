.data

.text
li $t0, 0 	# metto i in $t0

Main:
li $v0, 1

Whileloop:
bge $t0, 10, exitwhile
move$a0, $t0
syscall

addi $t0, $t0, 1
j Whileloop

exitwhile:
li $v0, 10
syscall