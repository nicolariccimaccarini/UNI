.data
inputstring: .asciiz "ENTER A NUMBER: "
newline: .asciiz "\n"

.text
la $a0, inputstring
li $v0, 4	#stampo la stringa per l'imput
syscall
li $v0, 5
syscall
move $s0, $v0 	#metto il numero rpeso in input in i

whileloop1:
blt $s0, 1, exitloop
la $a0, newline
li $v0, 4
syscall
move $s1, $s0 	# dn = i
j whileloop2

whileloop2:
blt $s1, 1, exitloop2
move $a0, $s1
li $v0, 1	#stampo dn
syscall
addi $s1, $s1, -1	#dn = dn -1
j whileloop2

exitloop:
li $v0, 10
syscall

exitloop2:
addi $s0, $s0, -1	#i = i - 1
j whileloop1


