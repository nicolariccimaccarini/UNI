.data
a: .word 8
b: .word 9
c: .word 10, 11, 12, 13 

.text
addi $s0, $zero, 0x10010000	# metti in $s0 l'indirizzo 0x10010000
lb $t0, 0($s0) 			# NUOVA ISTRUZIONE LOAD BYTE