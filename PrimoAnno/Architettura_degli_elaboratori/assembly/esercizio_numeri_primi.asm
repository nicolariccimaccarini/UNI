.data
x: .word 0
z: .word 0
y: .word 2

.test

lw $t0, x
lw $t1, z
lw $t2, y
li $v0, 5

move $t1, $v0

loop_label:
beq $t2, $t0, esci_label

div $t0, $t2
mfhi $t5 	# metto il resto della divisione nel registro $t5

beq $t5, $zero, salto

