.data
i: .word 1
j: .word 1

.text
lw $s0, i
lw $s1, j

whileloop:
bgt $s0, 4, exitloop
bgt $s1, 3, exitloop

move $a0, $s0
li $v0, 1
syscall

move $a0, $s1
li $v0, 1
syscall

addi $s0, $s0, 1
addi $s1, $s1, 1
j whileloop

exitloop:
li $v0, 10
syscall