.data
magic: .word 1325
enter: .asciiz "Enter your guess: "
wrong: .asciiz "Wrong!"
right: .asciiz "RIGHT!"
high: .asciiz "...too high\n"
low: .asciiz "...too low\n"
yes: .asciiz " is the magic number!!!"

.text
li $s0, 0 #guess
lw $s1, magic
li $s2, 0 #i
li $t0, 10
#condizione di terminazione
forloop:
slt $t1, $s2, $t0 #t1=1 se i<10
sne $t2, $s0, $s1 #t2=1 se guess!=magic
and $t1, $t1, $t2 #se t1=1, si itera
beq $t1, $zero, esci
li $v0, 4
la $a0, enter
syscall
li $v0, 5
syscall
#guess in v0
move $s0, $v0 #guess aggiornato
beq $s0, $s1, uguale
li $v0 4
la $a0, wrong
syscall
bgt $s0, $s1, esagerato
li $v0, 4
la $a0, low
syscall
j fineif
esagerato:
li $v0, 4
la $a0, high
syscall
fineif:
j exit
uguale:
li $v0, 4
la $a0, right
syscall
li $v0, 1
move $a0, $s0
syscall
li $v0, 4
la $a0, yes
syscall

exit:
addi $s2, $s2, 1
j forloop
esci:
li $v0, 10
syscall