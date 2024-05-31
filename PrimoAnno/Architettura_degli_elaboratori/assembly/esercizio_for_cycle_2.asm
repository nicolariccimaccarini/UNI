.data 
num: .word 0
count: .word 0
sum: .word 0

.text
lw $t0, num
lw $t1, count
lw $t2, sum

li $v0, 5
syscall
			#scanf
move $t0, $v0

loop_label:
bgt $t1, $t0, esci_label
#corpo loop

add $t2, $t2, $t1

#aggiornamento loop
addi $t1, $t1, 1
j loop_label

esci_label:
move $a0, $t2
li $v0, 1
syscall
li $v0, 10
syscall
