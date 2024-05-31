.data

.text

li $v0, 5
syscall

move $t0, $v0

li $t1, 1	# fine conteggio
li $t2, 0 	# indice count
li $v0, 1 	# syscall di stampa 

# verifica condizioni
loop_label:

beq $t1, $t0, esci_label
# corpo loop
add $t4, $t0, $t2

# aggiornamento
addi $t1, $t1, 1
j loop_label

esci_label:
li $v0, 10
syscall
