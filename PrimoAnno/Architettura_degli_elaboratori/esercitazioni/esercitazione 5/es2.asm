.data
inputstring: .asciiz "Enter a integer value: "
lessthan10: .asciiz "The value is less than 10"
greaterthan10 : .asciiz "The value is greater than 10"

.text
#stampo inputstring
li $v0, 4
la $a0, inputstring
syscall 

#prendo in input un intero
li $v0, 5
syscall
move $a0, $v0

li $t0, 10
blt $a0, $t0, minore
li $v0, 4
la $a0, greaterthan10
syscall
j exitmain

exitmain:
li $v0, 10
syscall

minore:
la $a0, lessthan10
li $v0, 4
syscall
