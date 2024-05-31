.data 
a: .half 8
b: .half 9
c: .half 10, 11, 12, 13

.text
addi $s0, $zero, 0x10010000	
lb $t0, 0($s0) 
