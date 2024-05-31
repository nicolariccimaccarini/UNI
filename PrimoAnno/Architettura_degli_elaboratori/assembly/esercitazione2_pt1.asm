.data

a: .word 15
b: .word 10
c: .word 7
d: .word 2
e: .word 18
f: .word -3
z: .space 4

.eqv base, $t0
.eqv temp, $t1
.eqv temp2, $t2

.text

# carica l'indirizzo nel registro
lui base, 0x1001

# $s1 = A + B
lw temp, 0(base)
lw temp2, 4(base)
add $s1, temp, temp2

# $s2 = C - D
lw temp, 8(base)
lw temp2, 12(base)
sub $s2, temp, temp2

#$s1 = (A + B) + (C - D) --> $s1 = $s1 + $s2
add $s1, $s1, $s2

# $s2 = E + F
lw temp, 16(base)
lw temp2, 20(base)
add $s2, temp, temp2

#$s1 = (A + B) + (C - D) + (E + F) --> $s1 = $s1 + $s2
add $s1, $s1, $s2

# $s2 = A - C
lw temp, 0(base)
lw temp2, 8(base)
sub $s2, temp, temp2

# $s1 = (A + B) + (C - D) + (E + F) - (A - C) --> $s1 = $s1 - $s2
sub $s1, $s1, $s2

# salvo la variabile in z
sw $s1, 24(base)

#printo la variabile z
addi $v0, $0, 1
add $a0, $s1, $zero


syscall
