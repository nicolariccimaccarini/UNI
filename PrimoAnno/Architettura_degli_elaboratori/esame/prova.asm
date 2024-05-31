.data
    arr: .space 40      # Array di 10 elementi interi (10*4=40 bytes)
    i:   .word 0        # Variabile intera i inizializzata a 0
    count: .word 0      # Variabile intera count inizializzata a 0
    input_text: .asciiz "Enter the Number: \n"
    reverse_text: .asciiz "\nReverse = "

.text
main:
    # Inizializzazione dei registri
    li $s0, 0          # $s0 conterrà il valore di num
    li $s1, 0          # $s1 conterrà il valore di rem
    la $s2, arr        # $s2 conterrà l'indirizzo di arr[0]
    li $s3, 0          # $s3 conterrà il valore di i
    li $s4, 0          # $s4 conterrà il valore di count

    # Stampare il messaggio "Enter the Number: "
    la $a0, input_text
    li $v0, 4
    syscall

    # Leggere l'input dell'utente e salvarlo in $s0
    li $v0, 5
    syscall
    move $s0, $v0      # Salva il numero inserito in $s0

    # Ciclo per calcolare i resti e salvare nell'array arr[]
loop:
    beq $s0, $zero, print_reverse   # Se num==0, salta alla stampa dell'array
    div $s0, $s0, 10     # Dividi num per 10 (resto in $s1, risultato in $s0)
    mfhi $s1             # Salva il resto in $s1
    sw $s1, 0($s2)       # Salva il resto nell'array arr[]
    addi $s2, $s2, 4     # Incrementa l'indirizzo dell'array
    addi $s3, $s3, 1     # Incrementa i (indice)
    addi $s4, $s4, 1     # Incrementa count
    j loop               # Ritorna al ciclo

print_reverse:
    # Stampa la stringa "\nReverse = "
    la $a0, reverse_text
    li $v0, 4
    syscall

    # Chiamata alla funzione "stampa" passando arr[] e count come argomenti
    move $a0, $s2        # Passa l'indirizzo di arr[0] come argomento
    move $a1, $s4        # Passa count come argomento
    jal stampa

    # Uscita dal programma
    li $v0, 10
    syscall

stampa:
    # La funzione "stampa" ha gli argomenti passati nei registri $a0 e $a1
    # (rispettivamente l'indirizzo dell'array e il numero di elementi da stampare)

    # Inizializzazione dei registri locali
    li $s0, 0           # $s0 conterrà il valore di i

print_loop:
    bge $s0, $a1, return   # Se i >= count, termina la funzione

    # Carica l'elemento dell'array da stampare
    lw $t0, 0($a0)
    move $a0, $t0        # Copia l'elemento in $a0 per la stampa

    # Stampa l'elemento
    li $v0, 1
    syscall

    # Stampa la virgola se non è l'ultimo elemento
    li $t1, 1
    beq $s0, $a1, no_comma   # Se i == count, non stampare la virgola
    li $v0, 11
    li $a0, ','
    syscall

no_comma:
    addi $s0, $s0, 1     # Incrementa i
    addi $a0, $a0, 4     # Passa al prossimo elemento dell'array
    j print_loop

return:
    jr $ra              # Ritorna al chiamante (main)

