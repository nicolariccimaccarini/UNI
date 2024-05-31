.data
    prompt: .asciiz "%d %d\n"
    criticalMsg: .asciiz "Found critical condition..Now breaking\n"
    errorMsg: .asciiz "something wrong happened!"

.text
.globl display
.globl main

# Funzione display
display:
    # Salvataggio dello stato del registro
    addi $sp, $sp, -4   # Crea spazio nello stack per la variabile locale z
    sw $ra, 0($sp)   # Salva il valore di $ra nello stack

    # Allocazione delle variabili locali
    move $s0, $a0   # Alloca ii (a0) nel registro $s0
    move $s1, $a1   # Alloca jj (a1) nel registro $s1
    sub $s0, $s0, $s1   # Calcola z = ii - jj

    # Confronto tra z e 0
    beq $s0, $zero, critical   # Salta alla label "critical" se z == 0

    # Stampa del messaggio di errore
    li $v0, 4   # Carica il codice di servizio 4 (stampa stringa)
    la $a0, errorMsg   # Carica l'indirizzo del messaggio di errore nella register $a0
    syscall

    # Uscita dal programma
    li $v0, 10   # Carica il codice di servizio 10 (uscita dal programma)
    syscall

critical:
    # Stampa del messaggio di condizione critica
    li $v0, 4   # Carica il codice di servizio 4 (stampa stringa)
    la $a0, criticalMsg   # Carica l'indirizzo del messaggio di condizione critica nella register $a0
    syscall

    # Ripristino dello stato del registro e uscita dalla funzione
    lw $ra, 0($sp)   # Ripristina il valore di $ra dallo stack
    addi $sp, $sp, 4   # Ripristina lo stack pointer
    jr $ra   # Salta all'indirizzo di ritorno salvato in $ra

# Funzione main
main:
    # Allocazione delle variabili locali
    li $s0, 1   # Alloca i con il valore 1 nel registro $s0
    li $s1, 1   # Alloca j con il valore 1 nel registro $s1

    outer_loop:
        # Ciclo esterno for (i=1; i<=3; i++)
        li $t0, 1   # Carica il valore 1 nella register $t0 (corrisponde a i)
        ble $s0, 3, inner_loop   # Salta al ciclo interno se i <= 3
        j end_outer_loop   # Altrimenti salta alla fine del ciclo esterno

    inner_loop:
        # Ciclo interno for (j=1; j<=3; j++)
        li $t1, 1   # Carica il valore 1 nella register $t1 (corrisponde a j)
        ble $s1, 3, print_and_check   # Salta alla stampa e al controllo se j <= 3
        j end_inner_loop   # Altrimenti salta alla fine del ciclo interno

    print_and_check:
        # Stampa di i e j
        move $a0, $s0   # Passa il valore di i come primo argomento
        move $a1, $s1   # Passa il valore di j come secondo argomento
        li $v0, 1   # Carica il codice di servizio 1 (stampa intero)
        syscall

        li $v0, 4   # Carica il codice di servizio 4 (stampa stringa)
        la $a0, prompt   # Carica l'indirizzo del prompt nella register $a0
        syscall

        # Confronto tra i e 2
        beq $s0, 2, check_j   # Salta al controllo di j se i == 2

        # Incremento di j
        addi $s1, $s1, 1   # Incrementa j di 1
        j inner_loop   # Salta all'inizio del ciclo interno

    check_j:
        # Confronto tra j e 2
        beq $s1, 2, call_display   # Salta alla chiamata della funzione display se j == 2

        # Incremento di j
        addi $s1, $s1, 1   # Incrementa j di 1
        j inner_loop   # Salta all'inizio del ciclo interno

    call_display:
        # Chiamata alla funzione display con i e j come argomenti
        move $a0, $s0   # Passa il valore di i come primo argomento
        move $a1, $s1   # Passa il valore di j come secondo argomento
        jal display   # Salta alla funzione display

        # Uscita dal ciclo interno
        j end_inner_loop

    end_inner_loop:
        # Incremento di i
        addi $s0, $s0, 1   # Incrementa i di 1
        j outer_loop   # Salta all'inizio del ciclo esterno

    end_outer_loop:
        # Uscita dal programma
        li $v0, 10   # Carica il codice di servizio 10 (uscita dal programma)
        syscall