//strampa la somma di tre numeri richiesti all'utente usando solo due variabili 

#include <stdio.h>

main () {
    int numero;
    int somma = 0; //contiene la somma dei nuemri inseriti a ogni istante
   
    printf ("Digita un numero intero\n");
    scanf ("%d, &numero"); 
    somma = somma + numero;
   
    printf ("Digita un numero intero\n");
    scanf ("%d, &numero"); 
    somma = somma + numero;

    
    printf ("Digita un numero intero\n");
    scanf ("%d, &numero"); 
    somma = somma + numero;
   
    printf ("La somma è %d\n", somma);
}