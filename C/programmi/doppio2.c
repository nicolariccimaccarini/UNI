#include <stdio.h>

int main () {
    int v; //definizione (MEMORIA CENTRALE)
    printf (" Inserisci un numero intero \n" ); 
    scanf ("%d" , & v ); //input
    printf("Il doppio è: %d\n",  v * 2); //output (CPU)
}