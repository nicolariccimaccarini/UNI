// calcolo del valore assoluto con l'uso delle espressioni condizionali al posto dei cicli if

#include <stdio.h>

int main()
{
    int n;
    int a;

    printf ("Inserisci un numero intero:\n");
    scanf ("%d", &n);

    printf ("|%d| = %d\n", n, n>=0 ? n : -n);
}