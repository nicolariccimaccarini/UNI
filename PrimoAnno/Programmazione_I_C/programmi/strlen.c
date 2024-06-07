#include <stdio.h>
#define DIM 20

int main()
{
    char s[DIM];
    int i, cont=0;
    
    printf ("Inserisci una parola:\n");
    
    for (i=0; i<DIM; i++)
    {
        scanf ("%c", &s[i]);
        cont++;
    }
    
    printf ("La lunghezza della parola è: %d\n", cont);
}