#include <stdio.h>
#define DIM 30

int main()
{
    char s1[DIM];
    int i;
    int contatore=0;

    printf ("Inserisci una parola:\n");
    scanf ("%s", s1);

    for (i=0; s1[i]!='\0'; i++)
    {
        contatore++;
    }

    printf ("La lunghezza della parola ''%s'' è: %d\n", s1, contatore);

    return 0;
}