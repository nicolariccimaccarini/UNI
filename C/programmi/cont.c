#include <stdio.h>

int main()
{
    int i;
    int cont=0;
    int n;

    printf ("Inserisci un nuemero intero:\n");
    scanf ("%d", &n);

    for (i=0; i<n; i++)
    {
        cont++;
    }
    return 0;
}