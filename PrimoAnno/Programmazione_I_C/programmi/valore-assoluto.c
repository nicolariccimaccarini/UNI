#include <stdio.h>

int main()
{
    int n;

    printf ("inserisci un numero intero: \n");
    scanf ("%d", &n);

    if (n >= 0)
    {
        printf ("il valore assoluto del numero è: %d\n", n);
    }

    else {printf ("il valore assoluto del numero intero è: %d\n", -n);}
    
}