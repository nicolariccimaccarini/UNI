#include <stdio.h>

int main()
{
    int a, i;
    int c = 0;

    printf ("Inserisci un numero intero:\n");
    scanf ("%d", &a);

    for (i = 0; i <= a; i++)
    {
        if (a%i==0)
        {
            c++;
        }
    }
    
    if (c==2)
        {
            printf ("Il numero è primo!\n");
        }
    
    else 
    { 
        printf ("Il numero non è primo!\n");
    }

    
}