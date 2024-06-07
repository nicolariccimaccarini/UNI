#include <stdio.h>

int main()
{
    int n, c;

    printf ("Inserisci un numero intero positivo:\n");
    scanf ("%d", &n);
    
    if (n==1)
    {
        return 0;
    }
    else if (n>1 && n%2==0)
    {
        c=n/2;
        printf ("%d\n", c);
    }
    else
    {
        c=(n*3)+1;
        printf ("%d\n", c);
    }
    
}