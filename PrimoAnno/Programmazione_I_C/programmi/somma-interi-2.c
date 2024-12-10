#include <stdio.h>

int main()
{
    int n, i, somma = 0;

    scanf ("%d", &n);

    for (i = 1; i <= n; i++)
    {
       printf ("%d", i);
       if (i < n)
       {
        printf (" + ");
       }
       
       else {printf (" = ");}
       
       somma = somma + 1;
    }

    printf ("%d\n", somma);
    
}