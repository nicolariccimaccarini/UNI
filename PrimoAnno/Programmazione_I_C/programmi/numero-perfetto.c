#include <stdio.h>

int main()
{
    int i, n, somma_divisori;

    somma_divisori=0;

    for (i=2; i<1000; i++)
    {
        for (n=1; n<=i/2; n++)
        {
            if (i%n==0)
            {
                somma_divisori=somma_divisori+n;
            }
        }

        if (somma_divisori==i)
        {
            printf ("%d è un numero perfetto\n", i);
        }

        else 
        {
            printf ("%d non è un numero perfetto\n", i);
        }

        somma_divisori=0;
    }
}