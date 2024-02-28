#include <stdio.h>

int main()
{
    int n;

    printf ("insrisci un numero intero\n");
    scanf ("%d", &n);

    int i, j, c;

    for (i = 1; i < n; i++)
    {
        if (n%i==0)
        {
            c = 0;
            for (j = 2; j < i; j++)
            {
                if (i%j==0)
                {
                    c++;
                }
                
            }

            if (c==0)
        {
            printf("Il divisore primo di %d è: %d\n", n, i);
        }

        } 
    }
}