#include <stdio.h>

int main()
{
    int n;

    for (n=1; n<=100; n++)
    {
        if (n%3==0 && n%5==0)
        {
            printf ("bimbum\n");
        }
        
        else if (n%3==0)
        {
            printf ("bim\n");
        }

        else if (n%5==0)
        {
            printf ("bum\n");
        }

        else 
        {
            printf (" %d\n ", n);
        }
    }

}