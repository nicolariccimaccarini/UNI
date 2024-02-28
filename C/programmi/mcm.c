#include <stdio.h>

int main()
{
    int a, b;
    int i;

    printf ("Inserisci due numeri interi:\n");
    scanf ("%d%d", &a, &b);

    for (i=1; i++;)
    {
        if (i%a==0 && i%b==0)
        {
            printf ("mcm(%d, %d) = %d\n", a, b, i);
            return 0;
        }
    }
}