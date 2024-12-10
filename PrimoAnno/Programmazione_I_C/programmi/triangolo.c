#include <stdio.h>

int main()
{
    int a, b, c;

    printf ("Inserisci tre numeri interi:\n");
    scanf ("%d%d%d", &a, &b, &c);

    if (a == b && a == c && b == c)
    {
        printf ("Il trianfolo è equilatero\n");
    }
    
    else if ((a == c || a == b || b == c) && (a != b || a != c || b != c))
    {
        printf ("Il triangolo è isoscele\n");
    }

    else if (a != b && a != c && b != c)
    {
        printf ("Il triangolo è scaleno\n");
    }
    
}