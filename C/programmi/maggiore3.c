//programma che ciede in input tre nuemri interi e restituisce in oputput il maggiore dei tre numeri

#include <stdio.h>

int main ()
{
    int a, b, c;

    printf ("inserisci tre numeri interi: \n");
    scanf ("%d%d%d", &a, &b, &c);

    if (a > b && a > c)
    {
        printf ("il maggiore fra i tre numeri è: %d\n", a);
    }
    
    else if (b > a && b > c)
    {
        printf ("il maggiore fra i tre numeri è: %d\n", b);
    }

    else if (c > a && c > b)
    {
        printf ("il maggiore fra i tre numeri è: %d\n", c);
    }
    
    
}