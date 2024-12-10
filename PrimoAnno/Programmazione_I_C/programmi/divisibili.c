#include <stdio.h>

int main()
{
    int a, b;

    printf ("Inserisci due numeri:\n");
    scanf ("%d%d", &a, &b);

    if (b==0)
    {
        printf ("Inserisci un numero diverso da 0:\n");
        scanf ("%d", &b);
    }
    
    if (a%b==0)
    {
        printf ("%d è divisibile per %d", a, b);
    }
    else
    {
        printf ("%d non è divisibile per %d", a, b);
    } 
}