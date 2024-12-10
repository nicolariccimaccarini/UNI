#include <stdio.h>

int main()
{
    int a;

    printf ("inserisci un numero intero: \n");
    scanf ("%d", &a);

    if (a%2 != 0)
    {
        printf ("DISPARI");
    }
    
    else {return 0;}
}