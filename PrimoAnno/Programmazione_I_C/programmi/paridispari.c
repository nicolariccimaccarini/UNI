#include <stdio.h>

int main () 
{
    int a;
    printf ("Inserisci un numero \n");
    scanf ("%d", &a);

    if (a%2==0)
    {
        printf ("il numero inserito è pari.");
    }
    
    else {
        printf ("il numero inserito è dispari.");
    }
}