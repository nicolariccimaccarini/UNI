#include <stdio.h>

int main ()
{
    int numero;

    printf ("Inserisci l'ipotenusa massima:\n");
    scanf ("%d", &numero);

    int a, b, c;

    for (a=1; a<=numero; a++)
    {
        for (b=a; b<=numero; b++)
        {
            for (c=b; c<=numero; c++)
            {
                if (a*a + b*b == c*c)
                {
                    printf ("%d %d %d\n", a, b, c);
                }
            }
        }
    }


}    