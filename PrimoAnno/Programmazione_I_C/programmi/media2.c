#include <stdio.h>

int main()
{
    int a, b;
    float media;

    printf ("Inserisci due numeri interi:\n");
    scanf ("%d%d", &a, &b);

    media=(a+b)/2.0;

    printf ("La media fra i due numeri è: %f\n", media);
}