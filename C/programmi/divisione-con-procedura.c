#include <stdio.h>

int divisione(int *p, int *q)
{
    int a, b;

    scanf ("%d%d", &a, &b);

    *p=a/b;
    *q=a%b;
}

int main(void)
{
    int a, b, quoziente, resto;

    divisione(&quoziente, &resto);

    printf ("Quoziente: %d\n Resto: %d\n", quoziente, resto);
}