#include <stdio.h>

int divisione(int *flag, int *p, int *q)
{
    int a, b;
    int *flag;

    scanf ("%d%d", &a, &b);

    if(!b)
    {
        flag=1;
        return;
    }

    *p=a/b;
    *q=a%b;
}

int main(void)
{
    int a, b, quoziente, resto, flag;

    divisione(&flag, &quoziente, &resto);

    if (!flag)
    {
        printf ("Quoziente: %d\n Resto: %d\n", quoziente, resto);
    }
}