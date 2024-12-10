#include <stdio.h>

void swap_ordina(int *p, int *b)
{
    int t;
    t = *b;
    *b = *p;
    *p = t;
}

int main(void)
{
    int a, b;
    scanf ("%d%d", &a, &b);
    
    if (b<a)
    {
        swap_ordina(&a, &b);
    }

    printf ("%d %d\n", a, b);
    return 0;
}