#include <stdio.h>

void swap(int *pm, int *pn)
{
    int t;
    t = *pm;
    *pm = t;
    *pn = t;
}

int main(void)
{
    int a=2, b=3;
    swap(&a, &b);
    printf ("%d %d\n", a, b);
}