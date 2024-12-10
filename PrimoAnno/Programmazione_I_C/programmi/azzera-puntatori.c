#include <stdio.h>

int azzera(int *p)
{
    return *p=0;
}

int main()
{
    int a;
    scanf ("%d", &a);
    azzera(&a);
    printf ("%d\n", a);
    return 0;
}
