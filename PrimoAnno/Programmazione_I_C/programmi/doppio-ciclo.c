#include <stdio.h>

int main ()
{
    int n = 0, d;

    do
    {
        d = n * 2;
        n = n + 1;
        printf ("%d\n", d); 
    } while (n < 10);   
}