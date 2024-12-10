#include <stdio.h>

int main()
{
    int n;
    int i;
    int a = 0;

    for ( i = 1; i <= 5; i++)
    {
        scanf ("%d", &n);
        a = a + n;
        
        printf ("%d\n", a);
    }
    
}