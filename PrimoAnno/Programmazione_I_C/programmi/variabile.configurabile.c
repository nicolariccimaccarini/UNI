#include <stdio.h>

int main()
{
    int a, b;
    char ch;
    int* p; //definisco il puntatore

    printf ("a o b?\n"); scanf ("%c", ch);

    if (ch=='a')
    {
        p=&a;
    }
    
    else
    {
        p=&b;
    }

    a=1; b=2;

    printf ("%d\n", *p);
    return 0;
}