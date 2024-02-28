#include <stdio.h>

int riga()
{
    int a;
   
    for (a=0; a<20; a++)
    {
        printf ("*");
    }
    
    printf ("\n");

    return 0;
}

int main()
{
    int i;

    for (i=0; i<5; i++)
    {
        riga();
    }

    return 0;
}