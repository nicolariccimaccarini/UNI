#include <stdio.h>

int f(int d)
{
    int a[] = {1, 8, 5, 7, 9, 2};

    if (d <= 0 || d > 6)
    {
        return 0;
    }

    else 
    {
        return a[d - 1];
    }
}

int main(int argc, char* argv[])
{
    int i = 8;

    while (!f(--i)) //while che non serve a nulla, confonde solo
        ;
    
    do 
    {
        printf ("%c", '0' + f(i--));
    } while (f(i));

    printf ("\n");
    return 0;
}