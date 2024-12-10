#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define DIM 20

int rnd_int(int a, int b)
{
    return a + rand() % (b - a + 1);
}

int main()
{
    int i;

    for (i=0; i<DIM; i++)
    {
        printf ("%d\n", rnd_int(0, 30));
    }
}

