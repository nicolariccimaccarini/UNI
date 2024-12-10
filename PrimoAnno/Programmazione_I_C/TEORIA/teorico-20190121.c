#include <stdio.h>

int f(int d)
{
    int a[] = {1, 8, 5, 7, 9, 2};

    if (d == 0 || d > 6)
    {
        return 1;
    }

    else 
    {
        return a[d - 1];
    }
}

int main(int argc, char* argv[])
{
    int a=0, i[]={1, 1, 1};

    while (a<2)
    {
        i[a] = f(++a);
    }

    for (i[0] = 2; i[0] >= 0; i[0]--)
    {
        printf ("%d ", i[i[0]]);
        printf ("\n");
    }

    return 0;
}