#include <stdio.h>
#define DIM 3

typedef int riga[DIM];
typedef riga matrice[DIM];

int main()
{
    matrice M;
    int i, j;

    for (i=0; i<DIM; i++)
    {
        for (j=0; j<DIM; j++)
        {
            if (j==i)
            {
                M[i][j] = 1;
            }

            else
            {
                M[i][j] = 0;
            }
        }
    }

    for (i=0; i<DIM; i++)
    {
        for (j=0; j<DIM; j++)
        {
            printf ("%3d", M[i][j]);
        }
        printf ("\n");
    }

    return 0;
} 