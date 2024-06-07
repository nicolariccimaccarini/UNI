#include <stdio.h>
#define DIM 3

typedef int riga[DIM];
typedef riga matrice[DIM];

void lettura(int M[][DIM])
{
    int i, j;

    for (i=0; i<DIM; i++)
    {
        for (i=0; i<DIM; i++)
        {
            scanf ("%d", &M[i][j]);
        }
    }
}

int simmetrica(int M[][DIM])
{
    int i, j;

    for (i=0; i<DIM; i++)
    {
        for (j=i+1; j<DIM; j++)
        {
            if (M[i][j] != M[j][i])
            {
                return 0;
            }
        }
    }
    
    return 1;
}

int main()
{
    int matrice[DIM][DIM];
    
    lettura(matrice);

    if (simmetrica(matrice))
    {
        printf ("La matrice non è simmetrica.\n");
    }

    else
    {
        printf ("La matrice è simmetrica.\n");
    }

    return 0;
}