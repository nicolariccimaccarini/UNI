#include <stdio.h>

int riga(int n)
{

    int i;

    for (i=1; i<=n; i++)
    {
        printf ("*");
    }

    printf ("\n");
    
    return 0;
}

int rettangolo(int n, int m)
{
    int j;
    for (j=0; j<m; j++)
    {
        riga(n);
    }

    return 0;
}

int main()
{
    int m, n;

    printf ("Inserisci i numeri di righe e colonne:\n");
    scanf ("%d%d", &m, &n);

    rettangolo(m, n);

    return 0;
}