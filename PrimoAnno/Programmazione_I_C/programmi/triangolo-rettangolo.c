#include <stdio.h>

int main()
{
    int c1, c2, i;

    printf ("Inserisci tre numeri interi che saranno i due cateti e l'ipotenusa di un triuangolo:\n");
    scanf ("%d%d%d", &c1, &c2, &i);

    if (c1 != 0 && c2 != 0 && i != 0 && (c1 * c1) + (c2 * c2) == (i * i))
    {
        printf ("Il triangolo è rettangolo\n");
    }
    
    else if (c1 == 0 || c2 == 0 || i == 0)
    {
        printf ("DATI NON VALIDI!\n");
        return 0;
    }
    
    else
    {
        printf ("Il triangolo non è rettangolo\n");
    }
}