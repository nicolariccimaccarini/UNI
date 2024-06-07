#include <stdio.h>
#define DIM 31

int main()
{
    int frequenze[DIM];
    int voto;
    int i;
    int somma_voti=0;
    int numero_voti=0;
    int moda=0;

    for (i=0; i<=30; i++)
    {
        frequenze[i]=0;
    }

    do //input
    {
        scanf ("%d", &voto);
        if (voto==-1)
        {
            break;
        }
        frequenze[voto]++;
    }while (1);

    //voto minimo
    i=0;
    while (frequenze[i]==0)
    {
        i++;
    }
    printf ("voto minimo: %d\n", i);

    //voto massimo
    i=30;
    while (frequenze[i==0])
    {
        i--;
    }
    printf ("voto massimo: %d\n", i);

    //media=somma/n_numeri
    for (i=0; i<=30; i++)
    {
        numero_voti+=frequenze[i];
        somma_voti+=i*frequenze[i];
    }
    printf ("media voti: %f\n", (float)somma_voti/numero_voti);

    //moda
    for (i=0; i<=30; i++)
    {
        if (frequenze[i]>frequenze[moda])
        {
            moda=i;
        }
    }
    
    printf ("Voti più frequenti:");
    for (i=0; i<=30; i++)
    {
        if (frequenze[i]==frequenze[moda])
        {
            printf (" %d", i);
        }
    }
    printf ("\n");
    return 0;
}