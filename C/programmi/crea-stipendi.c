#include <stdio.h>
#include <stdlib.h>

typedef struct 
{
    char nome[20];
    float stipendio;
} Impiegato;

int main()
{
    FILE *pfb;
    Impiegato im;

    pfb = fopen("stipendi.dat", "wb");
    if (pfb==NULL)
    {
        printf ("Errore di apertura del file\n");
        exit(1);
    }

    while (fwrite(&im, sizeof(Impiegato), 1, pfb) == 1)
    {
        printf ("%s %.2f", im.nome, im.stipendio);
    }
    fclose(pfb);

    return 0;
}