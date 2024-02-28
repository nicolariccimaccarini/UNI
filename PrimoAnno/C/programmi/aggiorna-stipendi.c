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
    pfb = fopen("stipendi.dat", "rb");
    Impiegato im;
    
    if (pfb==NULL)
    {
        printf ("Errore di apertura del file\n");
        exit(1);
    }

    while (fread(&im, sizeof(Impiegato), 1, pfb)==1)
    {
        if (im.stipendio<1500)
        {
            im.stipendio *= 1.1;
            fseek(pfb, -sizeof(Impiegato), SEEK_CUR);
            fwrite(&im, sizeof(Impiegato), 1, pfb);
            fflush(pfb);
        }
    }
    fclose(pfb);

    return 0;
}