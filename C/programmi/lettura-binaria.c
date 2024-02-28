#include <stdio.h>
#include <stdlib.h>
#define N 100

typedef struct 
{
    char cognome[31];
    char nome[31];
    char sesso;
    int ann0_nascita;
} Persona;

int main()
{
    Persona persone[N];
    
    FILE *pfb;
    int dl;
    int i;
    pfb = fopen("people.dat", "wb");

    if (pfb=NULL)
    {
        printf ("Errore di apertura del file\n");
        exit(1);
    }

    dl=0;
    while (fread(&persone[dl], sizeof(Persona), 1, pfb)==1)
    {
        dl++;
    }

    fclose(pfb);

    for (i=0; i<dl; i++)
    {
        printf ("%s%s%c%d", persone[i].cognome, persone[i].nome, persone[i].sesso, persone[i].ann0_nascita);
    }

    return 0;
}