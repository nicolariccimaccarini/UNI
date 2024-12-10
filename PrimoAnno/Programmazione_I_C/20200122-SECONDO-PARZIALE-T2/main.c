#include <stdio.h>
#include <stdlib.h>

#include "listaCani.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando: argomenti insufficienti\n");
        exit(1);
    }

    Lista l;
    Cane c;
    FILE* pfb;

    pfb = fopen("vaccini.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    while (fread(&c, sizeof(Cane), 1, pfb) == 1)
    {
        aggiorna(&l, c);
    }
    fclose(pfb);

    stampaVacciniMancanti(l);

    stampaVaccinati(l);

    return 0;
}