#include <stdio.h>
#include <stdlib.h>

#include "listaGiornate.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando, argomenti insufficienti\n");
        exit(1);
    }

    Lista l;
    Record r;
    FILE* pfb;

    pfb = fopen("ore.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    nuovaLista(&l);

    while (fread(&r, sizeof(Record), 1, pfb) == 1)
    {
        aggiorna(&l, r);
    }

    stampa(l);

    return 0;
}