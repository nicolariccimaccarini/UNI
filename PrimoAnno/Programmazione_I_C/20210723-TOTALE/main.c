#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaPersone.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando: argomenti insufficienti\n");
        exit(1);
    }

    Lista l;
    Record r;
    FILE* pfb;

    pfb = fopen("eventi.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    nuovaLista(&l);

    printf ("ciao\n");

    while (fread(&r, sizeof(Record), 1, pfb) == 1)
    {
        printf ("porcodio\n");

        aggiorna(&l, r);

        printf ("diocane\n");
    }

    printf ("ciao 2\n");
    
    stampa(l);

    return 0;
}