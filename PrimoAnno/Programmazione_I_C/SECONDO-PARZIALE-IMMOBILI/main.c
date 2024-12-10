#include <stdio.h>
#include <stdlib.h>

#include "listaImmobili.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando: argomentoi insufficienti");
        exit(1);
    }

    Lista l;
    Immobile i;
    FILE* pfb;


    pfb = fopen("immobili.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    nuovaLista(&l);

    while (fread(&i, sizeof(Immobile), 1, pfb) == 1)
    {
        aggiorna(&l, i);
    }

    stampa(l);

    return 0;
}