#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaUrl.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando, elementi mancanti\n");
        exit(1);
    }

    Lista l;
    Registro r;
    FILE* pfb;

    pfb = fopen("visite.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Erroe apertura file binario\n");
        exit(2);
    }

    nuovaLista(&l);

    while (fread(&r, sizeof(Registro), 1, pfb) == 1)
    {
        aggiorna(&l, r);
    }

    stampa(l);

    return 0;
}