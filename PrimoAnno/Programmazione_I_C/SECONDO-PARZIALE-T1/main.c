#include <stdio.h>
#include <stdlib.h>
#include "listaVeicoli.h"

int main(int argc, char* argv[])
{
    FILE* pb;
    Lista* l;
    Dato d;

    int cont = 0;

    pb = fopen(argv[1], "rb");
    if (pb == NULL)
    {
        printf ("Errore apertura file\n");
        exit(1);
    }

    nuovaLista(&l);

    while (fread(&d, sizeof(Dato), l, pb) == 1)
    {
        insTesta(d, &l);
    }

    while (l)
    {
        leggiTarga(l->dato.targa);
        
    }

    stampa(&l);

    return 0;
}