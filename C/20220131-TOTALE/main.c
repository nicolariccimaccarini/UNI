#include <stdio.h>
#include <stdlib.h>

#include "listaEsami.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando: argomenti insufficienti\n");
        exit(1);
    }

    Lista l;
    Lista l2;
    Esame e;
    FILE* pfb;

    pfb = fopen("prove.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    nuovaLista(&l);

    while (fread(&e, sizeof(Esame), 1, pfb) == 1)
    {
        aggiorna(&l, e);
    }

    stampa(l);
    printf ("\n");
    nuovaLista(&l2);
    insertionSort(&l, &l2);
    stampa(l);
    
    return 0;
}