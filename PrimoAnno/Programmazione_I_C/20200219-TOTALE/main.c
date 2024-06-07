#include <stdio.h>
#include <stdlib.h>

#include "listaUtenti.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando, argomenti insufficienti\n");
        exit(1);
    }

    Lista l;
    FILE* pfb;
    FILE* pft;

    pfb = fopen("utenti.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    pft = fopen("chiamate.txt", "rt");
    if (pft == NULL)
    {
        printf ("Errore apertura file di testo\n");
        exit(3);
    }

    nuovaLista(&l);
}