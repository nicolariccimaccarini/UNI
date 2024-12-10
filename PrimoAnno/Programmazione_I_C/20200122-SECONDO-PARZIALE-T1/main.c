#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaVeicoli.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Errore linea di comando: argomenti insufficienti\n");
        exit(1);
    }

    Lista l;
    Registro r;
    Targhe t;
    FILE* pfb;
    FILE* pft;

    pfb = fopen("accessi.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(2);
    }

    pft = fopen("ultimi3.txt", "a+");
    if (pft == NULL)
    {
        printf ("Errore apertura file di testo\n");
        exit(3);
    }

    nuovaLista(&l);

    while (fread(&r, sizeof(Registro), 1, pfb) == 1)
    {
        aggiorna(&l, r); 
    }

    stampa(l);

    while (fread(&t, sizeof(Targhe), 1, pfb) == 1)
    {
        if (feof(pfb) != 0)
        {
            //abbiamo raggiunto la fine del file 
        }
    }

    return 0;
}