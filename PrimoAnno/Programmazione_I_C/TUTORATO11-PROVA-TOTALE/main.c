#include <stdio.h>  
#include <stdlib.h>
#include <string.h>

#include "listaPazienti.h"

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        printf ("Elemento mancante linea di comando\n");
        exit(-2);
    }
    
    Lista l;
    Rilevazione v;
    FILE* pfb;

    nuovaLista(&l);

    pfb = fopen("rilevazioni.dat", "rb");
    if (pfb == NULL)
    {
        printf ("Errore apertura file binario\n");
        exit(-1);
    }

    while (fread(&v, sizeof(Rilevazione), 1, pfb) == 1)
    {
        aggiorna(&l, v);
    }

    printf ("\nPazienti prima di eliminaSani:\n");
    stampa(l);

    printf ("\n\nPazienti dopo eliminaSani:\n");
    eliminaSani(&l);
    stampa(l);
    printf ("\n");
    
    return 0;
}