#include <stdio.h>
#include "generatoreListe.h"

typedef int Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

int lunghezza(Lista l)
{
    // Reduce (++, 0, l)

    int cont = 0;

    while (l)
    {
        cont++; // aggiornamento accumulatore
        l = l->next;
    }
    
    return cont;
}

int main()
{
    Lista l;

    listaNonOrdinata(&l, 5);
    printf ("%d\n", lunghezza(l));

    return 0;
}