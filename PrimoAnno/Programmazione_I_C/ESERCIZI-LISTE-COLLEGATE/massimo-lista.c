#include <stdio.h>
#include "generatoreListe.h"

typedef int Dato;

typedef struct dato
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

int maxLista(Lista l)
{
    int max = 0;

    while (l)
    {
        if (l->dato > max)
        {
            max = l->dato;
        }

        l->next; 
    }

    return max;
}

void stampa(Lista l)
{
    //ForEach (stampa, l)
    
    while (l)
    {
        printf ("%d\n", l->dato);
        l = l->next;
    }
}

int main()
{
    Lista l;

    listaNonOrdinata(&l, 10);
    stampa(l);
    maxLista(l);

    return 0;
}