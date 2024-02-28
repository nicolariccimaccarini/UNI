#include <stdio.h>
#include "tipi.h"

Lista* ricerca(Lista *pl, int d) 
{
    while (*pl)
    {
        if ((*pl)->dato > d)
        {
            break;
        }

        pl = &(*pl)->next;
    }

    return pl;
}

void insTesta(Lista* pl, Dato d)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));

    if (aux == NULL) // allocazione memoria non riuscita                            
    {
        exit(100); 
    }

    aux->dato = d;
    aux->next = *pl;
    *pl = aux;
}

void InsOrd(Lista *pl, int d)
{
    pl = ricerca(pl, d);
    insTesta(pl, d);
}
