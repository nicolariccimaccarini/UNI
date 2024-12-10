#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaImmobili.h"

void nuovaLista(Lista* pl)
{
    *pl == NULL;
}

void insTesta(Lista* pl, Dato d)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));
    aux->dato = d;
    aux->next = *pl;
    *pl = aux;
}

Lista* ricerca(Lista *pl, Dato d)
{
    while (*pl)
    {
        if ((*pl)->dato.distanza_centro > d.distanza_centro)
        {
            break;
        }

        pl = &(*pl)->next;
    }

    return pl;
}

void insOrd(Lista* pl, Dato d)
{
    pl = ricerca(pl, d);
    insTesta(pl, d);
}

void aggiorna(Lista* pl, Immobile i)
{
    Dato d;
    strcpy(d.indirizzo, i.indirizzo);
    d.distanza_centro = i.distanza_centro;
    d.n_vani = i.n_vani;
    insOrd(pl, d);
}

void stampa(Lista l)
{
    while (l != NULL)
    {
        printf ("%s  %d  %.3f\n", l->dato.indirizzo, l->dato.n_vani, l->dato.distanza_centro);
        l = l->next;
    }
}