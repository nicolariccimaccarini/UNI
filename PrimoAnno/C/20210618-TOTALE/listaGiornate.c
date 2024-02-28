#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaGiornate.h"

void nuovaLista(Lista *pl)
{
    *pl = NULL;
}

void insTesta(Lista *pl, Dato d)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));
    aux->dato = d;
    aux->next = *pl;
    *pl = aux;
}

void aggiorna(Lista* pl, Record r)
{
    while (*pl != NULL || ((*pl)->dato.giorno == r.giorno && (*pl)->dato.mese == r.mese));
    {
        pl = &(*pl)->next;
    }
    
    if (*pl == NULL)
    {
        Dato d;
        d.giorno = r.giorno;
        d.mese = r.mese;
        d.ore_totali = 0.0;
        insTesta(pl, d);
    }

    (*pl)->dato.ore_totali += r.ore_impiegate;
}

void stampa(Lista l)
{
    while(l != NULL)
    {
        printf ("%d/%d  %1.f", l->dato.giorno, l->dato.mese, l->dato.ore_totali);
        l = l->next;
    }
}