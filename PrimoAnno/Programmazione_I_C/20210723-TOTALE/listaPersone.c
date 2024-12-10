#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaPersone.h"

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

void aggiorna(Lista* pl, Record r)
{
    while (*pl != NULL && strcmp((*pl)->dato.cf, r.cf) != 0)
    {
        pl = &(*pl)->next;
    }

    if (*pl == NULL)
    {
        Dato d;
        strcpy(d.cf, r.cf);
        d.tampone = 0;
        d.vaccino = 0;
    }

    (*pl)->dato.tampone++;
    (*pl)->dato.vaccino++;
}

Lista* ricerca(Dato d)
{

}

void insOrd(Lista* pl, Dato d)
{

}

void stampa(Lista l)
{
    while (l != NULL)
    {
        if (l->dato.tampone == 1 && l->dato.vaccino == 1)
        {
            printf ("%s, tampone, vaccino\n", l->dato.cf);
        }

        else if (l->dato.tampone == 1)
        {
            printf ("%s, tampone\n", l->dato.cf);
        }

        else if (l->dato.vaccino == 1)
        {
            printf ("%s, vaccino\n", l->dato.cf);
        }

        l = l->next;
    }
}