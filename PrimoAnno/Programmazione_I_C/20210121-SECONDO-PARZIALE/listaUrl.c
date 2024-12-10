#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaUrl.h"

void nuovaLista(Lista* pl)
{
    *pl = NULL;
}

void insTesta(Lista* pl, Dato d)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));
    aux->dato = d;
    aux->next = *pl;
    *pl = aux;
}

void aggiorna(Lista* pl, Registro r)
{
   while (*pl != NULL && strcmp((*pl)->dato.url, r.url))
   {
        pl = &(*pl)->next;
   }

   if (*pl == NULL)
   {
        Dato d;
        strcpy(d.url, r.url);
        d.n_visite = 0;
        d.minuto_visita = 0;
        d.ora_visita = 0;
        insTesta(pl, d);
   }

   (*pl)->dato.n_visite++;

   if ((r.ore_visita >= (*pl)->dato.ora_visita && r.minuti_visita > (*pl)->dato.minuto_visita) 
        || (r.ore_visita > (*pl)->dato.ora_visita))
   {
        (*pl)->dato.ora_visita = r.ore_visita;
        (*pl)->dato.minuto_visita = r.minuti_visita;
   }
}

Lista* ricerca(Lista* pl, int numero_visite)
{
    while (*pl)
    {
        if ((*pl)->dato.n_visite > numero_visite)
        {
            break;
        }

        pl = &(*pl)->next;
    }

    return pl;
}

void insOrd(Lista* pl, Dato d)
{
    /*
    farae inserimento ordinato in ordine decrescente in base al numero
    di visiste, se il numero di visite sono uguali ordinarlo in base
    alla visita piu' recente
    */

   pl = ricerca(pl, (*pl)->dato.n_visite); 
   insTesta(pl, d);
}

void stampa(Lista l)
{
    
    while (l != NULL)
    {
        if (l->dato.n_visite >= 5)
        {
            printf ("%s  %d  %d:%d\n", l->dato.url, l->dato.n_visite, 
                                       l->dato.ora_visita, l->dato.minuto_visita);
        }

        l = l->next;
    }
}