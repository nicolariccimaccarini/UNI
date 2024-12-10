#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaEsami.h"

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

Lista* ricerca(Lista* pl, Dato d)
{
    while (*pl)
    {
        if ((*pl)->dato.somma_punteggi < d.somma_punteggi)
        {
            break;
        }

        else if ((*pl)->dato.somma_punteggi == d.somma_punteggi && 
                 (*pl)->dato.matricola < d.matricola)
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

void elimTesta(Lista* pl)
{
    Nodo* aux = *pl;
    *pl = (*pl)->next;
    free(aux);
}

void insertionSort(Lista* pl, Lista* pl2)
{
    while (*pl != NULL)
    {
        printf ("ciao\n");
        insOrd(pl2, (*pl)->dato);
        elimTesta(pl);
    }

    *pl = *pl2;
}

void aggiorna(Lista* pl, Esame e)
{
    while (*pl != NULL && (*pl)->dato.matricola != e.matricola)
    {
        pl = &(*pl)->next;
    }

    if (*pl == NULL)
    {
        Dato d;
        d.matricola = e.matricola;
        d.punteggio_pratico = 0;
        d.punteggio_teorico = 0;
        d.somma_punteggi = 0;
        insTesta(pl, d);
    }

    switch (e.tipo_prova)
    {
        case 'T':
            (*pl)->dato.punteggio_teorico = e.punteggio;
            break;

        case 'P':
            (*pl)->dato.punteggio_pratico = e.punteggio;
            break;
    }

    (*pl)->dato.somma_punteggi = (*pl)->dato.punteggio_pratico + (*pl)->dato.punteggio_teorico;
}

void stampa(Lista l)
{
    while (l != NULL)
    {
        if (l->dato.punteggio_pratico >= 12 && l->dato.punteggio_teorico >= 6)
        {
            if (l->dato.somma_punteggi > 30)
            {
                printf ("%d  30 e lode\n", l->dato.matricola);
                l = l->next;
            }
            
            else 
            {
                printf ("%d  %d\n", l->dato.matricola, l->dato.somma_punteggi);
                l = l->next;
            }

        }

        else if (l->dato.somma_punteggi < 18 || 
                (l->dato.punteggio_pratico < 12 || l->dato.punteggio_teorico < 6))
        {
            printf ("%d  non superato\n", l->dato.matricola);
            l = l->next;
        }

        else 
        {
            l = l->next;
        }
    }
}