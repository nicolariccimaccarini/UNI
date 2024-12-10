#include <stdio.h>
#include <stdlib.h>

#include "listaCani.h"

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

Lista* ricerca(Lista* pl, Dato d)
{
    while (*pl)
    {
        if ((*pl)->dato.n_vaccini < d.n_vaccini)
        {
            break;
        }

        else if ((*pl)->dato.n_vaccini == d.n_vaccini && (*pl)->dato.chip > d.chip)
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

void aggiorna(Lista* pl, Cane c)
{
    while (*pl != NULL && (*pl)->dato.chip != c.chip)
    {
        pl = &(*pl)->next;
    }

    if (*pl == NULL)
    {
        Dato d;
        d.chip = c.chip;
        d.n_vaccini = 0;
        d.cimurro = 0;
        d.epatite = 0;
        d.parvovirosi = 0;
        insTesta(pl, d);
    }

    switch (c.tipo_vaccino)
    {
        case 'C':
            (*pl)->dato.cimurro = 1;
            (*pl)->dato.n_vaccini++;
            break;
        
        case 'E':
            (*pl)->dato.epatite = 1;
            (*pl)->dato.n_vaccini++;
            break;

        case 'P':
            (*pl)->dato.parvovirosi = 1;
            (*pl)->dato.n_vaccini++;
            break;
    }
}

void stampaVacciniMancanti(Lista l)
{
    while (l != NULL)
    {
        printf ("%d:  ", l->dato.chip);

        if (!(l->dato.cimurro))
        {
            printf ("cimurro ");
        }

        if (!(l->dato.epatite))
        {
            printf ("epatite ");
        }

        if (!(l->dato.parvovirosi))
        {
            printf ("parvovirosi ");
        }

        printf ("\n");
        l = l->next;
    }
}

void stampaVaccinati(Lista l)
{
    FILE* pft;
    Lista lOrd;

    pft = fopen("vaccinazioni.txt", "wt");
    if (pft == NULL)
    {
        printf ("Errore apertura file di testo\n");
        exit(3);
    }

    nuovaLista(&lOrd);
    
    while (l)
    {
        insOrd(&lOrd, l->dato);
        l = l->next;
    }

    while (lOrd)
    {
        fprintf (pft, "%d (%d v.)\n", lOrd->dato.chip, lOrd->dato.n_vaccini);
        lOrd = lOrd->next;
    }

    fclose(pft);
}