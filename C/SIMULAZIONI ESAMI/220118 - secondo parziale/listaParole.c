#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaParole.h"

void nuova_lista(Lista *pl)
{
    *pl == NULL; 
}

void insTesta(Lista *pl, Dato d)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));
    aux->dato = d;
    aux->next = *pl;
    *pl = aux;
}

void aggiorna(Lista *pl, char *s)
{
    while (*pl != NULL && strlen((*pl)->dato.parola) >= strlen(s) && 
            strcmp((*pl)->dato.parola, s) != 0)
    {
        pl = &(*pl)->next;
    }

    if (*pl != NULL && strcmp((*pl)->dato.parola, s)==0)
    {
        (*pl)->dato.frequenza++;
    }

    else
    {
        Dato d;
        strcpy(d.parola, s);
        d.frequenza = 1;
        insTesta(pl, d);
    }
}

void eliminaTesta(Lista *pl)
{
    Nodo *aux = *pl;
    *pl = (*pl)->next;
    free(aux);
}

void elimina_parola(Lista *pl, char *s)
{
    while (*pl)
    {
        if (strcmp((*pl)->dato.parola, s) == 0)
        {
            eliminaTesta(pl);
            return;
        }

        else
        {
            pl = &(*pl)->next;
        }
    }
}

void stampa(Lista l)
{
    while (l != NULL)
    {
        if (l->dato.frequenza >= 4)
        {
            printf ("%s %d \n", l->dato.parola, l->dato.frequenza);
        }
        l = l->next;
    }
}