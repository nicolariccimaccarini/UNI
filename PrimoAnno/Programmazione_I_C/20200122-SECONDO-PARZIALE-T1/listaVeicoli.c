#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaVeicoli.h"

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
    
    while (*pl != NULL && strcmp((*pl)->dato.targa_veicolo, r.targa_veicolo) != 0)
    {
        pl = &(*pl)->next;
    }

    if (*pl == NULL)
    {
        Dato d;
        strcpy(d.targa_veicolo, r.targa_veicolo);
        d.somma_sosta = 0;
        insTesta(pl, d);
    }

    (*pl)->dato.somma_sosta += r.durata_sosta;
    
}

void stampa(Lista l)
{
    while (l != NULL)
    {
        printf ("%s  %.1f\n", l->dato.targa_veicolo, l->dato.somma_sosta);
        l = l->next;
    }
}