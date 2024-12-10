#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "listaVeicoli.h"

void nuovaLista(Lista *pl)
{
    (*pl) == NULL;
}

void insTesta(Dato d, Lista *pl)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));

    if (aux == NULL)
    {
        exit(100);
    }

    aux->dato.sosta = d.sosta;
    strcpy(aux->dato.targa, d.targa);
    aux->next = *pl;
}

void stampa(Lista l)
{
    while (l)
    {
        printf ("%s: %f\n", l->dato.targa, l->dato.sosta);
        l = l->next;
    }
}

int leggiTarga(char targa)
{
    int cont = 0;


    return 0;
}