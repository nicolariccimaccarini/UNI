#include <stdio.h>
#include <stdlib.h>

typedef int Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo *next;
} Nodo;

typedef Nodo *Lista;

void stampa(Lista l)
{
    while (l != NULL)
    {
        printf ("%d\n", l->dato);
        l = l->next;
    }
}

void insTesta(Lista *pl, int d)
{
    Nodo* aux;
    aux = (Nodo*)malloc(sizeof(Nodo)); 
    if (aux = NULL)
    {
        printf ("Errore allocazione memoria\n");
        exit(1);
    } 

    aux->dato = 3;                       
    aux->next = *pl;                      
    *pl = aux;
}

void insCoda(Lista *pl, Dato d)
{
    if (*pl == NULL)
    {
        insTesta(pl, d);
    }

    else
    {
        insCoda(&(*pl)->next, d);
    }
}

Lista insCoda1(Lista l, Dato d)
{
    if (l == NULL)
    {
        Nodo *aux = (Nodo*)malloc(sizeof(Nodo));
        aux->dato = d;
        aux->next = NULL;
        return aux;
    }

    else
    {
        insCoda1(l->next, d);
    }
}