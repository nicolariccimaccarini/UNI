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

int main()
{
    Lista l;
    l=NULL;

    stampa(l);

    //inserimento in testa
    insTesta(&l, 3);
    stampa(l);
    insTesta(&l, 5);
    stampa(l);

    return 0;
}