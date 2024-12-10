#include <stdio.h>
#include <stdlib.h>
#include "generatoreListe.h"

typedef int Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo *next;
} Nodo;

typedef Nodo *Lista;

int lunghezza(Lista l)
{
    // Reduce (++, 0, l)

    int cont = 0;

    while (l)
    {
        cont++; // aggiornamento accumulatore
        l = l->next;
    }
    
    return cont;
}

int listToArray (Lista l)
{
    int *p, i;
    int n = lunghezza(l);

    p = (int*)malloc(sizeof(int) * n);
    
    i = 0;
    // forEach (assegni elementio a i++esimo dell'elemento)
    while (l)
    {
        p[i] = l->dato;
        i++;
        l = l->next;
    }

    return p;
}

int main()
{
    Lista l;
    int *p, i, n;

    listaNonOrdinata(&l, 6);

    p = listToArray(l);
    n = lunghezza(l);

    // stampa dell'array
    for (i=0; i<n; i++)
    {
        printf ("%d\n", p[i]);
    }

    return 0;
}