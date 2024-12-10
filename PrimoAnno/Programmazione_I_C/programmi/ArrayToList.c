#include <stdio.h>
#include <stdlib.h>
#define DIM 10

typedef int Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

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

array_to_list(int *pa, int *pl) //*pa -> puntatore array
{
    Lista l;

    return l;
}

int main()
{
    int a[DIM];
    int i;
    Lista l;

    printf ("Inserisci %d numeri: \n", DIM);
    for (i=0; i<DIM; i++)
    {
        scanf ("%d", &a[i]);
    }

    l = array_to_list(a, DIM);
    stamoa (&l);

    return 0;
}