#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef int Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl)
{
    *pl = NULL;
}

void insTesta(Lista *pl, int d)
{
    Nodo *aux;
    aux = (Nodo*)malloc(sizeof(Nodo));
    aux->dato;
    aux->next = *pl;
    *pl = aux;
}

void listaCasuale(Lista *pl, int numeroElemento)
{
    int i;
    nuovaLista(pl);
    
    for(i=0; i<numeroElemento; i++)
    {
        insTesta(pl, 1 + rand()%9);
    }
}
Lista* ricerca(Lista *pl) //restituisce l'indirizzo della lista in testa a cui fare riferimento
{
    while (*pl != NULL)
    {
        //assegno a pl l'indirizzo della coda di *pl
        pl = &(*pl)->next; 
    }

    return pl;
}

int main()
{
    Lista l;
    Lista *pl;    
    listaCasuale(&l, 3);

    //ricerca della lista in testa a cui inserire 5
    pl = ricerca(&l);

    //inerimento di 5 in testa alla lista trovata
    insTesta(pl, 5);
    
    return 0;
}