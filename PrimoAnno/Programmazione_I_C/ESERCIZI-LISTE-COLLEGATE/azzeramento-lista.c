#include <stdio.h>
#include "generatoreListe.h"

typedef int Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void azzera(Lista l)
{
    // applico il pattern ForEach (assegna 0 a tutti gli elementi di l)

    while (l)
    {
        // agisco sulla testa di l: l->dato
        l->dato = 0;

        // assegno a l la sua cosa (l->next)
        //passo all'elemento successivo
        l = l->next;
    }
}

void stampa(Lista l)
{
    //ForEach (stampa, l)
    
    while (l)
    {
        printf ("%d\n", l->dato);
        l = l->next;
    }
}

int main()
{
    Lista l;
    
    ListaNonOrdinata(&l, 6);
    azzera(l);
    stampa(l);

    return 0;
} 