#include <stdio.h>
#include <stdlib.h>
#include "tipi.h"
#include "generatoreListe.h"

void insTesta(Lista* pl, Dato d)
{
    Nodo* aux = (Nodo*)malloc(sizeof(Nodo));

    if (aux == NULL) // allocazione memoria non riuscita                            
    {
        exit(100); 
    }

    aux->dato = d;
    aux->next = *pl;
    *pl = aux;
}

int main(void)
{
    Lista l;

    listaNonOrdinata(&l, 3); // creo una lista non ordinata
    insTesta(&l, 5); // inserisco in testa alla lista un nodo con il valore indicato

    return 0;
}