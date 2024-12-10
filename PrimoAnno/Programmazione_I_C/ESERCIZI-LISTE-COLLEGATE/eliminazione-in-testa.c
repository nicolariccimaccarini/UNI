#include <stdio.h>
#include <stdlib.h>
#include "generatoreListe.h"
#include "tipi.h"

void eliminaTesta(Lista *pl)
{
    Nodo* aux = *pl;
    *pl = (*pl)->next;
    free(aux);
}

int main()
{
    Lista l;

    listaNonOrdinata(&l, 3);
    eliminaTesta(&l);

    return 0;    
}