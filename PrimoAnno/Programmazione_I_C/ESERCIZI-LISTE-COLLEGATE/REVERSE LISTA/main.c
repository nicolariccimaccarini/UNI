// Client ADT Lista 

#include <stdio.h>
#include <stdlib.h>
#include "lista.h"

int main()
{
    Lista l1, l2; //in l1 metteremo la lista "dritta" e in l2 la lista al contrario

    nuovaLista(&l1);
    insTesta(&l1, 1.5);
    insTesta(&l1, 0.7);
    insTesta(&l1, -2.3); 
    // l1 = [-2.3, 0.7, 1.5]

    reverse(l1, &l2);
    // l2 = [1.5, 0.7, -2.3]
    
    printf ("Lista originale:\n");
    stampa(l1);

    printf ("Lista inversa:\n");
    stampa(l2);

    return 0;
}