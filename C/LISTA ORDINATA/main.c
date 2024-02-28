// programma principale

#include <stdio.h>
#include <stdlib.h>

#include "lista.h"

int main()
{
    Lista l;
    int numero;

    nuovaLista(&l);

    printf ("Inserisci numeri interi (negativo o zero termina)\n");
    
    do
    {
        scanf ("%d", &numero);

        if (numero <= 0)
        {
            break;
        }
        insOrd(&l, numero);
    } while (lunghezza(l) < 50); 

    stampa(l);
    
    return 0;
} 