#include <stdio.h>
#include "contatore.h"

int main()
{
    Contatore c;
    inizializza(&c);
    stampa(c);
    incrementa(&c);
    incrementa(&c);
    stampa(c);

    Contatore c2;
    inizializza(&c2);
    stampa(c2);

    return 0;
}