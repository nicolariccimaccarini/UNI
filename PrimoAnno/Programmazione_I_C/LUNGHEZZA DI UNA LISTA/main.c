#include "lista.h"

int main()
{
    Lista l;
    listaNonOrdinata(&l, 6);

    printf ("Lunghezza: %d\n", lunghezza(l));
    
    return 0;
}