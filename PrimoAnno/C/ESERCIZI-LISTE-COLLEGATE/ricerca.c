#include <stdio.h>
#include "tipi.h"

Lista* ricerca(Lista *pl, Dato d)
{
    //finche' la lista ha elementi
    while (*pl)
    {
        // se l'elemento corrente ha la proprieta' desiderta
        if ((*pl)->dato == d)
        {
            // esco
            break;
        }

        pl = &(*pl)->next;
    }

    // qui l e' l'indirizzo della lista desiderata, o di quella vuota
    return pl;
}