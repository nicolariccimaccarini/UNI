#include "lista.h"
#include <stdlib.h>

int lunghezza(Lista l)
{
    int contatore = 0;
    Nodo *p = l;

    while (p != NULL)
    {
        contatore++;
        p = p->next;
    }

    return contatore;
}

void listaNonOrdinata(Lista* pl, int n) 
{
  // gli elementi da inserire nella lista
  int a[] = {6, 2, 3, 2, 4, 7, 0, 2, 5, 1};
  int i;
  // per i che va da 0 a n - 1
  for (i = 0; i < n; i++) 
  {
    // *pl punta a un nuovo nodo
    (*pl) = (Nodo*)malloc(sizeof(Nodo));
    // il cui dato è a[i]
    (*pl)->dato = a[i];
    // e il cui campo next è NULL (cioè *pl ha un solo elemento; eventualmente
    // ne saranno aggiunti altri nelle iterazioni successive)
    (*pl)->next = NULL;
    // assegna a pl l'indirizzo della sua coda
    pl = &(*pl)->next;
  }
}