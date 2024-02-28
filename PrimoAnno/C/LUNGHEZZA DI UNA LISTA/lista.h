typedef int Dato;

typedef struct 
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void listaNonOrdinata(Lista *pl, int n);
int lunghezza(Lista l);