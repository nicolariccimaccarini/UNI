typedef struct 
{
    char targa[8];
    float sosta;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next; 
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista *pl);
void insTesta(Dato d, Lista *pl);
void stampa(Lista l);
int leggiTarga(char targa);