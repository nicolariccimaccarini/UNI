typedef struct 
{
    char indirizzo[30];
    int n_vani;
    float distanza_centro;
} Immobile;

typedef struct 
{
    char indirizzo[30];
    int n_vani;
    float distanza_centro;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;   
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl);
void insTesta(Lista* pl, Dato d);
void aggiorna(Lista* pl, Immobile i);
void stampa(Lista l);