typedef struct 
{
    int chip;
    char tipo_vaccino;
} Cane;

typedef struct 
{
    int chip;
    int n_vaccini;
    int cimurro;
    int epatite;
    int parvovirosi;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl);
void insTesta(Lista* pl, Dato d);
void aggiorna(Lista* pl, Cane c);
void stampaVacciniMancanti(Lista l);
void stampaVaccinati(Lista l);