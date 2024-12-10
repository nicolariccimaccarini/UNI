typedef struct 
{
    int giorno;
    int mese;
    float ore_impiegate;
    char descrizione_attivita[30];
} Record;

typedef struct 
{
    int giorno;
    int mese;
    float ore_totali;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista *pl);
void aggiorna(Lista *pl, Record r);
void stampa(Lista l);