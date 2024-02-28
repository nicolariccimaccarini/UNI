typedef struct 
{
    int giorno;
    int mese;
    int anno;
    char cf[17];
    char tipo_evento[2];
} Record;

typedef struct 
{
    char cf[17];
    int vaccino;
    int tampone;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl);
void insTesta(Lista* pl, Dato d);
void aggiorna(Lista* pl, Record r);
void insOrd(Lista* pl, Dato d);
void stampa(Lista l);