typedef struct 
{
    int matricola;
    int punteggio;
    char tipo_prova;
} Esame;

typedef struct 
{
    int matricola;
    int punteggio_teorico;
    int punteggio_pratico;
    int somma_punteggi;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl);
void aggiorna(Lista* pl, Esame e);
void insTesta(Lista* pl, Dato d);
void elimTesta(Lista* pl);
void insOrd(Lista* pl, Dato d);
void insertionSort(Lista* pl1, Lista* pl2);
void stampa(Lista l);