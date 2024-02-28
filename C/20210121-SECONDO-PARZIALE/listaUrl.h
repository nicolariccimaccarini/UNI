typedef struct 
{
    char url[100];
    int ore_visita;
    int minuti_visita;
} Registro;

typedef struct 
{
    char url[100];
    int n_visite; //contatore
    int ora_visita;
    int minuto_visita;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl);
void insTesta(Lista* pl, Dato d);
void aggiorna(Lista* pl, Registro r);
void insOrd(Lista* pl, Dato d);
void stampa(Lista l);