typedef struct 
{
    char targa_veicolo[8];
    float durata_sosta;
} Registro;

typedef struct
{
    char targa1[8];
    char targa2[8];
    char targa3[8];
} Targhe;

typedef struct 
{
    char targa_veicolo[8];
    float somma_sosta;
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
void stampa(Lista l);