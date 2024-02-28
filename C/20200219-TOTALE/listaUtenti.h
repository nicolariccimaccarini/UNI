typedef struct 
{
    char numero_teleofono[11];
    char piano_tariffario;
    float credito;
} Utente;

typedef struct 
{
    char numero_telefono[11];
    int secondi_chiamata;
} Chiamata;

typedef struct 
{
    char numero_telefono[11];
    float credito_residuo;
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void nuovaLista(Lista* pl);
void aggiorna(Lista* pl, Dato d);
void stampa(Lista l);