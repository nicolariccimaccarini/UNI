typedef struct 
{
    int chip;
    char tipologia_vaccino[]; //puo contenere 'C', 'E' oppure 'P'
} Dato;

typedef struct nodo
{
    Dato dato;
    struct nodo *next;
} Nodo;

typedef Nodo* Lista;