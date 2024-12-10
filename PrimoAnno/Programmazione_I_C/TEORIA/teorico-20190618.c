#include <stdio.h>

typedef struct nodo
{
    int dato;
    struct nodo* next;
} Nodo;

typedef Nodo* Lista;

void inzializza(Lista* pl)
{
    
}

int main()
{
    int i, a[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    Lista l; // l = {1, 8, 5, 7, 9, 2}
    inzializza(&l);

    do 
    {
        for (; l->dato; l = l->next)
        {
            a[l->dato] = l->dato;
        }
    } while (0);

    for (i = 0; i < 10; i++)
    {
        printf ("%d", a[i]);
    }
    printf ("\n");

    return 0;
}