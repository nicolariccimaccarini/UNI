#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct 
{
    char nome[20];
    char indirizzo[30];
    int numero[15];
} Dati;

int main()
{
    FILE* ft;
    Dati d;
    char *s;

    ft = fopen("rubrica.txt", "rt");
    if (ft == NULL)
    {
        printf ("Errore apertura file\n");
        exit(-1);
    }

    printf ("Inserisci nome da cercare");
    scanf ("%s", d.nome);

    while (feof(ft) == 0)
    {
        fgets (ft, 50, s);
        if (strcmp(d.nome, s) == 1)
        {
            fprintf(ft, s);
        }
    }

    fclose(ft);
    return 0;
}