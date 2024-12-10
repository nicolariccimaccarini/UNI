#include <stdio.h>

int main()
{
    struct data
    {
        int giorno;
        int mese; 
        int anno;
    } oggi;

    printf ("Inserisci la data corrente:\n");
    scanf ("%d%d%d", &oggi.giorno, &oggi.mese, &oggi.anno);

    struct 
    {
        struct data oggi;
        char nome[20];
    } persona;

    printf ("Inserisci il tuo nome:\n");
    scanf ("%s", persona.nome);

    printf ("Inserisci la tua data di nascita:\n");
    scanf ("%d%d%d", &persona.oggi.giorno, &persona.oggi.mese, &persona.oggi.anno);

    if (oggi.giorno==persona.oggi.giorno && oggi.mese==persona.oggi.mese)
    {
        printf ("Buon compleanno %s\n", persona.nome);
    }

    return 0;
} 