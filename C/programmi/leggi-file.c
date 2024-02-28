#include <stdio.h>
#include <stdlib.h>

int main()
{
    FILE *pf;
    char nome[80];
    char c;

    printf ("Inserisci il nome del file:\n");
    scanf ("%s", nome);
    pf = fopen("nome", "rt");

    if (pf==NULL)
    {
        printf ("Errore di apertura del file\n");
        exit(1);
    }

    do 
    {
        fscanf (pf, "%c", &c);
        printf ("%c\n", c);
    } while (feof(pf)==0);

    if (fclose(pf)!=0)
    {
        printf ("Errore di chiusura del file\n");
        exit(2);
    }
}