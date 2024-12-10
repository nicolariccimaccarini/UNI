#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DIM 10

typedef struct 
{
    char lettera[2];
    char codificato[4];
} coppia;

int main()
{
    //leggere corrispondenze.txt salvare su un vettore di struct coppia
    //stampare a video il lettore

    FILE *fp;
    coppia lettere[DIM];
    int i;

    fp=fopen("corrispondenze.txt", "r");
    if (fp==NULL)
    {
        printf ("Errore apertura file\n");
        exit(1);
    }

    i=0;
    while (fscanf(fp, "%s %s", lettere[i].lettera, lettere[i].codificato)==2)
    {
        printf ("%c %s\n", lettere[i].lettera[0], lettere[i].codificato);
        i++;
    }

    fclose(fp);

    //leggiamo il file messaggi.txt
    fp=fopen("messaggi.txt", "r");
    if(fp==NULL)
    {
        printf ("Errore apertura file\n");
        exit(2);
    }

    return 0;
}