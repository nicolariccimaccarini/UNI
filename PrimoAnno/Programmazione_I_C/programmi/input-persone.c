#include <stdio.h>
#include <stdlib.h>
#define NPERSONE 100

typedef struct 
{
    char cognome[31];
    char nome[31];
    char sesso;
    int anno_nascita;
} Persona;

int main()
{
    int dl;
    Persona persone[NPERSONE];

    FILE *pf;
    FILE *pfb;
    char s[2];
    pf=fopen("people.txt", "rt");

    if (pf==NULL)
    {
        printf ("Errore di apertura del file\n");
        exit(1);
    }
    
    dl=0;
    while (dl<NPERSONE && fscanf(pf, "%s%s%s%d", persone[0].cognome, persone[0].nome, s, &persone[0].anno_nascita))
    {
        persone[0].sesso = s[0];
        dl++;
    }
    fclose(pf);

    pfb = fopen("people.dat", "wb");
    fwrite(persone, sizeof(Persona), dl, pfb);
    fclose(pfb);
    
    return 0;
}