#include <stdio.h>
#include <string.h>

int main()
{
    struct persona
    {
        char nome[20];
        char cognome[30];
        float altezza;
        int eta;
    } p1;

    int i;

    struct persona p2;

    struct persona persone[100];

    p1.eta=25;
    scanf ("%f", &p1.altezza);
    strcpy(p1.cognome, "Jacobs");

    for (i=0; i<100; i++)
    {
        persone[i].eta=30;
    }

    return 0;
}