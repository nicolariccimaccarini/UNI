#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "listaCalorie.h"

void lista_vuota(Lista *pl)
{
    pl->n_elementi = 0;
}

int piena(Lista l)
{
    return l.n_elementi == DIM;
}

void insCoda(Lista *pl, Record r)
{
    pl->dati[pl->n_elementi] = r;
    pl->n_elementi++; 
}

float calorie100grammi(Lista l, char nome[])
{
    int i;
    
    for (i=0; i<l.n_elementi; i++)
    {
        if (strcmp(l.dati[0].nome, nome) == 0)
        {
            return l.dati[0].calorie100grammi;
        }
    }

    printf ("Errore %s non trovato\n", nome);
    exit(4);
}