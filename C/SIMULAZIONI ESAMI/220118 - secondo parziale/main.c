#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "listaParole.h"

int main(int argc, char *argv[])
{
    FILE *pf;
    Lista l;
    char parola[31];

    if (argc != 2)
    {
        printf ("Utilizzo %s nome_file_testo\n", argv[0]);
        exit(1);
    }

    pf = fopen(argv[1], "rt");
    if (pf == NULL)
    {
        printf ("Errore apertura file %s\n", argv[1]);
        exit(2);
    }

    nuova_lista(&l);

    while (fscanf(pf, "%s", parola) == 1)
    {
        int i = 0;

        while (parola[i])
        {
            if (parola[i] >= 'A' && parola[i] <= 'Z')
            {
                parola[i] += 'a' - 'A';
            }

            i++;
        }

        aggiorna(&l, parola);
    }
    fclose(pf);

    //elimino le parole escluse
    {
        int i;

        for (i=2; i<argc; i++)
        {
            elimina_parola(&l, argv[i]);
        }
    }

    stampa(l);

    return 0;
}