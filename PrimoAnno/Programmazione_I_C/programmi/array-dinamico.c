#include <stdio.h>
#include <stdlib.h>

int main()
{
    int dim;
    float *v;
    int i;

    printf ("Quanti elementi vuoi nel tuo array?\n");
    scanf ("%d", &dim);

    // riservo un'are di memoria di dimensione pari a dim float
    v = (float*)malloc(sizeof(float) * dim);
    if (v == NULL)
    {
        printf ("Errore allocazione memoria\n");
        exit(1);
    }

    // assegno all'elemento di indice i il valore 1/i
    for (i=0; i<dim; i++)
    {
        v[i] = 1.0 / (i+1);
        // oppure: *(v+1) = 1.0 / (i+1)
    }

    for (i=0; i<dim; i++)
    {
        printf ("%f\n", *(v+1));
    }

    free(v); //deallocazione memoria 
    v = NULL; //faccio capire al programma che v non punta nessuna cella di memoria esistente 

    return 0;
}