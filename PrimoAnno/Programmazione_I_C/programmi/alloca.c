#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* allocaStringa(int n)
{
    char *s;
    s = (char*)malloc(n);

    if (s == NULL)
    {
        printf ("Errore allocazione memoria\n");
        exit(1);
    } 

    return s;
}

void libera(char **ps)
{
    free(*ps);
    *ps == NULL;
}

int main()
{
    char *pc = allocaStringa(100);
    strcpy(pc, "Ferrara");

    printf ("%s", pc);

    libera(&pc);

    return 0;
}