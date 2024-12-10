#include <stdio.h>
#include <string.h>
#define DIM 100

typedef struct 
{
    char a[DIM];
    char b[DIM];
}Funzione;

Funzione leggi()
{
    char a[DIM], b[DIM];
    
    Funzione d;
    scanf ("%s%s", a, b);

    d.a[DIM]=a;
    d.b[DIM]=b;

    return d;
}

Funzione rimuoviDuplicati(Funzione d)
{
    Funzione rd;
    
    int i;
    int cont=0;

    int j=0;
    while (j!='\0')
    {
        for (i=0; i!='\0'; i++)
        {
            if (rd.b[j]=rd.a[i])
            {
                strcpy(rd.a[i], rd.a[i+1]);
                cont++;
            }

            printf ("%s", rd.a[i]);
        }

        j++;
    }
    printf ("\n");
    printf ("%d\n", cont);
}

int main()
{
    Funzione a[DIM], b[DIM], c[DIM];

    a[DIM]=leggi();
    b[DIM]=leggi();

    c[DIM]=rimuoviDuplicati(a[DIM], b[DIM]);
}