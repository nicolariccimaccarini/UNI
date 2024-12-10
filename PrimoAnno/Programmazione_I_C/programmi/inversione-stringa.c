#include <stdio.h>
#include <string.h>
#define DIM 20

int main()
{
    char s[DIM];
    int i;
    int l;

    printf ("Inserisci una parola:\n");
    scanf ("%s", s);

    l=strlen(s);

    for (i=l; i>=0; i--)
    {
        printf ("%c", s[i]);
    }
    printf ("\n");
}