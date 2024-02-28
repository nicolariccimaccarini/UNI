#include <stdio.h>
#include <string.h>
#define dim 20

int main()
{
    char s[dim];
    int i;

    printf ("Inserisci una parola:\n");
    scanf ("%s", s);

    for (i=0; i<dim; i++)
    {
        if (s[i]=='a')
        {
            s[i]='u';
        }
    }
   
    printf ("%s\n", s);

    return 0;
}