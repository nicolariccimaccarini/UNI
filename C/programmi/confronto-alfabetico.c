#include <stdio.h>
#define DIM 30

int main() 
{
    char s1[DIM], s2[DIM];
    char dif;
    int i;

    printf ("Inserisci due parole:\n");
    scanf ("%s%s", s1, s2);

    i=0;
    while (dif==0 && (s1[i]!='\0' || s2[i]!='\n'))
    {
        dif = s1[i]-s2[i];
        i++;
    }

    if (dif<0)
    {
        printf ("%s precede %s\n", s1, s2);
    }

    else if (dif>0)
    {
        printf ("%s precede %s\n", s2, s1);
    }

    else 
    {
        printf ("%s e %s sono uguali\n", s1, s2);
    }
}