#include <stdio.h>
#include <string.h>
#define DIM 20

int main()
{
    char s1[DIM];
    char s2[DIM];
    int i, cont=0, j;
    int l1, l2;
    int uguali;

    printf ("Inserisci due frasi:\n");
    scanf ("%s%s", s1, s2);;

    l1=strlen(s1);
    l2=strlen(s2);

    if (l1>=l2)
    {
       j=0;
       uguali=strcmp(s2, s1);

        for (i=0; i<l1; i++)
        {
            if (s2[j]==s1[i])
            {
                cont++;
                j++;
            }
        }

        if (cont==l2)
        {
            printf ("si");
        }
    }

    else
    {
        
    }

    return 0;
}