#include <stdio.h>
#include <string.h>
#define DIM 50

int main()
{
    char s1[DIM], s2[DIM];
    int i1, i2;
    int potenzialmente_uguale;
    int contenuta;

    printf ("Inserisci due parole: \n");
    scanf ("%s%s", s1, s2);

    i2=0;
    contenuta=0;
    
    while(!contenuta && s2[i2]!='\0')
    {
        potenzialmente_uguale=1;
        i1=0;
        
        while (potenzialmente_uguale && s1[i1]!='\0')
        {
            if (s1[i1]!=s2[i2+i1])
            {
                potenzialmente_uguale=0;
            }

            else 
            {
                i1++;
            }
        }

        if (potenzialmente_uguale)
        {
            contenuta=1;
        }
        else
        {
            i2++;
        }
    }

    if (contenuta)
    {
        printf ("%s è contenuta in %s", s1, s2);
    }
    else 
    {
        printf ("%s non è conttenuta in %s", s1, s2);
    }
    printf ("\n");
    
    return 0;
}