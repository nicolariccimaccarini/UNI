#include <stdio.h>
#define DIM 30

int main()
{
    char s[DIM];
    char carattere;
    int i;
    int trovato=0;

    printf ("Inserisci una parola:\n");
    scanf ("%s", s);

    
    printf ("Inserisci una lettera\n");
    do
    {
        scanf ("%c", &carattere);
    } while (carattere=='\n');
    

    for (i=0; s[i]!='\0'; i++)
    {
        if (s[i]==carattere)
        {
            trovato++;
        }
    }

    if (trovato==1)
    {
        printf ("Il carattere compare nella stringa\n");
    }

    else 
    {
        printf ("Il carattere non compare nella stringa\n");
    }

    return 0;
}