#include <stdio.h>
#include <string.h>
#define DIM 30

int main()
{
    char s[DIM]; //stringa iniziale
    char s2[DIM]; //stringa dove ci andranno le prime consonanti e "ay"
    char s3[]="ay";
    char voc[DIM];
    int cont; //contatore che ci servirà per capire con quante consonanti inizia la stringa
    char finale[DIM]; //stringa finale
    int i, j;

    printf ("Inserisci una parola:\n");
    scanf ("%s", s);
    int l=strlen(s);

    if (s[1]!=('a'&'e'&'i'&'o'&'u')) //quando la parola inizia con una consonante
    {
        cont=0;
        for (i=0; i!='\0'; i++)
        {
            while (s[i]!=('a'&'e'&'i'&'o'&'u')) //finché s[i] è una consonante
            {
                s2[i]=s[i]; //copio la consonante dalla stringa originale in un'altra stringa
                cont++; 
            }  
        }
        strcat(s2, s3);

        j=0;
        for (i=cont; i!='\0'; i++)
        {
            voc[j]=s[i]; //copio le vocali nella stringa voc
            j++;
        }

        printf ("%s\n", strcat(voc, s2));
    }

    else if (s[1]==('a'|'e'|'i'|'o'|'u')) //quando la parola inizia con una vocale
    {
        printf ("%s\n", strcat(s, s3));
    }

    return 0;
}