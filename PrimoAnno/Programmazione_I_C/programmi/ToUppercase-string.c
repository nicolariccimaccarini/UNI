#include <stdio.h>
#define dim 51

char ToUppercase(char a)
{
     if (a>='a' && a<='z')
     {
          return a-('a'-'A');
     }

     else 
     {
          return a;
     }
}

int main()
{
     char str[dim];
     int i;
     
     printf ("Inserisci una parola:\n");
     scanf ("%s", str);

     for (i=0; i<dim; i++)
     {
          printf ("%c", ToUppercase(str[i]));
     }

     printf ("\n");

     return 0;
}