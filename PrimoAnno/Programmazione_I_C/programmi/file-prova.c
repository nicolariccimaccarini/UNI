#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
   FILE *pf;
   char s[80];

   pf = fopen("prova.txt", "wt");

   if (pf==NULL)
   {
     printf ("Errore apertura filen\n");
     exit(1);
   }

     do 
     {
          scanf ("%s", s);
          fprintf (pf, "%s\n", s);
     } while (strcmp(s, "FINE"));

   if (fclose(pf)!=0)
   {
     printf ("Errore chiusura file\n");
     exit(2);
   }

   return 0;
}