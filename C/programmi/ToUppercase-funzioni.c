#include <stdio.h>

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
     char a;
     while (a!='\n')
   {
    printf ("Insersci una frase:\n");
    scanf ("%c", &a);
    
    printf ("%c", ToUppercase(a));
   }
}