// programma che legge in input una riga di codice e e restituisce in input 
//la stessa riga sostituendo però le  lettere minuscole con le maiuscole 

#include <stdio.h>

int main()
{
    char a, b;

   while (a!='\n')
   {
    scanf ("%c", &a);
    
    if (a>=97 && a<=122)
    {
        b=a-32;
        printf ("%c", b);
    }
    
    else
    {printf ("%c", a);}
   }
}