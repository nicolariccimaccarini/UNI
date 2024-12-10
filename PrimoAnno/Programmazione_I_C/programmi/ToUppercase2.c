// programma che legge in input una riga di codice e e restituisce in input 
//la stessa riga sostituendo però le  lettere minuscole con le maiuscole 
//usando el espressioni condizionali

#include <stdio.h>

int main()
{
    char a;

   while (a!='\n')
   {
    scanf ("%c", &a);
    
    printf ("%c", a>=97 && a<=122 ? a-32 : a);
   }
}