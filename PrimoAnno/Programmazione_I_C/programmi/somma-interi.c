#include <stdio.h>

int main ()
{
  int numero;
  int i;
  int somma = 0;
  
  printf("Digita un numero intero positivo:\n");
  scanf("%d", &numero);

if(numero > 0)
{
    int i = 1;
    while(i < numero)
    {
        somma = somma + i;
        i = i + 1;
    }
  printf ("La somma dei primi %d", numero);
  printf (" numeri interi è:% d\n", somma);
}
  
  else 
  {
    return 0;
  }

}