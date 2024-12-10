#include <stdio.h>

int main() {
  int numero, n, i, j, c;
  int q, esp;


  printf("Inserisci un numero naturale\n");
  scanf("%d", &numero);


  q = numero;

  for (n = 2; n <= numero; n++) {
   
    if (numero % n == 0) {
 
      c = 0;


      for (i = 2; c == 0 && i * i <= n; i++)

        if (n % i == 0) {

          c++;

        }

      if (c == 0) {
        esp = 0;
        
        while (q % n == 0) {

          q = q / n;
          esp++;

        }
        printf("%d", n);

        if (esp > 1)

          printf("^%d", esp);

        if (q > 1)
        printf(" x ");
      }
    }

  }
  
  printf("\n");

}