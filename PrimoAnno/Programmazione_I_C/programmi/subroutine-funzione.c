#include <stdio.h>

logo() {
  printf("###############\n");
  printf("#   Rubrica   #\n");
  printf("###############\n");
}

main() {
  int scelta;

  logo();
  printf("1. Tizio 335-1234567\n");
  printf("2. Caio 347-1234567\n");
  printf("Scegliere un contatto\n");
  scanf("%d", &scelta);

  logo();
  printf("Chiamata del contatto %d in corso\n", scelta);

  printf("Fine\n");
}