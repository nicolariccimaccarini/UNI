#include <stdio.h>

int main () {
    int a , b , c , d;
    printf("Inserisci tre numeri interi \n");
    scanf ("%d" , &a );
    scanf ("%d" , &b );
    scanf ("%d" , &c );

    d = a + b + c;

    printf ("la somma dei ter numeri è: %d\n" , d);
}