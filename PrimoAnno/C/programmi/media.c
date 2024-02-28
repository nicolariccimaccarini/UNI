#include <stdio.h>

int main()
{
    float a, b;
    
    printf ("Inserisci due numeri reali:\n");
    scanf ("%f%f", &a, &b);

    printf ("La media fra i due numeri è: %f\n", (a+b)/2.0);
}