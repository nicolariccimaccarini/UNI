#include <stdio.h>

int main()
{
    int numero;
    int i;
    int contatore=0;

    printf("Inserisci un numero positivo");
    scanf("%d",&numero);

    int fattoriale=1;
   
    if(numero > 0)
    {
        for (i=1; i<=numero; i++)
        {
            fattoriale=fattoriale*i;
        }
    }

    printf("Fattoriale: %d\n", fattoriale);

    return 0;
}