#include <stdio.h>

int main()
{
    int numero;
    int i;
    int contatore=0;

    printf("Inserisci un numero positivo: \n");
    scanf("%d",&numero);

    int fattoriale=1;
   
    if(numero > 0)
    {
        for (i=1; i<=numero; i++)
        {
            fattoriale=fattoriale*i;
        }

        while (fattoriale%10==0)
        {
            contatore++;
            fattoriale=fattoriale/10;
        }
    }

    printf ("%d\n", contatore);

    return 0;
}