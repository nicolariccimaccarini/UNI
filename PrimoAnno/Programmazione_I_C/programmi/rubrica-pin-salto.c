#include <stdio.h>

int main()
{
    int pin, scelta;

    printf ("Inserisci il pin:\n");
    scanf ("%d", &pin);

    if (pin != 44122)
    {
        goto fine;
    }

    else 
    {
        printf ( " Accesso consentito \n");
        printf ( " 1. Tizio 335-1234567 \n");
        printf ( " 2. Caio 347-1234567 \n");
        printf ( " Scegliere un contatto \n");
        scanf ( " %d " , & scelta ); 
        printf ( " Chiamata al contatto %d\n" , scelta ); 
    }

fine:
    printf ("PIN ERRATO\n");

}