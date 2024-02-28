#include <stdio.h>
#include <math.h>

int main()
{
    char scelta;
    float lunghezza;
    float massa;
    
    printf ("//digitare 'm' per la massa, 'l' per la lunghezza e 'e' per uscire//\n");
    printf ("Desideri fare una conversione di massa(kg) o di lunghezza(km)?\n");
    scanf ("%c", &scelta);

    while (scelta=='l' || scelta=='m'|| scelta=='e')
    {
        if (scelta=='l')
    {
        printf ("Inserisci la lunghezza che si desidra convertire:\n");
        scanf ("%f", &lunghezza);

        lunghezza=lunghezza/1.609;

        printf ("La lunghezza in Miglia è: %f\n", lunghezza);
    }

    else if (scelta=='m')
    {
        printf ("Inserisci la massa che si desidera convertire:\n");
        scanf ("%f", &massa);

        massa=massa*2.205;

        printf ("La massa in Libbre è:%f\n", massa);
    }

    else 
    {
        return 0;
    }
    }

    return 0;
}