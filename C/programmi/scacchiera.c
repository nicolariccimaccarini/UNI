#include <stdio.h>

int main ()
{
    int riga, colonna;

    printf ("Inserisci i valori di riga e colonna:\n");
    scanf ("%d%d", &riga, &colonna);

    if ((riga<=0|| riga>8)||(colonna<=0||colonna>8))
    {
        printf ("Valore riga o colonna non valido! Riprova:\n");
        scanf ("%d%d", &riga, &colonna);
    }
    
    if ((riga%2==0&&colonna%2==0)||(riga%2!=0&&colonna!=0))
    {
        printf ("Va pedina si trova su una cella bianca");
    }
    else 
    { 
        printf ("La pedina si trova su una casella nera");
    }
}