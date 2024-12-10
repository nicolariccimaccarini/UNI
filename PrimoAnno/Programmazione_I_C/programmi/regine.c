#include <stdio.h>
#include <stdlib.h>

int main() 
{
    int x1, y1, x2, y2;

    printf ("inserisci le coordinate delle due regine:\n");
    scanf ("%d%d", &x1, &y1);
    scanf ("%d%d", &x2, &y2);

    if ((x1==x2)||(y1==y2)||(abs(x1-x2)==abs(y1-y2)))
    {
        printf ("Posizione di presa.");
    }
    
    else
    {
        printf ("Posizione sicura.");
    }
}