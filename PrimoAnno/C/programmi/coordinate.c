#include <stdio.h>

int main()
{
    int x, y;

    printf ("Inserisci le coordinate x e y:\n");
    scanf ("%d%d", &x, &y);

    if (x>=0&&y>=0)
    {
        printf ("Ti trovi nel prim quadrante");
    }
    
    if (x<=0&&y>=0)
    {
        printf ("Ti trovi nel secondo quadrante");
    }
    
    if (x<=0&&y<=0) 
    {
        printf ("Ti trovi nel terzo quadrante");
    }
    
    if (x>=0&&y<=0)
    {
        printf ("Ti trovi nel quarto quadrante");
    }
    
}