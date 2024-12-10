#include <stdio.h>

main () {
    int lato;
    int area;
    int perimetro;
    printf ("inserisci il lato\n");
    scanf ("%d", &lato);

    perimetro = 4 * lato;
    area = lato * lato;
    
    printf ("perimetro = %d\n", perimetro);
    printf ("area = %d\n", area);

}