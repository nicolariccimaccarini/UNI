//calcola e stampa il giorno della data in input 
// 0=lunedì, 1=martedì, 2=mercoledì, 3=giovedì, 4=venerdì, 5=sabato, 6=domenica

#include <stdio.h>

int main () {
    int G, M, A;
    int JD, N0, N1, N2, N3;
    int R;

    printf ("--- inserire la data: ---\n");
    printf ("Inserire il giorno della data:\n");
    scanf ("%d", &G);

    printf ("Inserire il mese della data:\n");
    scanf ("%d", &M);

    printf ("Inserire l'anno della data:\n");
    scanf ("%d", &A);

    //calcolo JD

    N0 = (M - 14) / 12;
    N1 = (1461 * (A + 4800 + N0)) / 4;
    N2 = (367 * (M - 2 - 12*N0)) / 12;
    N3 = (3 * (A + 4900 + N0)) / 400;

    JD = N1 + N2 - N3 + G - 32075;

    
    //calcolo il giorno della settimana
    R = (JD % 7);

    printf ("il giorno della setiimana è: %d\n", R);

    printf ("LEGENDA: 0=lunedì, 1=martedì, 2=mercoledì, 3=giovedì, 4=venerdì, 5=sabato, 6=domenica. \n");

    return 0;

}