#include <stdio.h>

int main () {
    int G1, M1, A1;
    int JD1, N0, N1, N2, N3;

    int G2, M2, A2;
    int JD2, C0, C1, C2, C3;
   
    printf ("inserire giorno, mese e anno della prima data\n");
    scanf ("%d%d%d", G1, M1, A1);

    //calcolo del primo Julian Day
    N0 = (M1 - 14) / 12;
    N1 = (1461 * (A1 + 4800 + N0)) / 4;
    N2 = (367 * (M1 - 2 - 12*N0)) / 12;
    N3 = (3 * (A1 + 4900 + N0)) / 400;

    JD1 = N1 + N2 - N3 + G1 - 32075; 

    printf ("inserire giorno, mese e anno della seconda data\n");
    scanf ("%d%d%d", G2, M2, A2);

    //calcolo del secondo Julian Day
    C0 = (M2 - 14) / 12;
    C1 = (1461 * (A2 + 4800 + C0)) / 4;
    C2 = (367 * (M2 - 2 - 12*C0)) / 12;
    C3 = (3 * (A2 + 4900 +C0)) / 400;

    JD2 = N1 + N2 - N3 + G2 - 32075; 

    //differenza tra i due Julian Days
    printf ("la differenza tra i due Julian Days è: %d", JD2 - JD1 );

    return 0;

} 