#include <stdio.h>

int main()
{
    int G1, M1, A1;
    int JD1, N0, N1, N2, N3;

    int G2, M2, A2;
    int JD2, C0, C1, C2, C3;

    int AB;

    printf ("inserire il primo giorno e il primo mese dell'anno scelto (es. 1/1/2022)\n");
    scanf ("%d%d%d", G1, M1, A1);

    //calcolo del primo Julian Day
    N0 = (M1 - 14) / 12;
    N1 = (1461 * (A1 + 4800 + N0)) / 4;
    N2 = (367 * (M1 - 2 - 12*N0)) / 12;
    N3 = (3 * (A1 + 4900 + N0)) / 400;

    JD1 = N1 + N2 - N3 + G1 - 32075; 

    printf ("inserire l'ultimo giorno e l'ultimo mese dell'anno scelto (es. 31/12/2022)\n");
    scanf ("%d%d%d", G2, M2, A2);

    //calcolo del secondo Julian Day
    C0 = (M2 - 14) / 12;
    C1 = (1461 * (A2 + 4800 + C0)) / 4;
    C2 = (367 * (M2 - 2 - 12*C0)) / 12;
    C3 = (3 * (A2 + 4900 +C0)) / 400;

    JD2 = N1 + N2 - N3 + G2 - 32075; 

    AB = JD2 - JD1;

    if (AB == 365)
    {
      printf ("L'anno è bisestile\n");
    }
    
    else if (AB == 364)
    {
      printf ("L'anno non è bisestile\n");
    }
    
    else 
    {
      printf("ERRORE! RINSERIRE I DATI\n");
      return 0;
    }
}