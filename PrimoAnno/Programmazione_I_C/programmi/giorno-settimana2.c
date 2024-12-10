//calcola e stampa il giorno della data che l'utente inserisce

#include <stdio.h>

int main () {
    int G, M, A;
    int JD, N0, N1, N2, N3;
    int R;

    printf ("--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n");
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

    if (R = 0) 
    {
        printf ("il giorno della data che hia inserito è lunedì\n");
      } 
      else if (R = 1)
      {
        printf ("il giorno della data che hai inserito è martedì\n");
      } 
      else if (R = 2)
      {
        printf ("il giorno della data che hai inserito è mercoledì\n");
      } 
      else if (R = 3)
      {
        printf ("il giorno della data che hai inserito è giovedì\n");
      } 
      else if (R = 4)
      {
        printf ("il giorno della data che hia inserito è venerdì\n");
      } 
      else if (R = 5)
      {
        printf ("il giorno della data che hia inserito è sabato\n");
      } 
      else if (R = 6)
      {
        printf ("ilg iorno della data che hai inserito è domenica\n");
      }
  
printf ("--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n");
   
    return 0;
}