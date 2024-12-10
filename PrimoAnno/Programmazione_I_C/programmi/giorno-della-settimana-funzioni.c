#include <stdio.h>

int giorno_giuliano(int g, int m, int a) 
{
  int N0, N1, N2, N3;
  
  N0 = (m - 14) / 12;
  N1 = 1461 * (a + 4800 + N0) / 4;
  N2 = 367 * (m - 2 - 12 * N0) / 12;
  N3 = 3 * (a + 4900 + N0) / 400;
  
  return N1 + N2 - N3 + g - 32075;
}

int giorno(int g, int m, int a)
{
     switch (giorno_giuliano(g, m, a)%7)
     {
          case 0: printf ("L\n"); break;
          case 1: printf ("Ma\n"); break; //Ma=martedì
          case 2: printf ("Me\n"); break; //Me=mercoledì
          case 3: printf ("G\n"); break; 
          case 4: printf ("V\n"); break;
          case 5: printf ("S\n"); break;
          defautlt: printf ("D\n"); break;
     }
     
}

int main(void)
{
     int g, m, a;

     printf ("Inserisci la data di cui vuoi sapere il giorno (giorno, mese e anno):\n");
     scanf ("%d%d%d", &g, &m, &a);

     giorno(g, m, a);

     return 0;
}