#include <stdio.h>

int main()
{
    int riga, colonna;
    int finito=0; //flag

    for (riga=1; finito && riga<=10; riga++)
    {
        for (colonna=1; finito && colonna<=10; colonna++)
        {
            printf ("%3d ", riga*colonna);

            if (riga*colonna>50)
            {
                finito=1;
            }
        }

        printf ("\n");
    }
}