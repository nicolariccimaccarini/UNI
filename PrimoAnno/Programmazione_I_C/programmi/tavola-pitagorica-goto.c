#include <stdio.h>

int main()
{
    int riga, colonna;

    for (riga=1; riga<=10; riga++)
    {
        for (colonna=1; colonna<=10; colonna++)
        {
            printf ("%3d ", riga*colonna);

            if (riga*colonna>50)
            {
                goto finito;
            }
        }

        printf ("\n");
    }

    finito:
    printf ("Finito!\n");
}