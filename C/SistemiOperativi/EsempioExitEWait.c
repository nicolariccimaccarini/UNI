#include <stdio.h>

int main()
{
    int pid, status;
    pid = fork();

    if (pid == 0)
    {
        printf("figlio");
    }
    else
    {
        pid = wait(&status);
        printf("terminato processo figlio n.%d", pid);

        if ((char)status == 0)      
        {
            printf("term. volontaria con stato %d", status>>8);
        }
        else
        {
            printf("terminazione volontaria per segnale %d\n", (char)status);
        }
    }

    return 0;
}