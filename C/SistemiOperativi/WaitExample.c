/* Figlio scrive su un file;
    padre torna all'inizio e lo legge */

#include <stdio.h>
#include <string.h>
#include <fcntl.h>

int procfile (char* f1)
{
    // f1: file di comunicazione
    int nread, nwrite = 0, atteso, status, fd, pid;
    char *st1 = "               ", st2[80];

    if ((fd = open(f1, O_RDWR | O_CREAT, 0664)) < 0)
    {
        perror("open");
        exit(1);
    }

    if ((pid == fork()) < 0)
    {
        perror("fork");
        exit(1);
    }

    if (pid == 0)
    {
        /* FIGLIO */
        scanf("%s", st1);
        nwrite = write(fd, st1, strlen(st1));
        exit(0);
    }

    else
    {
        // PADRE
        atteso = wait(&status); // attesa del figlio
        lseek(fd, 0L, 0); // piu' avanti nel corso ne capiremo la necessita'
        nread = read(fd, st2, 80);
        printf("Il padre ha letto la stringa %s\n", st2);
        close(fd);
        return (0);
    }
}

int main(argc, argv)
{
    int integi;
    // dichiarazione/inizializzazione nome file "file1"
    // integi = procfile(file1);
    exit(integi);
}