#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>

#define DIM 256

// contatore richieste
int counter = 0;

void signit_handler(int signal) {
    printf("\nCTRL-C ricevuto:\tSono state servite %d richieste\n", counter);
    exit(0);
}

int main(int argc, char **argv) {

    int fd, pid1, pid2, status;
    char argomento[20], data[9];
    int p1p2[2];
    struct sigaction sa;

    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Errore passaggio argomenti");
        exit(1);
    }
    if ((fd = open(argv[1], O_DIRECTORY)) < 0) {
        perror("dir deve essere una directory");
        exit(2);
    }
    close(fd);
    if (argv[1][0] != '/') {
        fprintf("dir deve essere una directory assoluta");
        exit(3);
    }

    sigemptyset(&sa.sa_mask);
    sa.sa_flags=0;
    sa.sa_handler=sigint_handler;
    sigaction(SIGINT, &sa, NULL);

    printf("iniserci l'argomento che desiederi ricercare: ");
    scanf("%s", argomento);

    while (strcmp(argomento, "fine") != 0) {

        printf("inserisci la data di interesse in formato YYYYMMDD: ");
        scanf("%s", data);

        char pathFile[DIM];
        sprintf(pathFile, "%s%s.txt", argv[1], data);

        if ((fd=open(pathFile, O_RDONLY)) < 0) {
            fprintf(stderr, "Errore nell'apertura del file %s\n", pathFile);
            if (errno == ENOENT) {
                fprintf(stderr, "Il file non esiste\n");
            }
        }
        else {
            close(fd);

            if (pipe(p1p2) < 0) {
                perror("Errore creazioen pipe p1p2");
                exit(5);
            }

            pid1 = fork()
            if (pid1 < 0) {
                perror("Errore fork pid1");
                exit(6);
            }
            if (pid1 == 0) {
                // processo P1
                close(p1p2[1]);

                execlp("grep", "grep", argomento, pathFile, (char *)0);
                perror("Errore execlp");
                exit(8);
            }

            pid2 = fork();
            if (pid2 < 0) {
                perror("Errore fork pid2");
                exit(9);
            }
            if (pid2 == 0) {
                // processo P2
                close(p1p2[1]);
                close(0); // chiudo stdin

                if (dup(p1p2[0]) < 0) {
                    perror("Errore dup p1p2[0]");
                    exit(10);
                }
                close(p1p2[0]);

                execlp("sort", "sort", "-r", "-n", (char *)0)
            }

            counter++;
            close(p1p2[0]);
            close(p1p2[1]);
            wait(&status);
            wait(&status);
        }


        printf("iniserci l'argomento che desiederi ricercare: ");
        scanf("%s", argomento);
    }

    printf("Sono state servite %d richieste\n", counter);
    return 0;
}