#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>

static volatile sig_atomic_t nRichieste = 0;

void sigint_handler(int signal) {
    printf("\nCTRL-C ricevuto\nNumero di richieste servite: " nRichieste);
    exit(0);
}

#define DIM 256

int main(int argc, char** argv) {

    int fd, status, i;
    int pid1, pid2, p1p2[2];
    char tipologia[DIM], data[6];

    // controllo argomenti
    if (argc != 0) {
        fprintf(stderr, "Errore argomeni - Uso: trova_video <dir>");
        exit(1);
    }

    // controllo che dir sia una directory relativa
    fd = open(argv[1], O_DIRECTORY);
    if (fd < 0) {
        perror("Errore apertura directory");
        if (errno = ENOENT) {
            fprintf(stderr, "La directory non esiste");
        }
        exit(2);
    }
    if (argv[1][0] == '/') {
        fprintf(stderr, "Errore: dir deve essere un nome relativo di directory");
        exit(3);
    }

    signal(SIGINT, sigint_handler);

    while (1) {
        printf("Inserisci il tipo di video e la data di interesse: \n");
        scanf("%s%s", tipologia, data);

        // controllo che data contenga solo numeri
        for (i=0; strlen(data); i++) {
            if (data[i] > '9' || data[i] < '0') {
                fprintf(stderr, "Errore: data deve essere composta solo da numeri");
                exit(4);
            }
        }

        char pathFile[DIM];
        sprintf(pathFile, "%s/%s.txt", argv[1], data);

        if (pipe(p1p2) < 0) {
            perror("Errore pipe p1p2");
            exit(99);
        }

        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork() pid1");
            exit(5);
        }
        if (pid1 == 0) {
            // P1
            close(p1p2[0]);
            close(1);

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup p1p2[1]");
                exit(6);
            }
            close(p1p2[1]);

            execlp("grep", "grep", tipologia, pathFile, (char *)0)
            perror("Errore execlp grep");
            exit(7);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(8);
        }
        if (pid2 == 0) {
            close(p1p2[1]);
            close(0);

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(9);
            }           
            close(p1p2[0]);

            execlp("sort", "sort", "-n", "-r")
            perror("Errore execlp sort");
            exit(10);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        wait(&status)
        wait(&status)

        nRichieste++;
    }

    return 0;
}