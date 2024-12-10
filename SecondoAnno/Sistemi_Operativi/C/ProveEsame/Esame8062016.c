#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define DIM 256

static volatile sig_atomic_t richieste = 0;

void sigint_handler(int signal) {
    printf("\nCTRL-C ricevuta\nStampo il numero di richieste servite: %d", richieste);
    exit(0);
}

int main(int argc, char** argv) {

    int fd, status;
    int pid1, pid2, p1p2[2];
    char argomento[DIM], data[8];

    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Errore argomenti - Uso: trova_news <dir>");
        exit(1);
    }

    // controllo che dir sia una directory assoluta
    if (argv[1][0] != '/') {
        fprintf(stderr, "Errore: %s deve essere un nome assoluto di directory", argv[1]);
        exit(2);
    }

    fd=open(argv[1], O_DIRECTORY);
    if (fd < 0)
    {
        perror("Errore nell'apertura di dir");
        exit(-3);
    }
    close(fd);

    signal(SIGINT, sigint_handler);

    printf("Inserisci l'argomento di interesse:\n");
    scanf("%s", argomento);

    while (strcmp(argomento, "fine") != 0) {
        printf("Inserisci la data di interesse:\n");
        scanf("%s", data);

        char nomeFile[DIM];
        sprintf(nomeFile, "%s/%s.txt", argv[1], data);

        fd = open(nomeFile, O_RDONLY);
        if (fd < 0) {
            fprintf(stderr, "Errore apertura file %s\n", nomeFile);
            if (errno == ENOENT) {
                fprintf("Il file non esiste\n");
            }
            exit(99);
        }
        close(fd);

        if (pipe(p1p2) < 0) {
            perror("Errore creazione pipe p1p2");
            exit(3);
        }

        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork() pid1");
            exit(4);
        }
        if (pid == 0) {
            close(p1p2[0]);
            close(stdout);

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup p1p2[1]");
                exit(5);
            }
            close(p1p2[1]);

            execlp("grep", "grep", argomento, nomeFile, (char *)0);
            perror("Errore grep");
            exit(6);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(7);
        }
        if (pid2 == 0) {
            close(p1p2[1]);
            close(stdin);

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(8);
            }
            close(p1p2[0]);

            execlp("sort", "sort", "-n", "-r", (char *)0);
            perror("Errore sort");
            exit(9);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        wait(&status);
        wait(&status);

        richieste++;

        printf("\n---------\nInserire nuovo argomento:\t(fine per terminare)\n");
        scanf("%s", argomento);
    }

    printf("Il numero di richieste servite e': %d", richieste);

    return 0;
}