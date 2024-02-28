#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

#define DIM 256

static volatile sig_atomic_t totRichieste;

void signit_handler(int signal) {
    printf("\nCTRL-C ricevuto:\tSono state servite %d richieste\n", totRichieste);
    exit(0);
}

int main(int argc, char** argv) {
    
	int status, pid1, pid2, pid3, fd;
	char tipologia[DIM];
	int p1p2[2], p2p3[2];
    struct sigaction sa;
    
    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Errore numero argomenti, uso: verfica fileBollette\n");
        exit(1);
    }
    if ((fd=open(argv[1], O_RDONLY)) < 0) {
        fprintf(stderr, "Errore apertura file\n");
        if (errno == ENOENT) {
            fprintf(stderr, "il file non esiste\n");
        }
    }

    sigemptyset(&sa.sa_mask);
    &sa.sa_flags=0;
    sa.sa_handler=sigint_handler;
    sigaction(SIGINT, &sa, NULL);

    printf("Inserisci la tipologia da ricercare: ");
    scanf("%s", tipologia);

    while (strcmp(tipologia, "esci") != 0) {

        if (pipe(p1p2) < 0) {
            perror("Errore creazione pipe p1p2");
            exit(2);
        }
        if (pipe(p2p3) < 0) {
            perror("Errore creazione pipe p2p3");
            exit(3);
        }

        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork pid1");
            exit(4);
        }
        if (pid1 == 0) {
            close(p2p3[0]);
            close(p2p3[1]);
            close(p1p2[0]);
            close(1);

            if (dup(p1p2[1]) < 0) {
                perror("errore dup p1p2[1]");
                exit(5);
            }
            close(p1p2[1]);

            execlp("grep", "grep", tipologia, argv[1], (char *)0);
            perror("Errore execlp");
            exit(6)
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork pid2");
            exit(7);
        }
        if (pid2 == 0) {
            close(p1p2[1]);
            close(p2p3[0]);
            close(0);

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(8);
            }
            close(p1p2[0]);

            execlp("grep", "grep", "da pagare", (char *)0);
            perror("errore");
            exit(9);
        }

        pid3 = fork();
        if (pid3 < 0) {
            perorr("Errore fork pid3");
            exit(10);
        }
        if (pid3 == 0) {
            close(p1p2[0]);
            close(p1p2[1]);
            close(p2p3[1]);

            close(0);

            if (dup(p2p3[0]) < 0) {
                perror("Errore dup p2p3[1]");
                exit(11);
            }
            close(p2p3[0]);

            execlp("sort", "sort", "-r", "-n", (char *)0);
            perror("Errore execlp");
            exit(12);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        close(p2p3[0]);
        close(p2p3[1]);
        wait(&status);
        wait(&status);
        wait(&status);

        totRichieste++;

        printf("Inserisci la tipologia da ricercare: ");
        scanf("%s", tipologia);

    }

    printf("Il numero totale di richieste eseguite e': %d", totRichieste);
    return 0;
}