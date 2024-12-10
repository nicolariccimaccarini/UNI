#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#define DIM 256

static volatile sig_atomic_t nRichieste = 0;

void sigint_handler(int signal) {
    printf("\nCTRL-C ricevuto\nNumero totale di richieste servite: %d", nRichieste);
    exit(0);
}

int main(int argc, char** argv) {

    int fd, status;
    int pid1, pid2, pid3, p1p2[2], p2p3[2];
    char pathFile[DIM];
    int giorno, mese, anno, data;

    // controllo argomenti
    if (argc != 3) {
        fprintf(stderr, "Errore argomenti - Uso: trova_biglietti destinazione N");
        exit(1);
    }

    // controllo che il file /var/local/ticket/argv[1] esista
    
    sprintf(pathFile, "/var/local/ticket/%s.txt", argv[1]);
    fd = open(pathFile, O_RDONLY);
    if (fd < 0) {
        perror("Errore, impossibile aprire il filen\n");
        if (errno == ENOENT) {
            fprintf(stderr, "Il file non esiste\n");
        }
        exit(2);
    }

    signal(SIGINT, sigint_handler);

    printf("Inserisci il giorno, il mese e l'anno della data di partenza:\n");
    scanf("%d%d%d", &giorno, &mese, &anno);

    while (giorno != -1 && mese != -1 || anno != -1) {

        if (giorno < 0 || mese < 0 || anno < 0) {
            printf("Errore: inserire numeri positivi\n");
            continue;
        }
        else if (giorno < 1 && giorno > 31) {
            printf("Errore: inserire <giorno> compreso tra 0 e 31\n");
            continue;
        }
        else if (mese < 1 && mese > 12) {
            printf("Errore: inserire <mese> compreso tra 0 e 12");
            continue;
        }

        sprinf(data, "%d%d%d", giorno, mese, anno);

        if (pipe(p1p2) < 0) {
            perror("Errore creazione pipe p1p2");
            exit(4);
        }
        if (pipe(p2p3) < 0) {
            perror("Errore creazione pipe p2p3");
            exit(5);
        }
        
        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork() pid1");
            exit(3);
        }
        if (pid1 == 0) {
            // P1 deve scrivere dentro la pipe p1p2 quindi chiudo la lettura e lo stdout
            close(p1p2[0]);
            close(p2p3[0]);
            close(p2p3[1]);
            close(stdout);

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup p1p2[1]");
                exit(6);
            }
            close(p1p2[1]);

            execlp("grep", "grep", data, pathFile, (char *)0);
            perror("Errore execlp grep");
            exit(20);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(7);
        }
        if (pid2 == 0) {
            close(p1p2[1]);
            close(p2p3[0]);
            close(stdin);
            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(8);
            }
            close(p1p2[0]);

            close(stdout);
            if (dup(p2p3[1]) < 0) {
                perror("Errore dup p2p3[1]");
                exit(9);
            }
            close(p2p3[0]);

            execlp("sort", "sort", "-n", (char *)0);
            perror("Errore execlp sort");
            exit(10)
        }

        pid3 = fork();
        if (pid3 < 0) {
            perror("Errore fork() pid3");
            exit(11);
        }
        if (pid3 == 0) {
            close(p1p2[1]);
            close(p1p2[0]);
            close(p2p3[1]);
            close(stdin);

            if (dup(p2p3[0]) < 0) {
                perror("Errore dup p2p3[0]");
                exit(12);
            }
            close(p2p3[0]);

            execlp("head", "head", "-n", argv[2], (char *)0);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        close(p2p3[0]);
        close(p2p3[1]);
        wait(&status);
        wait(&status);
        wait(&status);

        nRichieste++;

        printf("Inserisci il giorno, il mese e l'anno della data di partenza:\n");
        scanf("%d%d%d", &giorno, &mese, &anno);
    }

    printf("Numero totale di richieste servite: %d", nRichieste)
    return 0;
}