#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>

#define DIM 256

volatile sig_atomic_t totRichieste = 0;

void sigint_handler(int signal) {
    printf("\nCTRL-C ricevuto\nIl numero totale delle richieste servite e': %d", totRichieste);
    exit(0);
}

int main(int argc, char** argv) {

    int fd, status;
    int pid1, pid2;
    int p1p2[2];
    char cognome[DIM], prefisso[DIM];

    // controllo parametri
    if (argc != 2) {
        fprintf(stderr, "Errore, numero argomenti errato - Uso: trovaParola fileAnagrafica\n");
        exit(1);
    }

    fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
        perror("Errore apertura file %s\n", argv[1]);
        if (errno = ENOENT) {
            fprintf("Il file non esiste");
        }
        exit(2);
    }
    close(fd);

    // controllo che fileAnagrafica sia un nome assoluto di file
    if (argv[1][0] != '/') {
        fprintf("Errore: %s deve essere un nome assoluto di file\n", argv[1]);
        exit(2);
    }

    signal(SIGINT, sigint_handler);

    printf("Inserisci il cognome del cliente da ricercare:\n");
    scanf("%s", cognome);

    printf("Inserisci il prefisso telefonico da ricercare:\n");
    scanf("%s", prefisso);
    
    while (strcmp(cognome, "fine") != 0 && strcmp(prefisso, "fine") != 0) {
        
        if (pipe(p1p2) < 0) {
            perror("Errore creazione pipe p1p2");
            exit(3);
        }

        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork() pid1");
            eix(4);
        }
        if (pid1 == 0) {

            close(p1p2[0]);
            close(stdout);

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup p1p2[1]");
                exit(5);
            }
            close(p1p2[1]);

            execlp("grep", "grep", cognome, argv[1], (char *)0);
            perror("Errore execlp grep");
            exit(6);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(7);
        }
        if (pid == 0) {
            close(p1p2[1]);
            close(stdin);

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(8);
            }
            close(p1p2[0]);

            execlp("grep", "grep", prefisso, (char *)0);
            perror("Errore execlp grep 2");
            exit(9);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        wait(&status);
        wait(&status);
        
        totRichieste++;

        printf("Inserisci il cognome del cliente da ricercare:\n");
        scanf("%s", cognome);
    }

    return 0;
}