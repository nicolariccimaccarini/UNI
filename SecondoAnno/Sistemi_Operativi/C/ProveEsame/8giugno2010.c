#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

#define DIM 128

int main(int argc, char **argv) {

    int pid, pid1, pid2, i, status;
    char vino[DIM], risultato_pid2[DIM]={'\0'};
    int pipe p1p2[2], p2p3[2], p3p0[2];

    // Controllo argomenti
    if (argc < 2) {
        fprintf(stderr, "Uso: trova_vini <cantina-1> ... <cantina-N>\n");
        exit(1);
    }

    printf("Inserisci il vino da cercare: ");
    scanf("%s", vino);

    while(strcmp(cantina, "fine") != 0) {

        if (pipe(p1p2) < 0) {
            perror("Errore pipe()");
            exit(2);
        }
        if (pipe(p2p3) < 0) {
            perror("Errore pipe()");
            exit(3);
        }
        if (pipe(p3p0) < 0) {
            perror("Errore pipe()");
            exit(4);
        }

        pid = fork();
        if (pid1 < 0) {
            perror("Errore fork()");
            exit(5);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork()");
            exit(6);
        }

        pid2 = fork();
        if (pid3 < 0) {
            perror("Errore fork()");
            exit(7);
        }

        if (pid == 0) {
            // processo 1
            close(p1p2[0]);
            close(p2p3[0]);
            close(p2p3[1]);
            close(p3p0[0]);
            close(p3p0[1]);
            close(1); // chiusura stdout

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup() 1");
                exit(8);
            }
            close(p1p2[1]);

            // seleziono vini richiesti
            execlp("grep", "grep", vino, argv[1], NULL);
            perror("Errore execlp()");
            exit(9);
        }
        if (pid1 == 0) {
            // processo 2
            close(p1p2[1]);
            close(p2p3[1]);
            close(p3p0[0]);
            close(p3p0[1]);
            close(0); // chiusura stdin
            close(1); // chiusure stdout

            if (read(p1p2[0], risultato_p1, sizeof(risultato_p1)) < 0) {
                perror("Errore read()");
                exit(10);
            }

            if (dup(p2p3[0]) < 0) {
                perror("Errore dup() 2");
                exit(11);
            }
            close(p2p3[0]);

            // seleziono i vini disonibili
            execlp("grep", "grep", "-c", "disponibili", NULL);
            perror("Errore execlp()");
            exit(12);
        }
        if (pid2 == 0) {
            close(p1p2[0]);
            close(p1p2[1]);
            close(p2p3[1]);
            close(p3p0[1]);
            close(0); 
            close(1);

            if (dup(p3p0[0]) < 0) {
                perror("Errore dup() 3");
                exit(12);
            }
            close(p3p0[0]);

            // ordinamento in relazione al prezzo
            execlp("sort", "sort", "-n", NULL);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        close(p2p3[0]);
        close(p2p3[1]);
        close(p3p0[1]);
        close(0);

        wait(&status);
        wait(&status);
        wait(&status);

        if (read(p3p0[0]) < 0) {
            perror("Errore read() finale");
            exit(13);
        }

        printf("vini disponibili: %s", risultato_pid2);

        printf("Inserisci il vino da cercare o 'fine' per terminare: ");
        scanf("%s", vino);
    }

    return 0;
}