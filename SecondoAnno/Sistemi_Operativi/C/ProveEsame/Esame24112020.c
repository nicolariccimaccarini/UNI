#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include <fcntl.h>

#define DIM 256

static volatile sig_atomic_t nbyte = 0;

void sigint_handler(int signal) {
    printf("Nmero di byte letti da P2: %d\n", nbyte);
    wait(NULL);
    wait(NULL);
    exit(-1);
}

int main(int argc, char** argv) {

    char nomeLibro[DIM], cognome[DIM], nomeDir[DIM], nomeFile[DIM], risultato[DIM];
    int pid1, pid2;
    int p1p2[2], p2p0[2]
    int status, fd, tempByte;
    char BufferedReader[1024] - {'\0'};
    struct sigaction sa;

    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Errore: numero argomenti errato");
        exit(1);
    }   
    if (f = open(argv[1], __O_DIRECTORY) < 0) {
        if (errno == ENOENT) {
            fprintf("file inesistente");
            exit(2);
        }
    }
    close(f);
    if (argv[1][0] != '/') {
        fprintf(stderr, "dir deve essere un path di directory assoluta");
        exit(3);
    }

    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sa.sa_handler = sigint_handler;

    printf("Inserisci il congome della persona da cercare: \n");
    scanf("%s", cognome);

    while (strcmp(cognome, "esci") != 0) {

        printf("inserisci il nome del libro da cercare: \n");
        scanf("%s", nomeLibro);

        if (pipe(p1p2) < 0) {
            perror("Errore creazione pipe p1p2");
            exit(4);
        }
        if (pipe(p2p0) < 0) {
            perror("Errore creazione pipe p2p0");
            exit(5);
        }

        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork() pid1");
            exit(6);
        }
        if (pid == 0) {
            // processo p1
            sprintf(nomeDir, "%s/%s", argv[1], nomeLibro);

            fd = open(nomeDir, __O_DIRECTORY)
            if (fd < 0) {
                if (errno = ENOENT) {
                    fprintf(stderr, "Errore directory non esistente")
                    exit(7);
                }
                perror("Errore apertua directory")
                exit(7);
            }
            close(fd);

            sprintf(nomeFile, "%s/%s.txt", nomeDir, cognome);
            fd = open(nomeFile, O_RDONLY);
            if (fd < 0) {
                perror("Errore apertuara file");
                eit(8);
            }
            close(fd);

            close(p1p2[0]);
            close(p2p0[0]);
            close(p2p0[1]);
            close(1); // chiudo stdout

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup p1p2[1]");
                exit(9);
            }
            close(p1p2[1]);

            execlp("sort", "sort", nomeFile, (char *)0);
            perror("Errore eeclp");
            exit(10);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(11);
        }
        if (pid2 == 0) {
            // figlio p2
            close(p1p2[1]);
            close(p2p0[0]);
            close(0);

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup");
                exit(12);
            }
            close(p1p2[0]);

            close(1);
            if (dup(p2p0[1]) < 0) {
                perror("Errore dup");
                exit(13);
            }
            close(p2p0[1]);

            execlp("grep", "grep", nomeFile, "ingresso", (char *)0);
            perror("Errore execlp");
            exit(14);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        close(p2p0[1]);

        wait(&status);
        wait(&status);

        if (tempByte = read(p2p0[0], BufferedReader, sizeof(BufferedReader)) < 0) {
            perror("errore nella lettura dal padre");
            exit(13);
        }
        close(p2p0[0]);

        nbyte += tempByte;
        printf("%s\n", BufferedReader);

        printf("%s\n", risultato);
    }    

    return 0;
}