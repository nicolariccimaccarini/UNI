#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#define DIM 128

static volatile sig_atomic_t byte_ricevuti = 0;

void sigint_handler(int signal) {
    printf("byte totali ricevuti da P2: %d\n", byte_ricevuti);
    exit(0);
}

int main(int argc, char **argv) {

    int fd, i, pid, pid_nipote, status;
    int pipe p0p1[2], p2p0[2];
    char nome[DIM];

    // Controllo degli argomenti
    if (argc != 3) {
        fprintf(stderr, "Uso: test_eseguiti dir data\n");
        exit(1);
    }
    if ((fd = open(argv[1], O_DIRECTORY)) < 0) {
        fprintf(stderr, "dir deve essere una directory\n");
        exit(2);
    }
    close(fd);
    if (argv[1][0] != '/') {
        fprintf(stderr, "dir deve essere un path relativo\n");
        exit(3);
    }
    for (i=0; i<strlen(argv[2]); i++) {
        if (argv[2][i] < '0' && argv[2][i] > '9') {
            fprintf(stderr, "data deve contenere solo caratteri numerici\n");
            exit(4);
        }
    }

    if (pipe(p0p1) < 0) {
        perror("Errore creazione prima pipe\n");
        exit(6);
    }
    if (pipe(p2p0) < 0) {
        perror("Errore creazione seconda pipe\n");
        exit(7);
    }

    pid = fork();

    if (pid < 0) {
        perror("fork()");
        exit(5);
    }
    if (pid > 0) {
        close(pipe p0p1[0]);
        close(pipe p2p0[1]);

        printf("Inserisci il nome della persona: ");
        scanf("%d", nome);

        if (write(pipe p0p1[1], nome) < 0) {
            perror("errore in scrittura\n");
            exit(8);
        }
        close(pipe p0p1[1]);

        if (read(pipe p2p0[0]) < 0) {
            perrro("Errore in lettura\n");
            exit(12);
        }
    }
    if (pid == 0) {
        close(pipe p0p1[1]); 

        if (read(pipe p0p1[0], nome) < 0) {
            perror("Errore in lettura\n");
            exit(9);
        }

        close(pipe p0p1[0]);

        pid_nipote = fork();

        if (pid_nipote < 0) {
            perror("fork()");
            exit(10);
        }
        if (pid_nipote == 0) {
            close(pipe p2p0[0]);
            char NomeFile[DIM];

            sprintf(NomeFile, "%s%s.txt", argv[1], argv[2]);

            close(1); // chiudo lo stdout

            if (dup(pipe p2p0[1]) < 0) {
                perror("dup()");
                exit(11);
            }
            close(pipe p2p0[1]);

            execlp("grep", "grep", nome, NomeFile, (char *)0);
        }

        wait(&status);
        exit(20);
    }

    return 0;
}