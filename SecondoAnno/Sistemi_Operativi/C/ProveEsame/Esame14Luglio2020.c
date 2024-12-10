#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/wait.h>
#include <signal.h>

#define DIM 256

volatile sig_atomic_t counter;

void sigint_handler(int signal) {
    printf("\nCTRL-C ricevuto:\tnumero totale di richieste eseguite: %d", counter);
    exit(0);
}

int main(int argc, char** argv) {

   int fd, status;
   int pid1, pid2, pid3;
   int p1p2[2], p2p3[2];
   char tipologia[DIM];

    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Errore: numero argomenti errato");
        exit(1);
    }

    if (fd = open(argv[1], O_RDONLY) < 0) {
        perror("Errore apertura file");
        if (errno == ENOENT) {
            fprintf(stderr, "Il file non esiste");
        }
        exit(2);
    }
    close(fd);
    if (argv[1][0] != '/') {
        fprintf("Errore: %s deve essere un nome assoluto di file", argv[1]);
        exit(3);
    }

    signal(SIGINT, sigint_handler);

    printf("Inserisci la tipologia da cercare: \n");
    scanf("%s", tipologia);

    while(strcmp(tipologia, "esci") != 0) {

        if (pipe(p1p2) < 0) {
            perror("Errore creazione pipe p1p2");
            exit(4);
        }

        pid1 = fork();
        if (pid1 < 0) {
            perror("Errore fork() pid1");
            exit(5);
        }
        if (pid1 == 0) {
            close(p1p2[0]);
            close(stdout);

            if (dup(p1p2[1]) < 0) {
                perror("Errore dup p1p2[1]");
                exit(6);
            }
            close(p1p2[1]);

            execlp("grep", "grep", tipologia, argv[1], (char *)0);
            perror("Errore grep");
            exit(7);
        }

        if(pipe(p2p3) < 0) {
            perror("Errore creazione p2p3");
            exit(8);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(9);
        }
        if (pid2 == 0) {
            close(p1p2[1]);
            close(p2p3[0]);
            
            close(stdin);
            if(dup(p2p3[0]) < 0) {
                perror("Errore dup p2p3[0]");
                exit(10);
            }
            close(p2p3[0]);
            
            close(stdout);
            if(dup(p2p3[1]) < 0) {
                perror("Errore dup p2p3[1]");
                exit(11);
            }
            close(p2p3[1]);

            execlp("grep", "grep", "da pagre", (char *0));
            perror("Errore grep");
            exit(12);
        }

        pid3 = fork();
        if (pid3 < 0) {
            perror("Errore fork() pid3");
            exit(13);
        }
        if (pid == 0) {
            close(p1p2[0]);
            close(p1p2[1]);
            close(p2p3[1]);
            
            close(stdin);
            if (dup(p2p3[0]) < 0) {
                perror("Errore dup p2p3[0]");
                exit(14);
            }
            close(p2p3[0]);

            execlp("sort", "sort", "-n", "-r", (char *)0);
            perror("Errore sort");
            exit(15);
        }

        // incremento il contatore delle richieste eseguite
        counter++;

        close(p1p2[0]);
        close(p1p2[1]);
        close(p2p3[0]);
        close(p2p3[1]);
        wait(&status);
        wait(&status);
        wait(&status);

        printf("Inserisci la tipologia da cercare: \n");
        scanf("%s", tipologia);
    }

    return 0;
}