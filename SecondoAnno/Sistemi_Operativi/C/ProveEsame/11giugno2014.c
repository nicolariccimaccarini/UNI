#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#define DIM 128


static volatile sig_atomic_t cont=0;

void sigint_handler(int signal) {
    printf("richieste serivite: %d\n", cont);
    wait(&status);
    exit(0);
}

int main(int argc, char **argv) {

    struct sigaction sa;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0
    sa.sa_handler = sigint_handler;
    if (sigaction(SIGINT, &sa, NULL) < 0) {
        perror("sigaction()");
        exit(100);
    }

    char fornitore[DIM], applicazione[DIM];
    int pid;
    
    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso: conta_vm dir\n");
        exit(1);
    }
    if (argv[1][0] != '/') {
        fprintf(stderr, "dir non e' assoluto\n");
        exit(2);
    }

    printf("Inserisci il nome del fornitore: ");
    scanf("%s", fornitore);

    while (strcmp(fornitore, "fine") != 0)
    {
        int fd, status; 
        char NomeFile[DIM], RisultatoP2[8] = {'/0'};

        int pipe p1p2[2];
        int pipe p2p0[2];

        printf("Inserisci il nome dell'applicazione: ");
        scanf("%s", applicazione);

        sprintf(NomeFile, "%s%s.txt", argv[1], fornitore);

        fd = open(NomeFile, O_RDONLY);

        if (fd < 0) {
            fprintf(stderr, "Errore apertura file\n");

            if (errno == ENOENT) {
                fprintf(stderr, "Il file non esiste\n");
            }
        }

        close(fd);

        if (pipe(p1p2) < 0) {
            perror("Errore creazione prima pipe");
            exit(10);
        }

        if (pipe(p2p0) < 0) {
            perror("Errore creazione seconda pipe");
            exit(11);
        }

        pid = fork();

        if (pid < 0) {
            perror("fork()");
            exit(3);
        }
        if (pid > 0) { // Siamo dentro il padre
            
            // Creo il processo figlio (fratello di pid)
            pid2 = fork();

            if (pid2 < 0) {
                perror("fork()");
                exit(4);
            }
        }
        if (pid == 0) {

            close(pipe p1p2[0]);
            close(pipe p2p0[0]);
            close(pipe p2p0[1]);

            close(1) // Chiudo lo standard output

            if (dup(p1p2[1]) < 0) {
                perror("dup()");
                exit(4);
            }
                close(pipe p1p2[1]);

            execlp("grep", "grep", applicazione, NomeFile, NULL); // Cerco nel file le righe che hanno l'applicazione richiesta e le stampo dentro la pipe
            perror("execlp()");
            exit(5); 
        }
        if (pid2 == 0) {

            close(pipe p1p2[1]);
            close(pipe p2p0[0]);

            close(0); // Chiudo lo standard input

            if (dup(p1p2[0]) < 0) {
                perror("dup()");
                exit(6);
            }
                close(pipe p1p2[0]);

            close(1); 

            if (dup(p2p0[1]) < 0) {
                perror("dup()");
                exit(7);
            }
                close(pipe p2p0[1]);

            execlp("grep", "grep", "-c", "operative", NULL); // Cerco nel file le righe che sono operative e le stampo dentro la pipe
        }

        close(pipe p1p2[0]);
        close(pipe p1p2[1]);
        close(pipe p2p0[1]);

        wait(&status);
        wait(&status);

        if (read(pipe p2p0, RisultatoP2, sizeof(RisultatoP2)) < 0) {
            perror("read()");
            exit(8);
        }

        printf("Vi sono %d vm operative per l'applicazione %s\n", atoi(RisultatoP2), applicazione);
        cont++;

        printf("Inserisci il nome del fornitore: ");
        scanf("%s", fornitore);   
    }

    printf("richieste serivite: %d\n", cont);

    return 0;
    
}