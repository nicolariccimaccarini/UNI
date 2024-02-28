#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>

#define DIM 128

static volatile sig_atomic_t counter = 0;

void counter_handler(int signal) {
    printf("Rischieste servite: %d", counter);
}

int main(int argc, char **argv) {

    struct sigaction sa;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sa.sa_handler = counter_handler;

    int fd, status;
    int p1p2[2], p2p0[2];
    int pid, pid1;
    char cognome[DIM];
    int prefisso;

    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso: trovaParola fileAnagrafican\n");
        exit(1);
    }
    if (argv[1][0] != '/') {
        fprintf(stderr, "%s deve essere un path assoluto", argv[1]);
        exit(2);
    }
    if ((fd = open(argv[1], O_RDONLY)) < 0) {
        fprintf(stderr, "%s deve essre un file!", argv[1]);
        exit(3);
    }

    printf("Inserire cognome cliente");
    scanf("%s", cognome);

    while ((strcmp(cognome, "fine")) != 0) {

        if ((fd = open(argv[1], O_RDONLY)) < 0) {
            fprintf(stderr, "Errore apertura file");
            if (errno = ENOENT) {
                fprintf("errore, File none sistente");
            }
            exit(-1);
        }

        
        printf("Inserisci il prefisso telefonico: ");
        scanf("%d", &prefisso);

        if (pipe(p1p2) < 0) {
            perror("Errore creazione prima pipe");
            exit(4);
        }

        pid = fork();

        if (pid < 0) {
            perror("Errore prima fork");
            exit(6);
        }
        if (pid > 0) {
            // padre
            pid1 = fork(); // creo il processo fratello

            if (pid1 < 0) {
                perror("Errore seconda fork");
                exit(7);
            }
        }

        if (pid == 0) {
            // figlio 1

            close(p1p2[0]);
            close(1); // chiudo stdout

            if (dup(p1p2[1]) < 0) {
                perror("errore dup(p1p2[0])");
                exit(8);
            }
            close(p1p2[1]);

            execlp("grep", "grep", cognome, fd, NULL);
            perror("Errore execlp");
            exit(9);
        }
        if (pid1 == 0) {
            // figlio 2

            close(p1p2[1]);
            close(0); // chiudo stdin

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(11);
            }
            close(p1p2[0]);

            read(p1p2[0], risultati, sizeof(risultati));

            execlp("grep", "grep", prefisso, NULL);
            perror("Errore execlp grep 2");
            exit(10);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        wait(&status);
        wait(&status);
        
        counter++;

        printf("Inserisci cognome oppure 'fine'/CRTL-C per terminare: ");
        scanf("%s", cognome);
    }

    prntf("Richieste servite: %d\n", counter);
    return 0;
}