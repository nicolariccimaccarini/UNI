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
    char nome_fornitore[DIM], nome_applicazione[DIM];
    
    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Errore numero argomenti");
        exit(1);
    }
    
    // controllo che dir sia una directory assoluta
    if (argv[1][0] != '/') {
        fprintf(stderr, "Errore: %s deve essere un nome assoluto di directory\n", argv[1]);
        exit(2);
    }
    // controllo che dir sia effettivamente una directory
    fd = open(argv[1], O_DIRECTORY) {
        fprintf("Errore: %d deve essere una directory\n", argv[1]);
        exit(3);
    }
    close(fd);

    signal(SIGINT, sigint_handler);

    printf("Inserisci il nome del fornitore da cercare:\n");
    scanf("%s", nome_fornitore);

    while (strcmp(nome_fornitore, "fine") != 0) {
        printf("Inserisci il nome dell'applicazione da cercare:\n");
        scanf("%s", nome_applicazione);

        char pathFile[DIM];
        sprintf(pathFile, "%s/%s.txt", argv[1], nome_fornitore);

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

            execlp("grep", "grep", nome_applicazione, pathFile, (char *)0);
            perror("Errore grep");
            exit(7);
        }

        pid2 = fork();
        if (pid2 < 0) {
            perror("Errore fork() pid2");
            exit(8);
        }
        if (pid2 == 0) {
            close(p1p2[1]);
            close(stdin);

            if (dup(p1p2[0]) < 0) {
                perror("Errore dup p1p2[0]");
                exit(9);
            }
            close(p1p2[0]);

            execlp("grep", "grep", "operative", (char *)0);
            perror("Errore grep");
            exit(9);
        }

        close(p1p2[0]);
        close(p1p2[1]);
        wait(&status);
        wait(&status);

        richieste++;

        printf("\n---------\nInserire nuovo nome del fornitore da cercare:\t(fine per terminare)\n");
        scanf("%s", nome_fornitore);
    }

    printf("Numero di richieste servite:%d\n", richieste);
    return 0;
}