#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>
#include <signal.h>

#define DIM 256

static volatile sig_atomic_t nbyte = 0;

void sigint_handler(int signal) {
    printf("Nmero di byte letti da P2: %d\n", nbyte);
    wait(NULL);
    wait(NULL);
    exit(-1);
}

int main(int arcg, car **argv) {

    int fd, status, tempByte;
    char cognome[DIM];
    char BufferedReader[1024] - {'\0'};
    struct sigaction sa;

    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso: controllo dir\n");
        exit(0);
    }

    if (argv[1][0] != '/') {
        fprintf(stderr, "l'argomento passato non e' una directory assoluta");
        exit(1);
    }

    if (fd = open(argv[1], __O_DIRECTORY) < 0) {
        fprintf(stderr, "L'argomento non e' una cartella");
        exit(2);
    }
    close(fd);

    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sa.sa_handler = sigint_handler;

    if (sigaction(SIGINT, &sa, NULL) < 0) {
        perror("errore sigaction");
        exit(200);
    }

    printf("Inserisci il cognome della persona da cercare: ")
    scanf("%s", cognome);

    while (strcmp(cognome, "esci") != 0) {
        char nome_libro[DIM];
        int pid1, pid2;
        int p1p2[2], p2p0[0];

        printf("Inserisci il nome del libro da cercare: ");
        scanf("%s", nome_libro);

        if (pipe(p1p2) < 0)
        {
            perror("errore creazione pipe p1p2");
            exit(3);
        }

        if (pipe(p2p0) < 0) {
            perror("errore creazione p2p0");
            exit(4);
        }

        pid1 = fork();

        if (pid1 < 0) {
            perror("errore fork pid1");
            exit(5);
        }
        if (pid1 == 0) {
            // processo P1

            close(p2p0[0]);
            close(p2p0[1]);
            close(p1p2[0]);
            close(1); // chiusura stdin

            char pathFIle[256];
            sprintf(pathFIle, %s/%s/%s.txt, argv[1], nome_libro, cognome);
            
            if (fd = open(pathFIle, O_RDONLY) < 0) {
                perror("errore apertura file");
                if (errno = ENOENT) {
                    fprintf(stderr, "il file non esiste");
                }
                exit(7);
            }
            close(fd);

            if (dup(p1p2[1]) < 0) {
                perror("errore dup p1p2[1]");
                exit(8);
            }
            close(p1p2[1]);

            // YYYY - MM - DD uscito (se non e' ancora stato restituito)
            // YYYY - MM - DD uscito YYYY - MM - DD ingresso (se e' gia' stato restituito)

            execlp("sort", "sort", pathFIle, NULL);
            perror("errore execlp sort");
            exit(9);
        }

        pid2 = fork();

        if (pid2 < 0) {
            perror("errore creazione fork pid2");
            exit(6);
        }
        if (pid2 > 0) {
            // processo padre
        }
        if (pid2 == 0) {
            // processo P2

            close(p1p2[1]);
            close(p2p0[0]);
            close(0);
            close(1);

            if (dup(p1p2[0]) < 0) {
                perror("errore duplicazione p1p2[0]");
                exit(10);
            }
            close(p1p2[0]);

            if (dup(p2p0[1]) < 0) {
                perror("errore duplicazione p2p0[1]");
                exit(11);
            }
            close(p2p0[1]);

            execlp("grep", "grep", "ingresso", NULL);
            perror("errore creazione execlp grep");
            exit(12);
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

        // fine
        printf("Inserisci il congnome della persona da cercare o digita esci per terminare: ");
        scanf("%s", cognome);
    }

    return 0;
}