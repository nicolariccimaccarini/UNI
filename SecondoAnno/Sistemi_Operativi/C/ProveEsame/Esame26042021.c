#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#define DIM_REF 100
#define DIM_FP 200
#define DIM_RES 1024

#define ERR_MSG "nessun risultato trovato\n"

static volatile sig_atomic_t byte_ricevuti = 0;

void sigint_handler(int signal) {
    printf("\nCTRL-C ricevuto\nbyte totali ricevuti da P2: %d\n", byte_ricevuti);
    exit(0);
}

int main(int argc, char **argv) {

    int fd, i, pid, pid_nipote, status, nread, nwrite;
    int pipe p0p1[2], p2p0[2];
    char ref[DIM_REF], filepath[DIM_FP], res[DIM_RES];

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
    if (argv[1][0] == '/') {
        fprintf(stderr, "dir deve essere un path relativo\n");
        exit(3);
    }
    for (i=0; i<strlen(argv[2]); i++) {
        if (argv[2][i] < '0' && argv[2][i] > '9') {
            fprintf(stderr, "data deve contenere solo caratteri numerici\n");
            exit(4);
        }
    }

    // controllo che <data> esista
	sprintf(filepath, "%s/%s.txt", argv[1], argv[2]);
	fd = open(filepath, O_RDONLY);
	if (fd < 0) {
		perror("P0: open");
		exit(5);
	}
	close(fd);

    signal(SIGINT, sigint_handler);

    if (pipe(p0p1) < 0) {
		perror("P0: pipe p0p1");
		exit(6);
	}

	// creo pipe per comunicazione tra P0 e P2
	if (pipe(p2p0) < 0) {
		perror("P0: pipe p2p0");
		exit(7);
	}

    pid = fork();
    if (pid < 0) {
        perror("Errore fork() pid");
        exit(8);
    }
    if (pid == 0) {
        // processo P1 (figlio)

        close(p0p1[1]);
        close(p2p0[0]);

        // imposto la gestione SIGINT default
        signal(SIGINT, SIG_DFL);

        while (1) {
            // leggo refertatore inviato da P0
            nread = read(p0p1[0], ref, sizeof(ref) - 1);
            if (nread < 0) {
                perror("Errore lettura\n");
                exit(9);
            }
            ref[nread] = '\0';

            pid_nipote = fork();
            if (pid_nipote < 0) {
                perror("Errore fork() pid_nipote");
                exit(10);
            }
            if (pid_nipote == 0) {
                // processo P2 (nipoote)

                close(p0p1[0]);
                close(stdout);

                if (dup(p2p0[1]) < 0) {
                    perror("Errore dup p2p0[1]");
                    exit(11);
                }
                close(p2p0[1]);

                execlp("grep", "grep", ref, filepath, (char *)0);
                perror("Errore execlp");
                exit(12);
            }

            // attendo terminazione P2
            wait(&status);
        }
    }

    close(p0p1[1]);
    close(p2p0[1]);

    while (1) {

        printf("Inserisci refertatore\n");
        scanf("%s", ref);

        // P0 invia il refertatore a P1
        nwrite = write(p0p1[1], ref, strlen(ref) + 1);
        if (nwrite != (int)(strlen(ref) + 1)) {
            perror("Errore scrittura (write)");
            exit(13);
        }

        // P0 legge e stampa i risultati inviati da P2
        nread = read(p2p0[0], res, sizeof(res) - 1)
        if (nread < 0) {
            perror("Errore scrittura (read)");
            exit(14);
        }
        res[nread] = '\0';
        printf("%s\n", res);

        // incremento contatore byte ricevuti
        byte_ricevuti += nread;
    }

    return 0;
}