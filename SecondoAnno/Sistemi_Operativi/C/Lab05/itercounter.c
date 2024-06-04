#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <signal.h>

#define MAX_PROC_NUM 20

// Variabile globale
static volatile sig_atomic_t contatore = 0;

void gestore(int signo) {
    printf("Il processo %d ha eseguito %d interazioni prima del segnale %d\n", getpid(), contatore, signo);
    exit(0);
}

int main(int argc, char *argv[]) {

    int *pid;
    int i;
    int status;
    int Nf;
    int Nsec;

    // Controllo parametri
    if(argc != 3) {
        fprintf(stderr, "Usage: itercounter Nf Nsec");
        exit(1);
    }

    Nf = atoi(argv[1]);
    if(Nf <= 0) {
        fprintf(stderr, "L'argomento Nf deve essere un intero positivo");
        exit(2);
    }
    else if (Nf > MAX_PROC_NUM) {
        fprintf(stderr, "Il parametro <Nf> non puo' essere maggiore di %d\n", MAX_PROC_NUM);
        exit(3);
    }

    Nsec = atoi(argv[2]);
    if(Nsec <= 0) {
        fprintf(stderr, "L'argomento Nsec deve essere un intero positivo");
        exit(2);
    }

    pid = (int *) malloc(Nf * sizeof(int));

    for(i=0; i<Nf; i++) {
        pid[i] = fork();
        if(pid[i] < 0) {
            perror("Errore fork");
            exit(4);
        }

        if(pid[i] == 0) {
            struct sigaction sa;
            sigemptyset(&sa.sa_mask);
            sa.sa_handler = gestore;
            if (sigaction(SIGUSR1, &sa, NULL) < 0) {
                perror("Errore in sigaction, SIGUSR1 figlio");
                exit(-3);
            }
            while(1) {
                sleep(1);
                contatore++;
            }
        }
    }

    sleep(Nsec);
    for(i=0; i<Nf; i++) {
        kill(pid[i], SIGUSR1);      // Metodo 1
    }

    /*
    struct sigaction sa_ign;
    sigemptyset(&sa_ign.sa_mask);
    sa_ign.sa_flags = 0;
    sa_ign.sa_handler = SIG_IGN;
    if (sigaction(SIGUSR1, &sa_ign, NULL) < 0) {
        perror("Errore in sigaction, SIGUSR1 figlio");
        exit(-3)
    }

    kill(0, SIGUSR1);
    */

   free(pid);

   for (i=0; i<Nf; i++) {
       wait(&status);
   }
   return 0;

}