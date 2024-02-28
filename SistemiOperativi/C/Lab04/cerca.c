#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/wait.h>

int main(int argc, char** argv) {

    int pid, fd, status, i;

    // Controllo degli argomenti
    if (argc < 3) {
        fprintf(stderr, "Uso: ./cerca <nomefile> <stringa1> ... <stringaN> \n");
        exit(1);
    }

    // Creo (sovreascrivo se esiste) e chiudo il file conteggio.txt
    fd = open("conteggio.txt", O_CREAT | O_TRUNC | O_WRONLY, 0644);
    if (fd < 0) {
        perror("Errore nella creazione del file di conteggio, termino\n");
        exit(2);
    }

    // Chiudo il file
    close(fd);

    // Creo i figli 
    for(i=2; i<argc; i++) {
        if ((pid = fork()) < 0) {
            // Errore
            perror("fork");
            exit(3);
        }
        else if (pid == 0) {
            // Figlio
            char log[256];

            // Apro il file in append
            fd = open("conteggio.txt", O_WRONLY | O_APPEND);

            // Creo il log dell'operazione di conteggio
            sprintf(log, "%s %s\n", argv[1], argv[i]);

            // Scrivo il log sul file
            write(fd, log, strlen(log));

            // Chiudo il file
            close(fd);

            printf("\n Num volte che si ripete la stringa %s: \n", argv[i]);
            // Chiamata a: grep -c stringa nomefile
            execlp("grep", "grep", "-c", argv[i], argv[1], (char*)0);

            perror("exec");
            exit(4);
        }
        else {
            // Padre - attendo la terminazione del figlio
            wait(&status);
        }
    }

    // Esco senza errrori
    return 0;

}   