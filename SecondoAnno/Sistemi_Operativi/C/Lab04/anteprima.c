#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>

// Dimensione massima del nome del file
#define DIM 50

int main(int argc, char **argv) {

    int pid, status, fd, num;
    char nome[DIM];

    // Controllo il numero degli argomenti
    if (argc != 2) {
        printf("Uso: ./anteprima <num>\n");
        exit(1);
    }

    if ((num = atoi(argv[1])) <= 0) {
        printf("Errore: %s deve essere un intero positivo maggiore di zero\n", argv[1]);
        exit(2);
    }

    printf("Inserisci il nome del file di cui visualizzare l'anteprima (\"fine\" per uscire): ");
    scanf("%s", nome);

    while (strcmp(nome, "fine")) {
        // per controllare se il file esiste, provo ad aprirlo
        if ((fd = open(nome, O_RDONLY)) < 0) {
            /* la open potrebbe fallire anche se il file esiste, 
               ad esempio nel caso manchino i permessi in lettura. 
               Per gestire la differenza, possiamo guardare il valore di errno
             */

            if (errno == ENOENT) {
                printf("Il file %s non esiste\n", nome);
            }
            else {
                // Errore generico
                perror("open");
                exit(3);
            }
        }
        else {
            /* Chiudo il file, che a questo esiste. 
               Lasciandolo aperto, causeremo un resource
               leak, che va evitato. 
            */
           close(fd);

           // Creo un figlio pe visualizzare l'anteprima
           if ((pid = fork()) < 0) {
               perror("fork");
               exit(4);
           }
           else if (pid == 0) {
               // Figlio
               printf("\n Anteprima del file %s: \n", nome);
               execlp("head", "head", "-n", argv[1], nome, (char *)0);
               perror("exec");
               exit(5);
           }
           // Padre: aspettoc che il figlio termini
           wait(&status);
        }

        printf("\n Inserisci il nome del file di cui visualizzare l'anteprima (\"fine\" per uscire): ");
        scanf("%s", nome);
    }

    // Esco senza errori
    return 0;
}