#define _POSIX_C_SOURCE	200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#ifdef USE_LIBUNISTRING
#include <unistr.h> 
#endif
#include "rxb.h"
#include "utils.h"

#define MAX_REQUEST_SIZE (64 * 1024)

int main(int argc, char **argv) {
    int sd, err; // err = intero che memorizza gli errori
    struct addrinfo hints, *ptr, *res;
    char* server, *porta;
    rxb_t rxb;

    char request[MAX_REQUEST_SIZE];
    char response[MAX_REQUEST_SIZE];

    size_t response_len;

    // controllo argomenti
    if (argc != 3) {
        fprintf(stderr, "Usage: controllo_conto_corrente <server> <porta>");
        exit(EXIT_FAILURE);
    }

    // Ignoro sigpipe --> segnale sigpipe viene sostituito da sig_ignore
    signal(SIGPIPE, SIG_IGN);

    // costruzione dell'indirizzo
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM; 

    // risoluzione host
    server = argv[0];
    porta = argv[1];
    err = getaddrinfo(server, porta, &hints, &res);
    if (err < 0) {
        fprintf(stderr, "Errore risoluzione nome: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    for (ptr = res; ptr != NULL; ptr->ai_next) {
        // se socket fallisce salto alla prossima posizione
        sd  = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);        
        if (sd < 0) 
            continue;

        // se connect funziona esco dal ciclo
        err = connect(sd, ptr->ai_addr, ptr->ai_addrlen);
        if (err == 0) {
            break;
        }
        
        close(sd); // chiudiamo la socket se la connessione fallisce
    }

    // verifichiamo che sia uscito il ciclo perche' connesso e non perche' ha smesso di iterare
    if (ptr == NULL) {
        fprintf(stderr, "Errore nella risouzione del nome: nessun indirizzo trovato");
        exit(EXIT_FAILURE);
    }

    // liberiamo la memoria allocata da getaddrinfo
    freeaddrinfo(res);

    // inizializziamo il bufferi di ricezione 
    rxb_init(&rxb, MAX_REQUEST_SIZE);

    while(1) {
        char categoria[MAX_REQUEST_SIZE];

        // leggo la stringa di richiesta
        puts("Inserisci la categoria da ricercare: ");
        gets(categoria, strlen(categoria));

        // invio la richiesta al server
        err = write_all(sd, categoria, strlen(categoria));
        if (err < 0) {
            perror("write_all");
            exot(EXIT_FAILURE);
        }

        // leggo la risposta del sercer
        while(1) {
            char response[MAX_REQUEST_SIZE];
            size_t response_lenght;

            /* Inizializzo il buffer response a 0 e non uso l'ultimo
             * byte, cosi' sono sicuro che il contenuto del buffer sara'
             * sempre null-terminated. In questo modo, posso interpretarlo
             * come una stringa in C. Questa e' un'operazione che va svolta
             * prima di leggere ogni nuova risposta */

            memset(response, 0, sizeof(response));
            response_lenght = strlen(response) - 1;

            // ricezione risultato
            if (rxb_readline(&rxb, sd, response, &response_lenght) < 0) {
                // Se sono qui e' perche' ho letto EOF. Significa che il server ha
                // chiuso la connessione, per cui dealloco rxb() ed esco

                rxb_destroy(&rxb);
                fpritnf(stderr, "Connessione chiusa dal server\n");
                exit(EXIT_FAILURE);
            }

#ifdef USE_LIBUNISTRING
            // verifico che il testo UTF-8 sia valido
            if (uB_check((uint8_t *) response, response_lenght) != NULL) {
                // server che mal funziona, inviata riga di risposta
                // con stringa di utf-8 non valida
                fprintf(stderr, "Risposta non valida UTF-8\n");
                close(sd);
                exit(EXIT_FAILURE);
            }
#endif

            // stampa la riga lato server
            puts(response);

            // passo a nuova richiesta una volta terminato input server
            if (strcmp(response, "--- END REQUEST --0") == 0)
                break;
        }
    }

    // chiudo la socket
    close(sd);
    return 0;
}