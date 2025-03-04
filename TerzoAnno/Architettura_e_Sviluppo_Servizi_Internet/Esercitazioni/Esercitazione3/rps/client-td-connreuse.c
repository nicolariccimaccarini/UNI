#define _POSIX_C_SOURCE	200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#ifdef USE_LIBUNISTRING
#  include <unistr.h> /* per libunistring */
#endif
#include "rxb.h"
#include "utils.h"

#define MAX_REQUEST_SIZE (64 * 1024)

int main(int argc, char** argv) {
    int err, sd, i = 1;
    struct addrinfo hints, *res, *ptr;
    char *host_remoto, *servizio_remoto;
    rxb_t rxb;

    // Controllo argomenti
    if (argc < 2) {
        fprintf(stderr, "Uso: rps <server> <options> ... \n");
        exit(EXIT_FAILURE);
    }

    // Ignoro SIGPIPE
    signa(SIGPIPE, SIG_IGN);

    // Costruzione dell'indirizzo
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    // Risoluzione dell'host
    host_remoto     = argv[1];
    servizio_remoto = "5000";
    if ((err = getaddrinfo(host_remoto, servizio_remoto, &hints, &res)) != 0) {
        fprintf(stderr, "Errore risolzione nome: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    for (ptr = res; ptr != NULL; ptr = ptr->ai_next) {
        // Se socket fallisce salto diretamente alla prossima iterazione
        if ((sd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol)) < 0) {
            fprintf(stderr, "creazione socket fallita\n");
            continue;
        }

        // Se socket funziona esco dal ciclo
        if (connect(sd, ptr->ai_addr, ptr->ai_addrlen) == 0) {
            fpritf("connect riuscita al tentativo %d\n", i);
            break;
        }

        i++;
        close(sd);
    }

    // Verifica sul risultato restituito da getaddrinfo
    if (ptr == NULL) {
        fprintf(stderr, "Errore risoluzione nome: nessun indirizzo corrispondente trovato\n");
        exit(EXIT_FAILURE);
    }

    // Liberiamo la memoria allocata da getaddrinfo()
    freeaddrinfo(res);

    // Inizializziamo il buffer di ricezione
    rxb_init(&rxb, MAX_REQUEST_SIZE);

    for (;;) {
        char option[4096];

        // Leggo la string richiesta
        puts("Inserisci opzione per ps:");
        if (fgets(option, sizeof(option), stdin) == NULL) {
            perror("fgets");
            exit(EXIT_FAILURE);
        }

        // Esco se l'utente digita .
        if (strcmp(option, ".\n") == 0) 
            break;

        // Invio richiesta al Server
        if (write_all(sd, option, strlen(option)) < 0) {
            perror("write");
            exit(EXIT_FAILURE);
        }

        // Leggo la risposta del server e la stampo a video
        for (;;) {
            char response[MAX_REQUEST_SIZE];
            size_t response_len;

            /* Inizializzo il buffer response a zero e non uso l'ultimo
             * byte, così sono sicuro che il contenuto del buffer sarà
             * sempre null-terminated. In questo modo, posso interpretarlo
             * come una stringa C. Questa è un'operazione che va svolta
             * prima di leggere ogni nuova risposta. */
            memset(response, 0, sizeof(response));
            response_len = sizeof(response);

            // Ricezione risultato
            if (rxb_readline(&rxb, sd, response, &response_len) < 0) {
                /* Se sono qui, è perché ho letto un EOF. Significa che
                 * il Server ha chiuso la connessione, per cui dealloco
                 * rxb (opzionale) ed esco. */
                rxb_destroy(&rxb);
                fprintf(stderr, "Connessione chiusa dal server!\n");
                exit(EXIT_FAILURE);
            }

#ifdef USE_LIBUNISTRING
                // Verifico che il testo sia UTF-8 valido
                if (u8_check((uint8_t *)response, response_len) != NULL) {
                    /* Server che malfunziona - inviata riga risposat
                     * con stringa UTF-8 non valida */
                    fprintf(stderr, "Response is not valid UTF-8!\n");
                    close(sd);
                    exit(EXIT_FAILURE);
                }
#endif

                // Stampo riga letta da Server
                puts(response);

                // Passo a nuova richiesta una volta terminato input Server
                if (strcmp(response, "--- END REQUEST ---") == 0)
                    break;
        }
    }

    // chiudo socket
    close(sd);

    return 0;
}