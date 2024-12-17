#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include "rxb.h"
#include "utils.h"
#ifdef USE_LIBUNISTRING
#include <unistring.h>
#endif /* USE_LIBUNISTRING */

#define MAX_REQUEST_SIZE (64 * 1024)

int main(int argc, char** argv) {
    int err, sd;
    char *server, *porta;
    struct addrinfo hints, *ptr, *res;
    rxb_t rxb;

    // Controllo argomenti
    if (argc != 3) {
        fprintf(stderr, "Uso: %s server porta\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    // Ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Preparo hint per getaddrinfo
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    server = argv[1];
    porta  = argv[2];

    if ((err = getaddrinfo(server, porta, &hints, &res)) != 0) {
        fprintf(stderr, "Errore risoluzione nome: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    for (ptr = res; ptr != NULL; ptr = ptr->ai_next) {
        if ((sd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol)) < 0) 
            continue;
        
        if ((err = connect(sd, ptr->ai_addr, ptr->ai_addrlen)) == 0) 
            break;

        // Chiudo la socket
        close(sd);
    }

    // Verifico se la connessione è riuscita
    if (ptr == NULL) {
        fprintf(stderr, "Errore di connessione\n");
        exit(EXIT_FAILURE);
    }

    // Libero la memoria allocata da getaddrinfo
    freeaddrinfo(res);

    // Inizializzo il buffer di ricezione
    rxb_init(&rxb, MAX_REQUEST_SIZE);

    // Ciclo di richieste di servizio
    for (;;) {
        // Leggo sensore, stanza e N
        char sensore[1024], stanza[1024], N[1024];

        puts("Inserisci tipo sensore (fine per terminare):");
        if (fgets(sensore, sizeof(sensore), stdin) == NULL) {
            perror("fgets");
            exit(EXIT_FAILURE);
        }

        if (strcmp(sensore, "fine\n") == 0) 
            break;

        puts("Inserisci stanza sensore (fine per terminare):");
        if (fgets(stanza, sizeof(stanza), stdin) == NULL) {
            perror("fgets");
            exit(EXIT_FAILURE);
        }

        if (strcmp(stanza, "fine\n") == 0) 
            break;

        puts("Inserisci numero letture (fine per terminare):");
        if (fgets(N, sizeof(N), stdin) == NULL) {
            perror("fgets");
            exit(EXIT_FAILURE);
        }

        if (strcmp(N, "fine\n") == 0) 
            break;

        // Invio i dati al server
        if (write_all(sd, sensore, strlen(sensore)) < 0) {
            perror("write_all");
            exit(EXIT_FAILURE);
        }

        if (write_all(sd, stanza, strlen(stanza)) < 0) {
            perror("write_all");
            exit(EXIT_FAILURE);
        }

        if (write_all(sd, N, strlen(N)) < 0) {
            perror("write_all");
            exit(EXIT_FAILURE);
        }

        // Leggo risposta del server
        for (;;) {
            char response_line[1024];
            size_t response_line_len = sizeof(response_line) - 1;

            if (rxb_readline(&rxb, sd, response_line, &response_line_len) < 0) {
                perror("rxb_readline");
                rxb_destroy(&rxb);
                close(sd);
                exit(EXIT_FAILURE);
            }

#ifdef USE_LIBUNISTRING
            if (u8_check((uint8_t *)response_line, response_line_len) != NULL) {
                fprintf(stderr, "Errore: response_line non è una stringa UTF-8\n");
                break;
            }
#endif /* USE_LIBUNISTRING */

            if (strcmp(response_line, "---END RESPONSE---\n") == 0) 
                break;
            
            puts(response_line);
        }
    }

    // Deallocazione buffer di ricezione
    rxb_destroy(&rxb);

    // Chiusura socket
    close(sd);

    return EXIT_SUCCESS;
}