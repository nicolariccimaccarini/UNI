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

#define MAX_REQUEST_SIZE (64*1024)

int main(int argc, char** argv) {
    int err, sd, i = 1;
    struct addrinfo hints, *ptr, *res;
    char *server, *porta;
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
        fprintf(stderr, "Errore risoluzione nome %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    // Connessione al server
    for (ptr = res; ptr != NULL; ptr = ptr->ai_next) {
        if ((sd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol)) < 0) {
            fprintf(stderr, "creazione socket fallita\n");
            continue;
        }

        if ((err = connect(sd, ptr->ai_addr, ptr->ai_addrlen)) == 0) {
            printf("connect riuscita al tentativo %d\n", i);
            break;
        }

        i++;
        close(sd);
    }

    // Verifico che la connessione e' riuscita
    if (ptr == NULL) {
        fprintf(stderr, "Errore di connessione\n");
        exit(EXIT_FAILURE);
    }

    // Libero la memoria allocata da getaddrinfo
    freeaddrinfo(res);

    // Inizializzazione buffer di ricezione
    rxb_init(&rxb, MAX_REQUEST_SIZE);

    // Ciclo di richieste di servizio
    for (;;) {
        char username[MAX_REQUEST_SIZE], password[MAX_REQUEST_SIZE], nome_cognome_artista[MAX_REQUEST_SIZE];

        // Leggo username
        puts("Inserire username ('fine' per terminare)");
        if (fgets(username, sizeof(username), stdin) == NULL) {
            perror("fgets username");
            exit(EXIT_FAILURE);
        }

        if (strcmp(username, "fine\n") == 0)
            break;

        // Leggo password
        puts("Inserire password ('fine' per terminare)");
        if (fgets(password, sizeof(password), stdin) == NULL) {
            perror("fgets password");
            exit(EXIT_FAILURE);
        }

        if (strcmp(password, "fine\n") == 0)
            break;

        // Leggo nome_cognome_artista
        puts("Inserire nome e cognome dell'artista che si vuole cercare ('fine' per terminare)");
        if (fgets(nome_cognome_artista, sizeof(nome_cognome_artista), stdin) == NULL) {
            perror("fgets nome_cognome_artista");
            exit(EXIT_FAILURE);
        }

        if (strcmp(nome_cognome_artista, "fine\n") == 0)
            break;

        // Invio dati al server
        if (write_all(sd, username, strlen(username)) < 0) {
            perror("write username");
            exit(EXIT_FAILURE);
        }

        if (write_all(sd, password, strlen(password)) < 0) {
            perror("write password");
            exit(EXIT_FAILURE);
        }

        if (write_all(sd, nome_cognome_artista, strlen(nome_cognome_artista)) < 0) {
            perror("write nome_cognome_artista");
            exit(EXIT_FAILURE);
        }

        // Leggo risposta del server
        for (;;) {
            char response_line[MAX_REQUEST_SIZE];
            size_t response_line_len;

            memset(response_line, 0, sizeof(response_line));
            response_line_len = sizeof(response_line) - 1;

            if (rxb_readline(&rxb, sd, response_line, &response_line_len) < 0) {
                perror("rxb_readline response_line");
                rxb_destroy(&rxb);
                close(sd);
                exit(EXIT_FAILURE);
            }

#ifdef USE_LIBUNISTRING
            if (u8_check((uint8_t *)response_line, response_line_len) != NULL) {
                fprintf(stderr, "Errore: response_line non è una stringa UTF-8\n");
                rxb_destroy(&rxb);
                close(sd);
                break;
            }
#endif /* USE_LIBUNISTRING */

            puts(response_line);
            if (strcmp(response_line, "--- END REQUEST ---\n") == 0)
                break;
        }
    }

    // Deallocazione buffer di ricezione
    rxb_destroy(&rxb);

    // Chiusura socket
    close(sd);

    return EXIT_SUCCESS;
}