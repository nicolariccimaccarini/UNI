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

// client-td-connreuse hostname porta
//                     argv[1]  argv[2]

int main(int argc, char** argv) {
    int err, sd;
    struct addrinfo hints, *res, *ptr;
    rxb_t rxb;

    // Ignoro SIGPIPE per evitare crash in caso di scrittura su socket chiusa
    signal(SIGPIPE, SIG_IGN);

    // Controllo argomenti
    if (argc != 3) {
        fprintf(stderr, "Uso: %s hostname porta\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    // Inizializzazione di hints
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    // Connessione al server
    if ((err = getaddrinfo(argv[1], argv[2], &hints, &res)) != 0) {
        fprintf(stderr, "Errore in getaddrinfo: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    for(ptr = res; ptr != NULL; ptr = ptr->ai_next) {
        if ((sd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol)) < 0) 
            continue;

        if ((err = connect(sd, ptr->ai_addr, ptr->ai_addrlen)) == 0) 
            break;

        close(sd);
    }

    // Verifico se la connessione e' riuscita
    if (ptr == NULL) {
        fprintf(stderr, "Errore nella connessione\n");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(res);

    // Inizializzazione buffer interno readline
    rxb_init(&rxb, MAX_REQUEST_SIZE);

    // Ciclo di ricezione
    for(;;) {
        // Leggo mese, tipologia e località
        char mese[1024], tipologia[1024], localita[1024]; 

        if(fgets(mese, sizeof(mese), stdin) == NULL) {
            perror("fgets"); 
            exit(EXIT_FAILURE); 
        }

        if(fgets(tipologia, sizeof(tipologia), stdin) == NULL) {
            perror("fgets"); 
            exit(EXIT_FAILURE); 
        }

        if(fgets(localita, sizeof(localita), stdin) == NULL) {
            perror("fgets"); 
            exit(EXIT_FAILURE); 
        }

        if(write_all(sd, mese, sizeof(mese)) < 0) {
            perror("write_all"); 
            exit(EXIT_FAILURE); 
        } 

        if(write_all(sd, tipologia, sizeof(tipologia)) < 0) {
            perror("write_all"); 
            exit(EXIT_FAILURE); 
        } 

        if(write_all(sd, localita, sizeof(localita)) < 0) {
            perror("write_all"); 
            exit(EXIT_FAILURE); 
        }

        // Leggo la response_line
        for (;;) {
            char response_line[1024];
            size_t response_line_len;

            response_line_len = sizeof(response_line);
            err = rxb_readline(&rxb, sd, response_line, &response_line_len);

            if (err < 0) {
                perror("rxb_readline");
                exit(EXIT_FAILURE);
            }

#ifdef USE_LIBUNISTRING
            if (u8_check((uint8_t *)response_line, response_line_len) != NULL) {
                fprintf(stderr, "Errore: la risposta non è UTF-8\n");
                exit(EXIT_FAILURE);
            }
#endif

            if (strcmp(response_line, "---END RESPONSE---") == 0) 
                break;

            puts(response_line);
        }
    }

    // Deallocazione buffer interno readline
    rxb_destroy(&rxb);

    // Chiusura socket
    close(sd);

    return EXIT_SUCCESS;
}