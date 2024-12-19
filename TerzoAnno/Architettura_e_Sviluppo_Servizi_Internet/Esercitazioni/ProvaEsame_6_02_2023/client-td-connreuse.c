#define _POSIX_C_SOURCE 200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#ifdef USE_LIBUNISTRNG
#include <unistr.h>
#endif
#include "rxb.h"
#include "utils.h"

#define MAX_REQUEST_SIZE (64*1024)

int main(int argc, char** argv) {
    int err, sd, i = 1;
    struct addrinfo hints, *res, *ptr;
    rxb_t rxb;
    char *server, *porta;

    if (argc != 3) {
        fprintf(stderr, "Uso: %s server porta", argv[1]);
        exit(EXIT_FAILURE);
    }

    signal(SIGPIPE, SIG_IGN);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    server = argv[1];
    porta  = argv[2];

    if ((err = getaddrinfo(server, porta, &hints, &res)) != 0) {
        fprintf(stderr, "Errore risoluzione indirizzo %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    for (ptr = res; ptr != NULL; ptr = ptr->ai_next) {
        if ((sd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol)) < 0) {
            fprintf(stderr, "Creazione socket fallita\n");
            continue;
        }

        if (connect(sd, ptr->ai_addr, ptr->ai_addrlen) == 0) {
            printf("Connessione riuscita al tentativo %d\n", i);
            break;
        }

        i++;
        close(sd);
    }

    if (ptr == NULL) {
        fprintf(stderr, "Errore connessione");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(res);
    rxb_init(&rxb, MAX_REQUEST_SIZE);

    for (;;) {
        char nome_giocatore[MAX_REQUEST_SIZE], squadra[MAX_REQUEST_SIZE], edizione[MAX_REQUEST_SIZE];

        puts("Inserire il nome e cognome del giocatore ('fine' per terminare):");
        if (fgets(nome_giocatore, sizeof(nome_giocatore), stdin) == NULL) {
            perror("gets nome_cognome");
            exit(EXIT_FAILURE);
        }

        if (strcmp(nome_giocatore, "fine\n") == 0) {
            printf("Ricevuto fine ... terminazione\n");
            break;
        }

        puts("Inserire la squadra del giocatore ('fine' per terminare):");
        if (fgets(squadra, sizeof(squadra), stdin) == NULL) {
            perror("gets squadra");
            exit(EXIT_FAILURE);
        }

        if (strcmp(squadra, "fine\n") == 0) {
            printf("Ricevuto fine ... terminazione");
            break;
        }

        puts("Inserire l'edizione del torneo ('fine' per terminare):");
        if (fgets(edizione, sizeof(edizione), stdin) == NULL) {
            perror("gets edizione");
            exit(EXIT_FAILURE);
        }

        if (strcmp(edizione, "fine\n") == 0) {
            printf("Ricevuto fine ... terminazione");
            break;
        }

        if(write_all(sd, nome_giocatore, strlen(nome_giocatore)) < 0){
            perror("write nome_cognome");
            exit(EXIT_FAILURE);
        }

        if(write_all(sd, squadra, strlen(squadra)) < 0){
            perror("write squadra");
            exit(EXIT_FAILURE);
        }

        if(write_all(sd, edizione, strlen(edizione)) < 0){
            perror("write edizione");
            exit(EXIT_FAILURE);
        }

        for (;;) {
            char response[MAX_REQUEST_SIZE];
            size_t response_len;

            memset(response, 0, sizeof(response));
            response_len = sizeof(response) - 1;

            if (rxb_readline(&rxb, sd, response, &response_len) < 0) {
                fprintf(stderr, "Connessione chiusa dal server");
                rxb_destroy(&rxb);
                close(sd);
                exit(EXIT_FAILURE);
            }

#ifdef USE_LIBUNISTRNG  
            if((u_8check(uint8_t *)response,response_len)!=NULL){
                fprintf(stderr,"risposta non in utf-8");
                close(sd);
                exit(EXIT_FAILURE);
            }
#endif

            puts(response);
            if (strcmp(response, "--- END REQUEST ---") == 0) 
                break;
        
        }
    }

    close(sd);
    return EXIT_SUCCESS;
}