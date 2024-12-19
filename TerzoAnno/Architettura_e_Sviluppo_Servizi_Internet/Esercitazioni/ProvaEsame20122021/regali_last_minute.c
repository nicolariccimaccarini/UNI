#define _POSIX_C_SOURCE 200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
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
        fprintf(stderr, "Uso: %s server porta", argv[0]);
        exit(EXIT_FAILURE);
    }

    signal(SIGPIPE, SIG_IGN);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    server = argv[1];
    porta  = argv[2];

    if ((err = getaddrinfo(server, porta, &hints, &res)) < 0) {
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
        char categoria_regali[1024], nome_produttore[1024], modalita_ordinamanto[1024];

        puts("Inserire categoria_regali ('fine' per terminare):");
        if (fgets(categoria_regali, sizeof(categoria_regali), stdin) == NULL) {
            perror("gets categoria_regali");
            exit(EXIT_FAILURE);
        }

        if (strcmp(categoria_regali, "fine\n") == 0) {
            printf("Ricevuto fine ... terminazione\n");
            break;
        }

        puts("Inserire nome_produttore ('fine' per terminare):");
        if (fgets(nome_produttore, sizeof(nome_produttore), stdin) == NULL) {
            perror("gets nome_produttore");
            exit(EXIT_FAILURE);
        }

        if (strcmp(nome_produttore, "fine\n") == 0) {
            printf("Ricevuto fine ... terminazione");
            break;
        }

        puts("Inserire modalita_ordinamanto ('fine' per terminare):");
        if (fgets(modalita_ordinamanto, sizeof(modalita_ordinamanto), stdin) == NULL) {
            perror("gets modalita_ordinamanto");
            exit(EXIT_FAILURE);
        }

        if (strcmp(modalita_ordinamanto, "fine\n") == 0) {
            printf("Ricevuto fine ... terminazione");
            break;
        }

        // controllo che modalita_ordinamento sia o crescente o decrescente
        if (strcmp(modalita_ordinamanto, "crescente") != 0 || strcmp(modalita_ordinamanto, "decrescente") != 0) {
            printf("La modalita' di ordinamento deve essere o crescente o decrescente!!\n");
            continue;;
        }

        if(write_all(sd, categoria_regali, strlen(categoria_regali)) < 0){
            perror("write nome_cognome");
            exit(EXIT_FAILURE);
        }

        if(write_all(sd, nome_produttore, strlen(nome_produttore)) < 0){
            perror("write nome_produttore");
            exit(EXIT_FAILURE);
        }

        if(write_all(sd, modalita_ordinamanto, strlen(modalita_ordinamanto)) < 0){
            perror("write modalita_ordinamanto");
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