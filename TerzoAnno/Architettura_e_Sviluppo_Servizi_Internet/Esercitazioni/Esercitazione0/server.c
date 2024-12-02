#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include "rxb.h"

/* Assunzione di lavoro 1: massima dimensione di una richiesta è 16 KiB */
#define MAX_REQUEST_SIZE (16 * 1024)
/* Assunzione di lavoro 2: massima dimensione di una riga è 4 KiB */
#define MAX_LINE_LENGTH (4095)

/*
 * server  port
 * argv[0] argv[1]
 */
int main(int argc, char *argv[]) {
    int err, sd, opt;
    struct addrinfo hints, *res;

    /* Controllo degli argomenti */
    if (argc != 2) {
        fprintf(stderr, "Usage: %s port\n", argv[0]);
        return EXIT_FAILURE;
    }

    /* Inizializzazione di hints */
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    /* Chiamo getaddrinfo */
    err = getaddrinfo(NULL, argv[1], &hints, &res);
    if (err != 0) {
        fprintf(stderr, "Error (getaddrinfo): %s\n", gai_strerror(err));
        return EXIT_FAILURE;
    }

    sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sd < 0) {
        perror("socket");
        /* fprintf(stderr, "Error (socket): %s\n", strerror(errno)); */
        return EXIT_FAILURE;
    }

    opt = 1;
    err = setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    if (err < 0) {
        perror("setsockopt");
        return EXIT_FAILURE;
    }

    err = bind(sd, res->ai_addr, res->ai_addrlen);
    if (err < 0) {
        perror("bind");
        return EXIT_FAILURE;
    }

    err = listen(sd, SOMAXCONN);
    if (err < 0) {
        perror("listen");
        return EXIT_FAILURE;
    }

    for (;;) {
        int ns, pid, pid_head;
        char filename[2048];
        size_t filename_len;
        rxb_t rxb_client;

        /* Attendo richiesta di connessione dal Client */
        ns = accept(sd, NULL, NULL);
        if (ns < 0) {
            /*
            if (errno = EINTR)
                continue;
            */
            perror("accept");
            return EXIT_FAILURE;
        }

        /* Server concorrente */
        pid = fork();
        if (pid < 0) {
            perror("fork");
            return EXIT_FAILURE;
        } else if(pid == 0) {
            /* Processo figlio */

            /* Chiudo la socket passiva */
            close(sd);

            /* Inizio servizio */

            /* Inizializzo buffer readline dal Client */
            rxb_init(&rxb_client, MAX_REQUEST_SIZE * 2);

            /* Inizializzo buffer filename */
            memset(filename, 0, sizeof(filename));
            /* memset(&filename[0], 0, sizeof(filename)); */
            filename_len = sizeof(filename) - 1;

            /* Leggo filename */
            err = rxb_readline(&rxb_client, ns, filename, &filename_len);
            if (err < 0) {
                perror("rxb_readline");
                return EXIT_FAILURE;
            }
            
            pid_head = fork();
            if (pid_head < 0) {
                perror("fork");
                return EXIT_FAILURE;
            } else if(pid_head == 0) {
                /* Processo figlio */

                /* Redirigo stdout su socket */
                close(1);
                if (dup(ns) < 0) {
                    perror("dup");
                    return EXIT_FAILURE;
                }
                close(ns);

                /* Lancio processo head */
                execlp("head", "head", "-n", "5", filename, (char *)NULL);
                perror("execlp");
                return EXIT_FAILURE;
            }

            /* Deinizializzo il buffer della readline */
            rxb_destroy(&rxb_client);

            /* Fine servizio */
        }

        /* Padre */
        close(ns);
    }

    close(sd);

    return EXIT_SUCCESS;
}