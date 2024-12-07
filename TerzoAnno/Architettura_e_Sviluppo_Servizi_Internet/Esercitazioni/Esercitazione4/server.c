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

// gestore del segnale SIGCHLD
void handler(int signo) {
    int status;
    (void) signo;

    // gestisco tutti i figli terminati
    while (waitpid(-1, &status, WNOHANG) < 0) {
        continue;
    }
}

int main(int argc, char **argv) {
    struct addrinfo hints, *res;
    int err, sd, ns, pid, on;
    struct sigaction sa;

    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso: $s porta\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    // ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Installo gestore SIGCHLD usando la syscall sigaction
    sigemptyset(&sa.sa_mask);
    sa.sa_flags   = SA_RESTART;
    sa.sa_handler = handler;

    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    if ((err = gettaddrinfo(NULL, argv[1], &hints, &res)) != 0) {
        fprintf(stderr, "Errore setup indirozzo bind: %s\n", gai_strerror);
        exit(EXIT_FAILURE);
    }

    if ((sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) < 0) {
        perror("Errore in socket");
        exit(EXIT_FAILURE);
    }

    on = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    if (bind(sd, res->ai_addr, res->ai_addrlen) < 0) {
        perror("Errore in bind");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(res);

    if (listen(sd, SOMAXCONN) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    while(1) {
        printf("Server in ascolto...\n");

        if ((ns = accept(sd, NULL, NULL)) < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        // generazione figlio
        pid = fork();
        if (pid < 0) {
            perror("prima fork");
            exit(EXIT_FAILURE);
        } else if (pid == 0) {
            // figlio UNO
            int status, pid2;
            const char *end_request = "\n--- END REQUEST ---\n";
            rxb_t rxb;
            char categoria[MAX_REQUEST_SIZE];

            // figlio chiude socket
            close(sd);

            // disabilito gestore SIGCHLD
            memset(&sa, 0, sizeof(sa));
            sigemptyset(&sa.sa_mask);
            sa.sa_handler = SIG_DFL;

            if (sigaction(SIGCHLD, &sa, NULL) == -1) {
                perror("sigaction");
                exit(EXIT_FAILURE);
            }

            // inizializzo buffer di ricezione
            rxb_init(&rxb, MAX_REQUEST_SIZE);

            // avvio della gestione delle richieste
            while (1) {
                size_t categoria_len;

                memset(categoria, 0, sizeof(categoria));
                categoria_len = strlen(categoria);

                // leggo la richiesta del client
                err = rxb_readline(&rxb, ns, categoria, &categoria_len);
                if (err < 0) {
                    rxb_destroy(&rxb);
                    break;
                }

#ifdef USE_LIBUNISTRING
                // verifica messaggio UTF-8
                if (u8_check(((u_int8_t *) categoria, categoria_len)) != 0) {
                    fprintf(stderr, "Richiesta non valida in UTF-8\n");
                    close(ns);
                    exit(EXIT_FAILURE);
                }
#endif
                // creo unu nipote a cui demandero' la grep
                pid2 = fork();
                if (pid2 < 0) {
                    perror("Secondo figlio");
                    exit(EXIT_FAILURE);
                } else if (pid2 == 0) {
                    // figlio due
                    // Ridirigo stdout

                    close(STDOUT_FILENO);
                    if (dup(ns) < 0) {
                        perror("dup");
                        exit(EXIT_FAILURE);
                    }

                    close(ns);

                    // eseguo la grep sul server
                    execlp("grep", "grep", categoria, "./local/conto_corrente.txt", (char *) NULL);
                    perror("exec grep");
                    exit(EXIT_FAILURE);
                }

                // asoetto che il figlio termini
                wait(&status);
            }
        }
    }
}