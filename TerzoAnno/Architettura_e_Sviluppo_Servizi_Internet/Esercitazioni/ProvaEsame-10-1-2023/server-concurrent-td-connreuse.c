#define _POSIX_C_SOURCE	200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
#include <sys/wait.h>
#ifdef USE_LIBUNISTRING
#include <unistr.h> 
#endif
#include "rxb.h"
#include "utils.h"

#define MAX_REQUEST_SIZE (64 * 1024)

void sigchld_handler(int signo) {
    int status;
    (void)signo;

    // Deallocazione di tutti i figli terminati (con wait dealochermo solo le risorse del primo)
    while (waitpid(-1, &status, WNOHANG) > 0)
        continue;
}

int main(int argc, char** argv) {
    int err, sd, on;
    char *porta;
    struct addrinfo hints, *res;
    struct sigaction sa;

    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso: %s server porta\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    porta = argv[1];

    // Ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Installo gestore SIGCHLD
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sigchld_handler;
    sa.sa_flags   = SA_RESTART;
    sigemptyset(&sa.sa_mask);

    if ((err = sigaction(SIGCHLD, &sa, NULL)) < 0) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    // Inizializzazione di hints
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    // Chiamo getaddrinfo
    if ((err = getaddrinfo(NULL, porta, &hints, &res)) < 0) {
        fprintf(stderr, "Errore in getaddrinfo: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    // Creo socket
    if ((sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) < 0) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    // Disattivo TIME_WAIT
    on = 1;
    if ((err = setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on))) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    // Chiamo il bind
    if ((err = bind(sd, res->ai_addr, res->ai_addrlen)) < 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(res);

    // Chiamo listen
    if ((err = listen(sd, SOMAXCONN)) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    for (;;) {
        int ns;
        pid_t pid_child;

        if ((ns = accept(sd, NULL, NULL)) < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        if ((pid_child = fork()) < 0) {
            perror("fork");
            exit(EXIT_FAILURE);
        } else if (pid_child == 0) {
            // FIGLIO
            rxb_t rxb;

            // Chiudo socket passiva
            close(sd);

            // Inizializzo buffer rxb_readline
            rxb_init(&rxb, MAX_REQUEST_SIZE);

            // Reinstallo gestore SIGCHLD  
            memset(&sa, 0, sizeof(sa));
            sa.sa_handler = sigchld_handler;
            sa.sa_flags   = SA_RESTART;
            sigemptyset(&sa.sa_mask);

            if ((err = sigaction(SIGCHLD, &sa, NULL)) < 0) {
                perror("sigaction");
                exit(EXIT_FAILURE);
            }

            // Ciclo for di richieste
            for (;;) {
                char sensore[1024], stanza[1024], N[1024];
                size_t sensore_len, stanza_len, N_len;
                char filename[PATH_MAX];    
                pid_t pid_n1, pid_n2;
                int pipe_n1n2[2];

                char *stringa_terminazione = "---END RESPONSE---\n";
                int status;

                memset(sensore, 0, sizeof(sensore));
                sensore_len = sizeof(sensore) - 1;

                // Leggo sensore
                if (rxb_readline(&rxb, ns, sensore, &sensore_len) < 0) {
                    perror("rxb_readline");
                    break;
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)sensore, sensore_len) != NULL) {
                    fprintf(stderr, "Errore: sensore non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo stanza
                memset(stanza, 0, sizeof(stanza));
                stanza_len = sizeof(stanza) - 1;

                if (rxb_readline(&rxb, ns, stanza, &stanza_len) < 0) {
                    perror("rxb_readline");
                    break;
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)stanza, stanza_len) != NULL) {
                    fprintf(stderr, "Errore: stanza non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo N
                memset(N, 0, sizeof(N));
                N_len = sizeof(N) - 1;

                if (rxb_readline(&rxb, ns, N, &N_len) < 0) {
                    perror("rxb_readline");
                    break;
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)N, N_len) != NULL) {
                    fprintf(stderr, "Errore: N non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Preparazione risposta
                snprintf(filename, sizeof(filename), "./local/%s.txt", stanza);

                // Creazione pipe
                if (pipe(pipe_n1n2) < 0) {
                    perror("pipe");
                    break;
                }
                
                if ((pid_n1 = fork()) < 0) {
                    perror("fork nipote 1");
                    exit(EXIT_FAILURE);
                } else if (pid_n1 == 0) {
                    // NIPOTE 1

                    // grep sensore filename
                    // Redirezione output su pipe
                    close(1);
                    if (dup(pipe_n1n2[1]) < 0) {
                        perror("dup");
                        exit(EXIT_FAILURE);
                    }

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("grep", "grep", sensore, filename, (char *)NULL);
                    perror("execlp grep");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n2 = fork()) < 0) {
                    perror("fork nipote 2");
                    exit(EXIT_FAILURE);
                } else if (pid_n2 == 0) {
                    // NIPOTE 2

                    // head -n
                    // Redirezione input da pipe
                    close(0);
                    if (dup(pipe_n1n2[0])) {
                        perror("dup");
                        exit(EXIT_FAILURE);
                    }

                    // Redirigo output su socket
                    close(1);
                    if (dup(ns)) {
                        perror("dup");
                        exit(EXIT_FAILURE);
                    }

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[1]);
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("head", "head", "-n", N, (char *)NULL);
                    perror("execlp head");
                    exit(EXIT_FAILURE);
                }

                // FIGLIO

                // Attesa terminazione dei nipooti
                waitpid(pid_n1, &status, 0);
                waitpid(pid_n2, &status, 0);

                // Scrivo messaggio fine risposta
                if (write_all(ns, stringa_terminazione, strlen(stringa_terminazione)) < 0) {
                    perror("write_all");
                    break;
                }
            }

            // Deinizializza rxb
            rxb_destroy(&rxb);

            // Chiusura socket attiva
            close(ns);

            return EXIT_SUCCESS;
        }

        // PADRE
        close(ns);
    }

    // Chiudo socket passiva
    close(sd);

    return EXIT_SUCCESS;
}