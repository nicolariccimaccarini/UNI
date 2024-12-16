#define _POSIX_C_SOURCE	200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
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
    struct addrinfo hints, *res;
    struct sigaction sa;

    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso: %s hostname porta\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    // Ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Installo gestore SIGCHLD
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sigchld_handler;
    sa.sa_flags = SA_RESTART;
    sigemptyset(&sa.sa_mask);

    if ((err = sigaction(SIGCHLD, &sa, NULL)) < 0) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    // Inizializzazione di hints
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    // Chiamo getaddrinfo
    if ((err = getaddrinfo(argv[1], argv[2], &hints, &res)) != 0) {
        fprintf(stderr, "Errore in getaddrinfo: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    // Creo socket
    if ((sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) < 0) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    // Disabilito TIME_WAIT
    on = 1;
    if ((err = setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on))) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    // Chiamo bind
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
        // Chiamo accept
        int ns = accept(sd, NULL, NULL);
        if (ns < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        pid_t pid_child = fork();
        if (pid_child < 0) {
            perror("fork");
            exit(EXIT_FAILURE);
        } else if (pid_child == 0) {
            // FIGLIO
            rxb_t rxb;

            // Chiudo socket passiva
            close(sd);

            // Inizializzazione buffer rxb_readline
            rxb_init(&rxb, MAX_REQUEST_SIZE);

            // Reinstallo gestore SIGCHLD
            memset(&sa, 0, sizeof(sa));
            sa.sa_handler = SIG_DFL;
            sa.sa_flags = SA_RESTART;
            sigemptyset(&sa.sa_mask);

            if ((err = sigaction(SIGCHLD, &sa, NULL)) < 0) {
                perror("sigaction");
                exit(EXIT_FAILURE);
            }

            // Ciclo for di richieste
            for (;;) {
                char mese[1025], tipologia[1025], localita[1025];
                size_t mese_len, tipologia_len, localita_len;
                char filename[PATH_MAX];    
                pid_t pid_n1, pid_n2;
                int pipe_n1n2[2];

                memset(mese, 0, sizeof(mese));
                mese_len = sizeof(mese) - 1;

                // Leggo mese
                if (rxb_readline(&rxb, ns, mese, &mese_len) < 0) {
                    perror("rxb_readline");
                    break;
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)mese, mese_len) != NULL) {
                    fprintf(stderr, "Errore: mese non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo tipologia
                if (rxb_readline(&rxb, ns, tipologia, &tipologia_len) < 0) {
                    perror("rxb_readline");
                    break;
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)tipologia, tipologia_len) != NULL) {
                    fprintf(stderr, "Errore: tipologia non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo localita
                if (rxb_readline(&rxb, ns, localita, &localita_len) < 0) {
                    perror("rxb_readline");
                    break;
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)localita, localita_len) != NULL) {
                    fprintf(stderr, "Errore: localita non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Preparazione risposta
                snprintf(filename, sizeof(filename), "/var/local/holidays/%s/%s.txt", tipologia, mese);

                if ((pid_n1 = fork()) < 0) {
                    perror("fork");
                    break;
                } else if (pid_n1 == 0) {
                    // NIPOTE 1

                    // gerp localita filename
                    // Redirezione output su pipe
                    cloe(1);
                    dup(pipe_n1n2[1]);

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("grep", "grep", localita, filename, (char *)NULL);
                    perror("execlp grep");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n2 = fork()) < 0) {
                    perror("fork");
                    break;
                } else if (pid_n2 == 0) {
                    // NIPOTE 2

                    // sort -r -n 
                    // Redirezione input da pipe
                    close(0);
                    dup(pipe_n1n2[0]);

                    // Redirigo output su socket
                    close(1);
                    dup(ns);

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[1]);
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("sort", "sort", "-r", "-n", (char *)NULL);
                    perror("execlp sort");
                    exit(EXIT_FAILURE);
                }

                // FIGLIO
                char stringa_terminazione = "---END RESPONSE---\n";

                // Attesa terminazione nipoti
                waitpid(pid_n1, NULL, 0);
                waitpid(pid_n2, NULL, 0);

                // Scrivo messaggio fine risposta
                if (write_all(ns, stringa_terminazione, strlen(stringa_terminazione)) < 0) {
                    perror("write_all");
                    break;
                }
            }

            // Deinizializzo rxb
            rxb_destroy(&rxb);   
            
            // Chiusura socket attiva
            close(ns);
            
            return EXIT_SUCCESS;        
        }

        // PADRE
    }

    exit(EXIT_SUCCESS);
}