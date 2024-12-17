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

int autorizza(const char *username, const char *password) {
    return 1;
}

int main(int argc, char** argv) {
    int err, sd, on;
    char *porta;
    struct addrinfo hints, *res;
    struct sigaction sa;

    // Controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Uso %s server porta\n", argv[0]);
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

    // Chiamo listen
    if ((err = listen(sd, SOMAXCONN)) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
    
    freeaddrinfo(res);

    for (;;) {
        int ns;
        pid_t pid_child;

        if ((ns = accept(sd, NULL, NULL)) < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        if ((pid_child = fork()) < 0) {
            perror("fork pid_child");
            exit(EXIT_FAILURE);
        } else if (pid_child == 0) {
            // FIGLIO
            rxb_t rxb;

            // Inizializzo buffer di ricezione
            rxb_init(&rxb, MAX_REQUEST_SIZE);

            // Reinstallo gestore SIGCHLD
            // Reinstallo gestore SIGCHLD
            memset(&sa, 0, sizeof(sa));
            sa.sa_handler = sigchld_handler;
            sa.sa_flags   = SA_RESTART;
            sigemptyset(&sa.sa_mask);

            if ((err = sigaction(SIGCHLD, &sa, NULL)) < 0) {
                perror ("sigaction");
                exit(EXIT_FAILURE);
            }

            // Ciclo for di richieste
            for (;;) {
                char username[1024], password[1024], tipologia[1024];
                size_t username_len, password_len, tipologia_len;
                char filename[PATH_MAX];
                pid_t pid_n1, pid_n2;
                int pipe_n1n2[2];

                char *stringa_terminazione = "--- END REQUEST ---\n";
                int status;

                // Leggo username
                memset(username, 0, sizeof(username));
                username_len = sizeof(username) - 1;

                if (rxb_readline(&rxb, ns, username, &username_len) < 0) {
                    perror("rxb_readline username");
                    exit(EXIT_FAILURE);
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)username, username_len) != NULL) {
                    fprintf(stderr, "Errore: username non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo password
                memset(password, 0, sizeof(password));
                password_len = sizeof(password) - 1;

                if (rxb_readline(&rxb, ns, password, &password_len) < 0) {
                    perror("rxb_readline password");
                    exit(EXIT_FAILURE);
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)password, password_len) != NULL) {
                    fprintf(stderr, "Errore: password non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo tipologia
                memset(tipologia, 0, sizeof(tipologia));
                tipologia_len = sizeof(tipologia) - 1;

                if (rxb_readline(&rxb, ns, tipologia, &tipologia_len) < 0) {
                    perror("rxb_readline tipologia");
                    exit(EXIT_FAILURE);
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)tipologia, tipologia_len) != NULL) {
                    fprintf(stderr, "Errore: tipologia non e' una stringa UTF-8\n");
                    break;
                }
#endif

                if (autorizza(username, password) != 1) {
                    char *unauthorized = "Non autorizzato!\n";
                    write_all(ns, unauthorized, strlen(unauthorized));
                    write_all(ns, stringa_terminazione, strlen(stringa_terminazione));
                    continue;
                }

                snprintf(filename, sizeof(filename), "./toys/%s.txt", tipologia);

                // Creazione pipe
                if (pipe(pipe_n1n2) < 0) {
                    perror("pipe_n2n1");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n1 = fork()) < 0) {
                    perror("fork pid_n1");
                    exit(EXIT_FAILURE);
                } else if (pid_n1 == 0) {
                    // NIPOTE 1

                    // grep "disponibile" filename
                    // Redirezione output su pipe
                    close(1);
                    if (dup(pipe_n1n2[1]) < 0) {
                        perror("dup pipe_n1n2[1]");
                        exit(EXIT_FAILURE);
                    }

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("grep", "grep", "disponibile", filename, (char *)NULL);
                    perror("execlp grep");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n2 = fork()) < 0) {
                    perror("fork pid_n2");
                    exit(EXIT_FAILURE);
                } else if (pid_n2 == 0) {
                    // NIPOTE 2

                    // sort -n
                    // Redirezione input da pipe
                    close(0);
                    if (dup(pipe_n1n2[0]) < 0) {
                        perror("dup pipe_n1n2[0]");
                        exit(EXIT_FAILURE);
                    }

                    // Redirigo output su socket
                    close(1);
                    if (dup(ns) < 0) {
                        perror("dup ns");
                        exit(EXIT_FAILURE);
                    }

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[1]);
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("sort", "sort", "-n", (char *)NULL);
                    perror("execlp sort");
                    exit(EXIT_FAILURE);
                }

                // FIGLIO

                // Attesa temrinazione dei nipoti
                waitpid(pid_n1, &status, 0);
                waitpid(pid_n2, &status, 0);

                // Scrivo messaggio fine risposta
                if (write_all(ns, stringa_terminazione, strlen(stringa_terminazione)) < 0) {
                    perror("write_all stringa_terminazione");
                    exit(EXIT_FAILURE);
                }
            }

            // Deinizializzo rxb
            rxb_destroy(&rxb);

            // Chiusura socket
            close(ns);

            exit(EXIT_SUCCESS);
        }

        // PADRE
        close(ns);
    }

    // Chiusura socket passiva
    close(sd);

    return EXIT_SUCCESS;
}