#define _POSIX_C_SOURCE	200809L
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
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
        fprintf(stderr, "Uso server porta\n");
        exit(EXIT_FAILURE);
    }

    porta = argv[1];

    // Ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Installo gestore SIGCHLD
    sa.sa_handler = sigchld_handler;
    sa.sa_flags   = SA_RESTART;
    sigemptyset(&sa.sa_mask);

    if (sigaction(SIGCHLD, &sa, NULL) < 0) {
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
    if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    // Chiamo il bind
    if (bind(sd, res->ai_addr, res->ai_addrlen) < 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    }

    // Chiamo listen
    if (listen(sd, SOMAXCONN) < 0) {
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

            // Inizializzazione buffer di ricezione
            rxb_init(&rxb, MAX_REQUEST_SIZE);

            // Reinstallo gestore SIGCHLD
            memset(&sa, 0, sizeof(sa));
            sigemptyset(&sa.sa_mask);
            sa.sa_handler = SIG_DFL;

            if (sigaction(SIGCHLD, &sa, NULL) < 0) {
                perror ("sigaction");
                exit(EXIT_FAILURE);
            }

            // Ciclo for di richieste
            for (;;) {
                char username[MAX_REQUEST_SIZE], password[MAX_REQUEST_SIZE], nome_cognome_artista[MAX_REQUEST_SIZE];
                size_t username_len, password_len, nome_cognome_artista_len;
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
                    rxb_destroy(&rxb);
                    close(ns);
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
                    rxb_destroy(&rxb);
                    close(ns);
                    exit(EXIT_FAILURE);
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)password, password_len) != NULL) {
                    fprintf(stderr, "Errore: password non e' una stringa UTF-8\n");
                    break;
                }
#endif

                // Leggo nome_cognome_artista
                memset(nome_cognome_artista, 0, sizeof(nome_cognome_artista));
                nome_cognome_artista_len = sizeof(nome_cognome_artista) - 1;

                if (rxb_readline(&rxb, ns, nome_cognome_artista, &nome_cognome_artista_len) < 0) {
                    perror("rxb_readline nome_cognome_artista");
                    rxb_destroy(&rxb);
                    close(ns);
                    exit(EXIT_FAILURE);
                }

#ifdef USE_LIBUNISTRING
                if (u8_check((uint8_t *)nome_cognome_artista, nome_cognome_artista_len) != NULL) {
                    fprintf(stderr, "Errore: nome_cognome_artista non e' una stringa UTF-8\n");
                    break;
                }
#endif

                if (autorizza(username, password) != 1) {
                    char *unauthorized = "Non autorizzato!\n";
                    write_all(ns, unauthorized, strlen(unauthorized));
                    write_all(ns, stringa_terminazione, strlen(stringa_terminazione));
                    continue;
                }

                snprintf(filename, sizeof(filename), "./music/album_database.txt");

                // Controllo apertura del file
                if (open(filename, O_RDONLY) < 0) {
                    fprintf(stderr, "Errore apertura file\n");
                    if (errno == ENOENT) {
                        fprintf(stderr, "Il file non esiste\n");
                        break;
                    }
                    break;
                }

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

                    // grep nome_cognome_artista filename
                    close(1);
                    if (dup(pipe_n1n2[1]) < 0) {
                        perror("dup pipe_n1n2[1]");
                        exit(EXIT_FAILURE);
                    }
                    close(pipe_n1n2[1]);

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[0]);
                    close(ns);

                    execlp("grep", "grep", nome_cognome_artista, filename, (char *)NULL);
                    perror("execlp grep");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n2 = fork()) < 0) {
                    perror("fork pid_n2");
                    exit(EXIT_FAILURE);
                } else if (pid_n2 == 0) {
                    // NIPOTE 2

                    // sort -r -n
                    // Redirezione input da pipe
                    close(0);
                    if (dup(pipe_n1n2[0]) < 0) {
                        perror("dup pipe_n1n2[0]");
                        exit(EXIT_FAILURE);
                    }
                    close(pipe_n1n2[0]);

                    // Redirigo output su socket
                    close(1);
                    if (dup(ns) < 0) {
                        perror("dup ns");
                        exit(EXIT_FAILURE);
                    }

                    // Chiusura file descriptor non utilizzati
                    close(pipe_n1n2[0]);
                    close(pipe_n1n2[1]);
                    close(ns);

                    execlp("sort", "sort", "-r", "-n", (char *)NULL);
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