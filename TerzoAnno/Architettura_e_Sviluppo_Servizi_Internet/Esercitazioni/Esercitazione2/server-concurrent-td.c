#define _POSIX_C_SOURCE 200809L
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#ifdef USE_LIBUNISTRING
#  include <unistr.h> /* per libunistring */
#endif
#include "rxb.h"
#include "utils.h"

/* La massima dimensione di una richiesta è di 64KiB */
#define MAX_REQUEST_SIZE (64 * 1024)

// Gestore del segnale SIGCHLD
void handler(int signo) {
    int status;
    (void)signo;

    while (waitpid(-1, &status, WNOHANG) > 0)
        continue;
}

int main(int argc, char **argv) {
    int sd, err, on;
    struct addrinfo hints, *res;
    struct sigaction sa;

    // controllo argomenti
    if (argc != 2) {
        fprintf(stderr, "Sintassi: %s porta\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    /* Installo gestore SIGCHLD usando la syscall sigaction, che è uno
	 * standard POSIX, al posto di signal che ha semantiche diverse a
	 * seconda del sistema operativo */
    sigemptyset(&sa.sa_mask);
    /* uso SA_RESTART per evitare di dover controllare esplicitamente se
     * accept è stata interrotta da un segnale e in tal caso rilanciarla
     * (si veda il paragrafo 21.5 del testo M. Kerrisk, "The Linux
     * Programming Interface") */
    sa.sa_flags   = SA_RESTART;
    sa.sa_handler = handler;

    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    // ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // preparo le direttive per addrinfo
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    // uso getaddrinfo per preparare le strutture dati da usare con socket e bind
    if ((err = getaddrinfo(NULL, argv[1], &hints, &res)) != 0) {
        fprintf(stderr, "Errore setup indirizzo bind: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    // creo la socket
    if ((sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) < 0) {
        perror("Errore in socket");
        exit(EXIT_FAILURE);
    }

    // disabilito attesa uscita dase TIME_WAIT prima di creazione socket
    on = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    // a questo punto, posso liberare la memoria allocata da gettaddrinfo
    freeaddrinfo(res);

    // trasforma la socket passiva d'ascolto
    if (listen(sd, SOMAXCONN) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    for (;;) {
        int ns, pid;

        // mi metto in attesa di richieste di connessione
        ns = accept(sd, NULL, NULL);
        if (ns < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        // creo un processo figlio per gestire la richiesta
        if ((pid = fork()) < 0) {
            perror("fork");
            exit(EXIT_FAILURE);
        } else if (pid == 0) {
            // FIGLIO
            rxb_t rxb;
            char request[MAX_REQUEST_SIZE];
            char response[80];
            int lenght_of_string;
            size_t request_len;

            //chiudo la socket passiva
            close(sd);

            rxb_init(&rxb, MAX_REQUEST_SIZE);

            // avvio il ciclo gestione richieste
            for (;;) {
                /* Inizializzo il buffer request a zero e non uso
				 * l'ultimo byte, così sono sicuro che il contenuto del
				 * buffer sarà sempre null-terminated. In questo modo,
				 * posso interpretarlo come una stringa C e passarlo
				 * direttamente alla funzione strlen. Questa è
				 * un'operazione che va svolta prima di ogni nuova
				 * richiesta. */
				memset(request, 0, sizeof(request));
				request_len = sizeof(request) - 1;

                // Leggo la richiesta da client
                if (rxb_readline(&rxb, ns, request, &request_len) < 0) {
                    /* Se sono qui, è perché ho letto un
					 * EOF. Significa che il Client ha
					 * chiuso la connessione, per cui
					 * dealloco rxb ed esco dal ciclo di
					 * gestione delle richieste. */
                    rxb_destroy(&rxb);

                    break;
                }

#ifdef USE_LIBUNISTRING
                // Verifico che il messaggio sia UTF-8 valido
                if (u8_check((uint8_t *)request, request_len) != NULL) {
                    fprintf(stderr, "Request is not valid UTF-8!\n");
                    close(ns);
                    exit(EXIT_FAILURE);
                }

                // Calcolo numero caratteri UTF-8 contenuti in stringa richiesta
                lenght_of_string = u8_strlen((uint8_t *)request, request_len);
#else
                // Calcolo del numero di byte contenuti in stringa richiesta
                lenght_of_string = strlen(request);
#endif
                // Preparo il bufer contenente la risposta
                snprintf(response, sizeof(response), "%d\n", lenght_of_string);

                // Invio la risposta
                if (write_all(ns, response, strlen(response)) < 0) {
                    perror("write all");
                    exit(EXIT_FAILURE);
                }
            }

            // chiudo la socket attiva
            close(ns);

            // Termino il figlio
            exit(EXIT_SUCCESS);
        }

        // PADRE

        // Chiudo la socket attiva
        close(ns);
    }

    // Chiudo la socket passiva
    close(sd);

    return 0; 
}