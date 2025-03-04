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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef USE_LIBUNISTRING
#  include <unistr.h> /* per libunistring */
#endif
#include "utils.h"

/* Massima lunghezza stringhe: 64 KiB */
#define MAX_REQUEST_SIZE (64 * 1024)

// Gestore del segnale SIGCHLD
void handler(int signo) {
    int status;
    (void)signo;

    while (waitpid(-1, &status, WNOHANG) > 0)
        continue;
}

int main(int argc, char** argv) {
    int sd, err, on;
    struct addrinfo hints, *res;
    struct sigaction sa;

    // Controllo argomenti
    if (argc < 2) {
        fprintf(stderr, "Uso: ./server-concurrent-Ipd-connreuse-buffer <porta> \n");
        exit(EXIT_FAILURE);
    }

    // Ignoro SIGPIPE
    signal(SIGPIPE, SIG_IGN);

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

    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    if ((err = getaddrinfo(NULL, argv[1], &hints, &res)) != 0) {
        fprintf(stderr, "Errore setup indirizzo bind: %s\n", gai_strerror(err));
        exit(EXIT_FAILURE);
    }

    if ((sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) < 0) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    on = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    if (bind(sd, res->ai_addr, res->ai_addrlen) < 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    }

    // rilascio memoria allocata da getaddrinfo
    freeaddrinfo(res);

    // trasformo in socket passiva di ascoto
    if (listen(sd, SOMAXCONN) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    for (;;) {
        int ns, pid;

        // mi metto in attesa di richieste di connessione
        if ((ns = accept(sd, NULL, NULL)) < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        // Creo un processo figlio per gestire la richiesta
        if ((pid = fork()) < 0) {
            perror("fork");
            exit(EXIT_FAILURE);
        } else if (pid == 0) {
            // FIGLIO

            // chiudo la socket passiva
            close(sd);

            for (;;) {
                uint8_t len[2], response[65536];
                char nomefile[MAX_REQUEST_SIZE], stringa[MAX_REQUEST_SIZE];
                size_t nomefile_len, stringa_len;
                int pipe_nf[2], file, pid_n, nread, read_so_far, response_len;

                // Leggo lunghezza username
                if (read_all(ns, len, 2) < 0) {
                    perror("read");
                    exit(EXIT_FAILURE);
                }

                /* Decodifico lunghezza nomefile come intero unsigned a
				 * 16 bit in formato big endian (AKA network byte
				 * order) */
                nomefile_len = (size_t)len[1] | ((size_t)len[0] << 8);

                /* Inizializzo il buffer nomefile a zero e so che
				 * nomefile_len < sizeof(nomefile), quindi sono sicuro
				 * che il buffer sarà sempre null-terminated. In questo
				 * modo, posso interpretarlo come una stringa C e
				 * passarlo direttamente alla funzione strcmp. */
				memset(nomefile, 0, sizeof(nomefile));
				if (read_all(ns, nomefile, nomefile_len) < 0) {
					perror("read");
					exit(EXIT_FAILURE);
				}

#ifdef USE_LIBUNISTRING
                // Verifico che il messaggio sia UTF-8 valido
                if (u8_check((uint8_t *)nomefile, nomefile_len) != NULL) {
                    fprintf(stderr, "Request is not valid UTF-8!\n");
                    exit(EXIT_FAILURE);
                }
#endif

                if ((file = open(nomefile, O_RDONLY)) < 0) {
                    // non trovo il file, termino il programma
                    close(ns);
                    exit(EXIT_FAILURE);
                } else {
                    close(file);
                }

                // Leggo la lunghezza stringa
                if (read_all(ns, len, 2) < 0) {
                    perror("read");
                    exit(EXIT_FAILURE);
                }

                // Decodifico lunghezza stringa come intero unsigned a 16 bit in formato big endian
                stringa_len = (size_t)len[1] | ((size_t)len[0] << 8);

                /* Inizializzo il buffer stringa a zero e so che
				 * stringa_len < sizeof(stringa), quindi sono sicuro
				 * che il buffer sarà sempre null-terminated. In questo
				 * modo, posso interpretarlo come una stringa C e
				 * passarlo direttamente alla funzione strcmp. */
				memset(stringa, 0, sizeof(stringa));
				if (read_all(ns, stringa, stringa_len) < 0) {
					perror("read");
					exit(EXIT_FAILURE);
				}

#ifdef USE_LIBUNISTRING
                // Verifico che il messaggio sia UTF-8 valido
                if (u8_check((uint8_t *)nomefile, nomefile_len) != NULL) {
                    fprintf(stderr, "Request is not valid UTF-8!\n");
                    exit(EXIT_FAILURE);
                }
#endif

                if (pipe(pipe_nf) < 0) {
                    perror("pipe");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n = fork()) < 0) {
                    perorr("fork");
                    exit(EXIT_FAILURE);
                } else if (pid_n == 0) {
                    // NIPOTE

                    // Chiudo il file descriptor non usati
                    close(ns);
                    close(pipe_nf[0]);

                    // Redirezione output su pipe
                    close(1);
                    if (dup(pipe_nf[1]) < 0) {
                        perror("dup");
                        exit(EXIT_FAILURE);
                    }
                    close(pipe_nf[1]);

                    // Eseguo la grep sul server con i parametri corretti
                    execlp("grep", "grep", stringa, nomefile, (char *)NULL);
                    perror("exec grep");
                    exit(EXIT_FAILURE);
                }

                // FIGLIO

                // chiudo file descriptor non usati
                close(pipe_nf[1]);

                /* Questa implementazione fa uso di un buffer
				 * di memoria in cui salvare temporaneamente la
				 * risposta. Si tratta di un'operazione
				 * necessaria perché i protocolli applicativi
				 * di tipo length-prefixed data richiedono che
				 * le risposte siano precedute da due byte che
				 * ne indicano la dimensione. Quindi dobbiamo
				 * salvarne il contenuto da qualche parte e
				 * processarlo prima di reinviarlo al Client.
				 * Un buffer di memoria è un'opzione molto
				 * semplice da realizzare, che però è
				 * applicabile solo nel caso in cui la risposta
				 * ha una dimensione massima nota a priori. Una
				 * possibile implementazione alternativa, che
				 * non soffre di questo limite ma è decisamente
				 * più complessa, è quella basata sull'uso di
				 * file temporanei. */
                read_so_far = 0;
                while((nread = read(pipe_nf[0], response + read_so_far, sizeof(response) - read_so_far)) > 0) 
                    read_so_far +=  nread;

                if (nread < 0) {
                    perror("nread");
                    exit(EXIT_FAILURE);
                }

                if (read_so_far > 65535) {
                    fprintf(stderr, "Troppi dati\n");
                    exit(EXIT_FAILURE);
                }

                response_len = read_so_far;

                len[0] = (uint8_t)((response_len & 0xFF00) >> 8);
                len[1] = (uint8_t)(response_len & 0x00FF);

                if (write_all(ns, len, 2) < 0) {
                    perror("write");
                    exit(EXIT_FAILURE);
                }

                if (write_all(ns, response, response_len) < 0) {
                    perror("write");
                    exit(EXIT_FAILURE);
                }
            }
        }

        // PADRE

        // chiudo la socket attiva
        close(sd);
    }

    close(sd);
    return 0;
}