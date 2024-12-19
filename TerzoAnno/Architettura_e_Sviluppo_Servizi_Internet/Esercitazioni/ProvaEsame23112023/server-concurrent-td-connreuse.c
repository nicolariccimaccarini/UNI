#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <string.h>
#include <limits.h>
#ifdef USE_LIBUNISTRING
#include <unistr.h>
#endif
#include "utils.h"
#include "rxb.h"

#define MAX_REQUEST_SIZE (64*1024)

void sigchld_handler(int signo){
    int status;
    (void)signo;

    while(waitpid(-1, &status, WNOHANG) > 0)
        continue;
}

/* solo se richiede la funzione autorizza 
int autorizza(const char *username, const char *password) {
    return 1;
} 
*/ 

int main(int argc, char** argv) {
    int err, sd, ns, on;
    pid_t pid_child;
    struct addrinfo hints, *res;
    struct sigaction sa;

    if (argc != 2) {
        fprintf(stderr, "Uso server porta\n");
        exit(EXIT_FAILURE);
    }

    signal(SIGPIPE, SIG_IGN);

    sigemptyset(&sa.sa_mask);
    sa.sa_flags   = SA_RESTART;
    sa.sa_handler = sigchld_handler;

    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction SIGCHLD");
        exit(EXIT_FAILURE);
    }

    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    if ((err = getaddrinfo(NULL, argv[1], &hints, &res)) != 0) {
        fprintf(stderr, "Erroore setup indirizzo %s\n", gai_strerror(err));
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
        perror("Errore bind");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(res);

    if (listen(sd, SOMAXCONN) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    for (;;) {
        printf("Server in ascolto ...\n");

        if ((ns = accept(sd, NULL, NULL)) < 0) {
            perror("accept");
            exit(EXIT_FAILURE);
        }

        if ((pid_child = fork()) < 0) {
            perror("fork pid_child");
            exit(EXIT_FAILURE);
        }

        if (pid_child == 0) {
            pid_t pid_n1, pid_n2;
            int status, pipe_n1n2[2];
            const char *end_request = "\n--- END REQUEST ---\n";
            rxb_t rxb;

            close(sd);

            memset(&sa, 0, sizeof(sa));
            sa.sa_handler = SIG_DFL;

            if (sigaction(SIGCHLD, &sa, NULL) == -1) {
                perror("sigaction sigchld");
                exit(EXIT_FAILURE);
            }

            rxb_init(&rxb, MAX_REQUEST_SIZE);

            for (;;) {
                char username[1024], nome_progetto[1024], nome_versione[1024];
                size_t username_len, nome_progetto_len, nome_versione_len;
                char filename[PATH_MAX];

                memset(username, 0, sizeof(username));
                username_len = sizeof(username) - 1;

                if(rxb_readline(&rxb ,ns,username, &username_len) < 0) {
                    rxb_destroy(&rxb);
                    break;
                }

#ifdef USE_LIBUNISTRING
                if(u8_check((uint8_t*)username, username_len) != NULL) {
                    fprintf(stderr, "richiesta non in utf-8");
                    close(ns);
                    exit(EXIT_SUCCESS);
                }
#endif

                memset(nome_progetto, 0, sizeof(nome_progetto));
                nome_progetto_len = sizeof(nome_progetto) - 1;

                if(rxb_readline(&rxb, ns, nome_progetto, &nome_progetto_len) < 0){
                    rxb_destroy(&rxb);
                    break;
                }

#ifdef USE_LIBUNISTRING
                if(u8_check((uint8_t*) nome_progetto,nome_progetto_len) != NULL){
                    fprintf(stderr, "richiesta non in utf-8");
                    close(ns);
                    exit(EXIT_SUCCESS);
                }
#endif

                memset(nome_versione, 0, sizeof(nome_versione));
                nome_versione_len = sizeof(nome_versione) - 1;

                if(rxb_readline(&rxb, ns, nome_versione, &nome_versione_len) < 0){
                    rxb_destroy(&rxb);
                    break;
                }

#ifdef USE_LIBUNISTRING
                if(u8_check((uint8_t*) nome_versione, nome_versione_len) != NULL){
                    fprintf(stderr, "richiesta non in utf-8");
                    close(ns);
                    exit(EXIT_SUCCESS);
                }
#endif

                /* solo se richiede la funzione autorizza
                if(autorizza(username, password) != 1) {
                    char *unauthorized = "Non autorizzato!\n";
                    write_all(ns, unauthorized, strlen(unauthorized));
                    write_all(ns, end_request, strlen(end_request));
                    continue;
                } 
                */

                snprintf(filename, sizeof(filename), "./compilation_reports/%s.txt", nome_progetto);

                if (pipe(pipe_n1n2) < 0) {
                    perror("creazione pipe");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n1 = fork()) < 0) {
                    perror("fork pid_n1");
                    exit(EXIT_FAILURE);
                }

                if (pid_n1 == 0) {
                    // NIPOTE 1

                    close(ns);
                    close(pipe_n1n2[STDIN_FILENO]);
                    close(STDOUT_FILENO);

                    if (dup(pipe_n1n2[STDOUT_FILENO]) < 0) {
                        perror("dupe pipe_n1n2[1]");
                        exit(EXIT_FAILURE);
                    }
                    close(pipe_n1n2[STDOUT_FILENO]);

                    execlp("grep", "grep", username, filename, (char *)NULL);
                    perror("grep username");
                    exit(EXIT_FAILURE);
                }

                if ((pid_n2 = fork()) < 0) {
                    perror("fork pid_n2");
                    exit(EXIT_FAILURE);
                }

                if (pid_n2 == 0) {
                    // NIPOTE 2

                    close(pipe_n1n2[STDOUT_FILENO]);
                    close(STDIN_FILENO);

                    if (dup(pipe_n1n2[STDIN_FILENO]) < 0) {
                        perror("dup pipe_n1n2[0]");
                        exit(EXIT_FAILURE);
                    }
                    close(pipe_n1n2[STDIN_FILENO]);
                    close(STDOUT_FILENO);

                    if (dup(ns) < 0) {
                        perror("dup ns");
                        exit(EXIT_FAILURE);
                    }
                    close(ns);

                    execlp("grep", "grep", nome_versione, (char *)NULL);
                    perror("grep nome_versione");
                    exit(EXIT_FAILURE);                  
                }

                close(pipe_n1n2[STDIN_FILENO]);
                close(pipe_n1n2[STDOUT_FILENO]);

                waitpid(pid_n1, &status, 0);
                waitpid(pid_n2, &status, 0);

                if (write_all(ns, end_request, strlen(end_request)) < 0) {
                    perror("write_all end_request");
                    exit(EXIT_FAILURE);
                }
            }

            close(ns);
            exit(EXIT_SUCCESS);
        }

        close(ns);
    }

    close(sd);
    return EXIT_SUCCESS;
}