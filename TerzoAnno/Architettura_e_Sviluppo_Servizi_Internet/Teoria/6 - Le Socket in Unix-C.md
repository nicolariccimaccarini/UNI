## Unix: strumenti di sincronizzazione, memorizzazione, comunicazione
- **Uso di segnali** (sincronizzazione) $\rightarrow$ processo invia limitata quantita' di info e manca il mittente del segnale
- **Uso di file** (memorizzazione) $\rightarrow$ solo tra processi che condividono un file system
- **IPC** (Inter Process Communication) 
	- pipe (solo tra processi con un avo in comune)
	- pipe con nome (per processi su una stessa macchina)
	- shared memory
	- **Socket** (e RPC)

## I processi e l'I/O
I processi interagiscono con l'I/O secondo il paradigma *open-read-write-close*.
Un processo vede il mondo esterno come un insieme di descrittori.
Anche le **Socket sono identificate da un descrittore**.
**Stessa semantica** delle pipe

## Programmazione di rete e paradigma *open-read-write-close*
La programmazione di rete richiede delle funzionalità non gestibili in modo completamente omogeneo alle pipe (e ai file):
- un collegamento via rete, cioe' l'associazione delle due parti puo' essere 
	- con connessione (simile o-r-w-c)
	- senza connessione
- i descrittori $\rightarrow$ trasparenza dai nomi va bene nel caso del file (flessibilita'), ma nel caso di network programming puo' non essere sufficientemente espressivo
- in programmazione di rete bisogna specificare piu' parametri per definire un collegamento con connessione
  `<protocollo; indirizzo locale; indirizzo remoto; processo remoto>`

Altre problematiche:
- UNIX I/O e' orientato allo stream, non ai messaggi di dimensioni prefissate
	- alcuni protocolli di rete fanno uso di messaggi di dimensioni prefissate
- E' necessario che l'interfaccia socket possa gestire piu' protocolli
- Schemi Client/Server sono asimmetrici

## L'interfaccia Socket
- Comunicazione fra processi su diversi nodi
- Interfaccia dipendente da architettura di rete
- Supportare diversi protocolli, diversi hardware, diversi tipi di nomi, etc.

UNIX + TCP-UDP/IP $\rightarrow$ BSD UNIX (progetto Universita' Berkeley finanziato da ARPA, ma ora Socket disponibili su tutte le versioni Unix, Windows, etc.)

Il canale di comunicazione tra il processo A e il processo B e' definito da:
`<protocollo; indirizzo IP locale; porta locale; indirizzo IP remoto; porta remota>`

## Socket
Una socket e' un'astrazione che definisce il terminale di canale di comunicazione bidirezionale.
Uso di descrittori per le socket, ma diverse operazioni per avere diverse semantiche.
![[socketUNIX.png]]

### Strutture dai Socket
Una socket e' creata all'interno di un dominio di comunicazione.

**Dominio di comunicazione** $\rightarrow$ semantica di comunicazione + standard di dominazione
Esempi domini: **AF_INET** (IPv4), **AF_INET6** (IPv6 con compatibilità IPv4  ove possibile), **AF_UNSPEC** (IPv4 e IPv6), **AF_UNIX** (Unix socket), etc.

![[strutturaDatiSocket.png]]

### Formato indirizzi e struttura socket
- **Dominio AF_INET** $\rightarrow$ comunicazioni Internet IPv4-only. Indirizzo composto da IPv4 dell'host (32 bit) e da numero di porta (16 bit)
- **Dominio AF_INET6** $\rightarrow$ comunicazione Internet IPv6-enabled. Indirizzo Internet composto da indirizzo IPv6 dell'host (128 bit) e numero di porta (16 bit). API retrocompatibile con IPv4 attraverso uso di indirizzi IPv6 speciali denominati V4-mapped.
- **Dominio AF_UNIX** $\rightarrow$ comunicazioni tra processi UNIX. L'indirizzo ha stesso formato di nome di file.

In ciascun dominio abbiamo bisogno di una struttura dati specifica per rappresentare gli indirizzi delle socket.

#### Retrocompatibilita'
Alcuni sistemi non offrono retrocompatibilita'. **In tali sistemi non e' quindi possibile catturare richieste di connessioni e traffico IPv4 da una socket AF_INET6**

Problema $\rightarrow$ come gestire le richieste di connessione contemporaneamente su piu' socket?
- Necessita' di adottare un'architettura significativamente piu' complicata.

#### Formato indirizzi nel Dominio AF_INET
``` C
// struttura in_addr
struct in_addr {
	in_addr_t s_addr;
};

// struttura sockaddr_in
struct sockaddr_in {
	sa_family_t sin_family;
	in_port_t sin_port;
	struct in_addr sin_addr;
	char sin_zero[8];
};
```

#### Formato indirizzi nel Dominio AF_INET6
``` C
// struttura in6_addr
struct in6_addr {
	uint8_t s6_addr[16];
};

//struttura sockaddr_in6
struct sockaddr_in6 {
	sa_family_t sin6_family;
	in_port_t sin6_port;
	uint32_t sin6_flowinfo;
	struct in6_addr sin6_addr;
	uint32_t sin6_scope_id;
};
```

#### Formato indirizzi nel Dominio AF_UNIX
``` C
// struttura sockaddr_un
struct sockaddr_un {
	sa_family_t sun_family;
	char sun_path[108];
};
```

### Formato indirizzi e struttura socket
Le funzioni per la programmazione di rete nella Socket API possono ricevere diversi tipi di indirizzo a seconda della famiglia di protocolli di scelta.

Uso di due struttura dati gerarchiche, `struct sockaddr` e `struct sockaddr_storage`, e poi cat al tipo specifico:
- `sockaddr` come interfaccia comune per tutti i diversi possibili indirizzi 
- `sockaddr_storage` per contenere i diversi possibili tipi di indirizzi

``` C
struct sockaddr {
	sa_family_t sa_family;
	char sa_data[14];
}

struct sockaddr_storage {
	sa_family_t ss_family;
	char ss_padding[SIZE];
}
```

### Implementazione primitive di comunicazione nel kernel 
``` C
int connect(int sd, const struct sockaddr *sa, socklen_t len) { ...
	if (len < sizeof(struct sockaddr)) return -1;
	switch (sa->sa_family) {
	case AF_INET:
		/* chiamo versione IPv4 della primitiva */
		if (len < sizeof(struct sockaddr_in)) return -2;
		connect_ipv4(sd, (struct sockaddr_in *)sa);
	case AF_INET6:
		/* chiamo versione IPv6 della primitiva */
		if (len < sizeof(struct sockaddr_in6)) return -3;
		connect_ipv6(sd, (struct sockaddr_in6 *)sa);
	case AF_UNIX: /* chiamo versione Unix della primitiva */
		if (len < sizeof(struct sockaddr_un)) return -4;
		connect_unix(sd, (struct sockaddr_un *)sa);
	...
}
```

In realta', sebbene funzionante e equivalente, l'architettura di gestione dei diversi domini di comunicazione nel kernel di Linux e' leggermente piu' complessa di quella nella slide precedente.

AF_INET usa `struct proto_ops inet_stream` e `inet_dgram_ops`
AF_INET6 usa invece `inet6_stream_ops` e `inet6_dgram_ops`

### Tipi di socket

| TIPO           | DESCRIZIONE                                                                                            |
| -------------- | ------------------------------------------------------------------------------------------------------ |
| SOCK_STREAM    | terminale di un canale di comunicazione **con connessione e affidabile** (socket stream o TCP)         |
| SOCK_SEQPACKET | trasferimento affidabile di sequenze di pacchetti (protocollo SCTP)                                    |
| SOCK_DGRAM     | terminale di un canale di comunicazione **senza connessione e non affidabile** (socket datagram o UDP) |
| SOCK_RAW       | accesso diretto al livello di rete (IP)                                                                |
| SOCK_DCCP      | protocollo DCCP(~ UDP + congestion control)                                                            |
|                |                                                                                                        |

Una **socket STREAM** stabilisce una **connessione**. La connessione e' affidabile, bidirezionale e i byte sono consegnati in ordine.
Non sono mantenuti i confini dei messaggi. Queste caratteristiche sono forzate dalla scelta di TCP come protocollo di livello di trasporto.

Una **socket DATAGRAM** non stabilisce alcuna connessione (connectionless), non c'e' garanzia di consegna del messaggio, i confini dei messaggi sono mantenuti e non e' garantito l'ordine.
Queste caratteristiche sono forzate dalla scelta di UDP come protocollo di livello di trasporto.

### Creazione di una socket
``` C
sd = socket(dominio, tipo, protocollo);
int sd, dominio, tipo, protocollo;
```

Crea una SOCKET e ne restituisce il descrittore `sd` (socket descriptor).
`dominio` denota il particolare dominio di comunicazione (es. AF_INET, AF_INET6)
`tipo` indica il tipo di comunicazione (es. SOCK_STREAM o SOCK_DGRAM)
`protocollo` specifica uno dei protocolli supportati dal dominio (se indica 0 viene scelto il protocollo di default)

### Associazione socket - indirizzo locale
``` C
error = bind(sd, ind, lun)
int error, sd;
const struct sockaddr* ind;
socket_t lun;
```

Associa alla socket di descrittore `sd` l'indirizzo codificato nella struttura puntata da `ind` e di lunghezza `lun` (la lunghezza e' necessaria poiche' la funzione bind puo' essere impiegata con indirizzi di lunghezza diversa).

### Comunicazione connection-oriented
Collegamento (**asimmetrico**) tra processo Client e Server:
1. il server e il client devono creare ciascuno una propria socket e definire l'indirizzo
2. deve essere creata la connessione tra le due socket
3. fase di comunicazione
4. chiusura delle socket

Creazione tra il client e il server (schema asimmetrico):
- LATO CLIENT
	- Il client richiede una connessione al server (primitiva connect)
	- Uso di una socket attiva
- LATO SERVER
	- Il server definisce una coda di richieste di connessione (primitiva **listen**) e attende le richieste
	- Uso di una socket passiva 
	- Quando arriva una richiesta, la richiesta viene accettata, stabilendo cosi' la connessione. La comunicazione puo' quindi iniziare

**Lato Client**:
``` C
error = connect(sd, ind, lun);
int err, sd;
struct sockaddr* ind;
socklen_t lun;
```

Richiede la connessione fra la socket locale il cui descrittore e' `sd` e la socket remota il cui indirizzo e' codificato nella struttura puntata da `ind` e la cui lunghezza e' `lun`.

**Lato Server**:
```C
error = listen(sd, dim);
int error, sd, dim;
```

Trasforma la socket `sd` in **passiva**, pronta per ricevere una richiesta di connessione.
Crea una **coda**, associata alla socket `sd` in cui vengono inserite le richieste di connessione dei client.
La coda puo' contenere al piu' `dim` elementi.
Le richieste di connessione vengono estratte dalla coda quando il server esegue la `accept()`.

``` C
nuovo_sd = accept (sd, ind, lun);
int nuovo_sd, sd;
struct sockaddr * ind;
socklen_t * lun;
```

Estrae una richiesta di connessione dalla coda predisposta alla `listen()`
Se non ci sono richieste di connessione in coda, sospende il server finche' non arriva una richiesta alla socket `sd`.
Quando la richiesta arriva, crea una nuova socket di lavoro `nuovo_sd` e restituisce l'indirizzo della socket del client tramite `ind` e la sua lunghezza tramite `lun`.

Una volta completata la connessione si possono utilizzare le normali primitive `read` e `write`
``` C
nread = read (sock_desc, buf, lun);
nwrite = write (sock_desc, buf, lun);
```
`buf` e' un puntatore a un buffer di lunghezza `lun` dal quale prelevare o in cui inserire il messaggio.
`sock_desc` e' il socket descriptor (di una socket attiva).
Uso di `send` e `recv` per dati out-of-band.

**Termine connesione**:
Al termine della comunicazione, la connessione viene interrotta mediante la primitiva close che chiude la socket
```C
error = close(sd);
int sd;
```

Problemi `close()`
- chiusura connessione avviene solo se `sd` e' l'ultimo descrittore aperto della socket
- chiusura della connessione in entrambi i versi (sia ricezione che trasmissione)

```C
error = shutdown(sd, how);
int sd, how;
```

Per terminare la connessione si puo' usare anche shutdown, che fornisce maggiore flessibilita':
- chiusura solo dell'esterno di ricezione (`how=0`, SHUT_RD)
- chiusura solo dell'esterno di trasmissione (`how=1`, SHUT_WR)
- chiusura dei due estremi (`how=2`, SHUT_RDWR)


Esempio: dominio AF_INET
``` C
// client
int main (int argc, char **argv) { // argv[1] e argv[2] indirizzo IP e porta server
	int sd; struct sockaddr_in rem_indirizzo;
	...
	sd = socket (AF_INET, SOCK_STREAM, 0);
	.......
	/* preparazione in struttura sockaddr_in rem_indirizzo dell'indirizzo del server */
	memset(&rem_indirizzo, 0, sizeof(rem_indirizzo));
	rem_indirizzo.sin_family = AF_INET;
	rem_indirizzo.sin_addr.s_addr = inet_addr(argv[1]);
	rem_indirizzo.sin_port = htons(atoi(argv[2]));
	.......
	connect (sd, (struct sockaddr *)&rem_indirizzo, sizeof(rem_indirizzo));
	.....
	write (sd, buf, dim); read (sd, buf, dim);
	......
	// chiusura, uso di close o shutdown
	close(sd);
	...

// server
int main (int argc, char **argv) { // argv[1] contiene porta server
	int ss; struct sockaddr_in mio_indirizzo;
	...
	ss = socket(AF_INET, SOCK_STREAM, 0);
	...
	/* preparazione nella struttura mio_indirizzo dell'indirizzo del server */
	memset(&mio_indirizzo, 0, sizeof(mio_indirizzo));
	mio_indirizzo.sin_family = AF_INET;
	mio_indirizzo.sin_addr.s_addr = htonl(INADDR_ANY);
	mio_indirizzo.sin_port = htons(atoi(argv[1]));
	...
	bind (ss, (struct sockaddr *)&mio_indirizzo, sizeof(mio_indirizzo));
	..........
	listen (ss, SOMAXCONN);
	..........
```


Esempio: Dominio AF_INET6
``` C
// client
int main (int argc, char **argv) { // argv[1] e argv[2] indirizzo IP e porta server
	struct sockaddr_in6 rem_indirizzo;
	int sd = socket (AF_INET6, SOCK_STREAM, 0);
	.......
	/* preparazione in struttura sockaddr_in rem_indirizzo dell'indirizzo del server */
	memset(&rem_indirizzo, 0, sizeof(rem_indirizzo));
	rem_indirizzo.sin6_family = AF_INET6;
	inet_pton(AF_INET6, argv[1], &rem_indirizzo.sin6_addr);
	rem_indirizzo.sin6_port = htons(atoi(argv[2]));
	.......
	connect (sd, (struct sockaddr *)&rem_indirizzo, sizeof(rem_indirizzo));
	.....
	write (sd, buf, dim); read (sd, buf, dim);
	......
	// chiusura, uso di close o shutdown
	close (sd);
	return 0;
}

// server
int main (int argc, char **argv) { // argv[1] contiene porta server
	struct sockaddr_in6 mio_indirizzo;
	int ss = socket(AF_INET6, SOCK_STREAM, 0);
	...
	/* preparazione nella struttura mio_indirizzo dell'indirizzo del server */
	memset(&mio_indirizzo, 0, sizeof(mio_indirizzo));
	mio_indirizzo.sin6_family = AF_INET6;
	mio_indirizzo.sin6_addr = in6addr_any;
	mio_indirizzo.sin6_port = htons(atoi(argv[1]));
	...
	bind (ss, (struct sockaddr *)&mio_indirizzo, sizeof(mio_indirizzo));
	..........
	listen (ss, SOMAXCONN);
	..........
```

### Trasformazione da nome logico a indirizzo fisico
```C
int getaddrinfo(const char *host_name, const char *service_name, 
				struct addrinfo *hints, struct addrinfo **res);
```

`getaddrinfo` serve per effettuare la risoluzione dei nomi. 
Riceve in ingresso: nome logico di host Internet; servizio a cui connettersi, varie preferenze; restituisce la lista di struttura `addrinfo` in cui ciascun nodo rappresenta uno degli indirizzi fisici del servizio.
\
### Preparazione di un indirizzo remoto
``` C
int err;                            /* controllo errori */
struct addrinfo hints;              /* direttive per getaddrinfo */
struct addrinfo *res;               /* lista indirizzi processo remoto */
char *host_remoto = “www.unife.it”; /* nome host remoto */
char *servizio_remoto = “http”;     /* nome (o numero porta) servizio remoto */
.......
/* preparazione delle direttive per getaddrinfo */
memset ((char *)&hints, 0, sizeof(hints));
hints.ai_family = AF_INET; /* voglio solo indirizzi di famiglia AF_INET */
hints.ai_socktype = SOCK_STREAM; /* voglio usare una socket di tipo stream */

err = getaddrinfo(host_remoto, servizio_remoto, &hints, &res);
if (err != 0) { /* controllo errori */
	/*gai_strerror restituisce un messaggio che descrive il tipo di errore */
	fprintf(stderr, “Errore risoluzione nome: %s\n“, gai_strerror(err)); exit(1);
}
```

### Quale tipo di famiglia AF usare per la risoluzione?
Specificando opportunamente il valore di `hints.ai_family` e' possibile controllare il comportamento di `getaddrinfo`.
- Scegliendo AF_INET effettuiamo solo la risoluzione da nome a indirizzo IPv4.
- Scegliendo AF_INET6 effettuiamo la risoluzione da nome a indirizzo IPv6.
- Scegliendo AF_UNSPEC effettuiamo la risoluzione a indirizzo IPv4 che quella da nome a indirizzo IPv6 se il sistema operativo supporta quest'ultimo protocollo.

### Procedura di connessione "naive"
Uso risultato di `getaddrinfo` come argomento per le system call `socket` e `connect`
``` C
/* mi connetto al primo degli indirizzi restituiti da getaddrinfo */
if ((sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) < 0) {
	fprintf(stderr, “Errore creazione socket!”); exit(2);
}

if (connect(sd, res->ai_addr, res->ai_addrlen) < 0) {
	fprintf(stderr, “Errore di connessione!”); exit(3);
}

/* una volta terminatone l'utilizzo, la memoria allocata da getaddrinfo
va esplicitamente liberata tramite una chiamata a freeaddrinfo */
freeaddrinfo(res);
...
```

### Best practice per codice IPv6-enabled
Per lo sviluppo di applicazioni portabili IPv6-enabled, e' bene specificare sempre:
``` C
hints.ai_family = AF_UNSPEC
```
In questo modo, le nostre applicazioni useranno automaticamente IPv6 dove presente, e continueranno comunque a funzionare in IPv4 laddove IPv6 non sia ancora supportato.

Tuttavia, questa pratica puo' portare a problemi di connessione in macchine che supportano IPv6 ma che non hanno connettivita' IPv6 (ovverosia gran parte dei sistemi attualmente connessi ad internet).

Infatti, `getaddrinfo` ordina la listi di indirizzi restituiti secondo la procedura specificata nello RFC 6724

Quando chiamata con AF_UNSPEC su un sistema con supporto a IPv6, `getaddrinfo` elenca per primi gli indirizzi IPv6. Questo significa che tipicamente l'applicazione provera' prima a connettersi al server tramite IPv6. Se non c'e' connettivita' IPv6, il tentativo di connessione ovviamente fallira'.
Per evitare questo tipo di problemi e' bene progettare la fase di connessione delle nostre applicazioni in modo da consentire il fallback a indirizzi IPv4 laddove la connettivita' IPv6 non fosse presente.

#### Procedura di connessione fallback
``` C
struct addrinfo hints, *res, *ptr;
...
for (ptr = res; ptr != NULL; ptr = ptr->ai_next) {
	/* se socket fallisce salto direttamente alla prossima iterazione */
	if ((sd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol)) < 0)
		continue;
	/* se connect funziona esco dal ciclo */
	if (connect(sd, ptr->ai_addr, ptr->ai_addrlen) == 0)
		break;
	close(sd);
}
/* se ptr vale NULL vuol dire che nessuno degli indirizzi restituiti da
getaddrinfo è raggiungibile */
if (ptr == NULL) {
	fprintf(stderr, “Errore di connessione!\n”); exit(3);
}
...
freeaddrinfo(res); /* non dimentichiamo mai di chiamare freeaddrinfo! */
```

### happy eyeballs
Se si implementa una procedura di connessione con fallback, e' possibile aver tempi di connessione al server molto lunghi, soprattutto nel caso non vi sia connettivita' IPv6.
Per risolvere questo problema, IETF ha pubblicato lo RFC 6555, che raccomanda l'adozione di un nuovo algoritmo di connessione denominato "happy eyeballs".
Con happy eyeballs, il client prova a connettersi contemporaneamente al server sia via IPv4 che via IPv6, e usa la prima delle due connessioni che viene stabilita.
Happy eyeballs e' un meccanismo pensato in ottica di breve-medio termine, per facilitare la migrazione a IPv6 e verra' deprecato quando IPv6 sara' la versione piu' diffusa del protocollo IP.

### Server e `getaddrinfo`
In realta' `getaddrinfo` e' cosi' comoda che ci conviene usarla (con la flag AI_PASSIVE) anche sul server per preparare l'indirizzo da passare a `bind`:
``` C
memset((char *)&hints, 0, sizeof(hints));
hints.ai_flags = AI_PASSIVE;
hints.ai_family = AF_INET; /* AF_INET6 per comunicare con server IPv6,
							  AF_UNSPEC per comunicare sia con server IPv4
							  che con server IPv6 (dove disponibile) */

hints.ai_socktype = SOCK_STREAM;

err = getaddrinfo(NULL, “50000”, &hints, &res);

sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

if (bind(sd, res->ai_addr, res->ai_addrlen) < 0) { perror(“bind”); exit(1); }

freeaddrinfo(res); /* non dimentichiamoci mai di liberare la memoria */
```

### Caveat
Il codice nella slide precedente presenta un problema nel caso 
`hints.ai_family = AF_UNSPEC` e la configurazione di default del file `/etc/gai.config` assegni (erroneamente) una priorita' piu' elevata a indirizzi IPv4 rispetto a IPv6.
Problema facilmente risolvibile modificando la configurazione del file `/etc/gai.config`

### Perche' non usare fallback anche lato server?
APPROCCIO FONDAMENTALMENTE ERRATO.
Infatti, lato server vogliamo usare solo la prima struttura `sockaddr` restituita da `getaddrinfo`. Se questa non funziona, molto meglio terminare il processo con un errore piuttosto che tentare di usare le strutture `sockaddr` successive, che limiterebbero significativamente la connettivita' del nostro server.

### Strumenti `test_gai` e `testga`
Si possono usare gli strumenti `test_gai` e `testga` per interrogare `getaddrinfo` con diversi parametri e stampare a video gli indirizzi restituiti.

Esempio d'uso `test_gai`:
``` sh
./test_gai –s github 443 # AF_UNSPEC e SOCK_STREAM
./test_gai -6 –d dns.unife.it dns # AF_INET6 e SOCK_DGRAM
```

Esempio d'uso `testga`:
``` sh
./testga –f inet –s http –h www.google.com # AF_INET
```


### Comunicazione connectionless
Non viene creata una connessione tra processo client e processo server.
1. la primitiva `socket()` crea la socket
2. la primitiva `bind()` lega la socket a un numero di porta (opzionale per il client)
3. i processi inviano/ricevono msg su socket. Per ogni msg specifica l'indirizzo destinatario (IP e porta)

Si noti che un processo puo' usare la stessa socket per spedire/ricevere messaggi a/da diversi processi.

Attenzione che le socket DATAGRAM permettono comunicazione:
- Senza connessione
- Non affidabili
- Datagram

``` C
int sendto(int sd, const void *msg, size_t len, int flags,
		   const struct sockaddr *to, socklen_t tolen);
	   
int recvfrom(int sd, void *buf, size_t len, int flags,
			 struct sockaddr *from, socklen_t *fromlen);
```

Le primitive `recvfrom` e `sendto`, oltre agli stessi parametri delle `read` e `write`, hanno anche due parametri aggiuntivi che denotano indirizzo e lunghezza di una struttura socket:
- nella `sendto`  servono per specificare l'indirizzo del destinatario
- nella `recvfrom` servono per restituire l'indirizzo del mittente

La `recvfrom` sospende il processo di attesa di ricevere il messaggio (coda di messaggi associata alla socket).
La `recvfrom` puo' ritornare zero, se ha ricevuto un messaggio (un datagramma) di dimensione zero.

![[socketDATAGRAMConnectionless.png]]

### Note conclusive sulle socket UDP
Ovviamente si possono fare le stesse considerazioni svolte nella parte delle socket Java:
- **UDP non e' affidabile**, perdita di messaggi puo' bloccare Cliente
- **Blocco del Client** anche per perdita msg verso Server inattivo
- **UDP non ha flow control**, Server lento perde messaggi

### Quale tipo di socket utilizzare
Per la scelta del tipo di socket stesse considerazioni viste in Java:
- servizi che richiedono connessioni $\leftrightarrow$ servizi connectionless
- Socket STREAM sono **affidabili**, DATAGRAM no
- **Prestazioni** STREAM inferiori alle DATAGRAM
- Socket STREAM hanno **semantica** at-most-once, socket DATAGRAM hanno semantica may-be
- Ordinamento messaggi $\rightarrow$ preservato in STREAM, non in DATAGRAM
- Per fare del **broadcast/multicast** piu' indicate le DATAGRAM

### Opzioni per le socket
Le opzioni permettono di modificare il comportamento delle socket.
Configurazione attraverso le primitive `getsockopt()` e `setsockopt()`.

``` C
#include <sys/types.h>
#include <sys/socket.h>
int getsockopt (int sockfd, int level, int optname, void *optval, sock_len *optlen);
int setsockopt (int sockfd, int level, int optname, const void *optval, sock_len 
				optlen);
```

`sockfd` = socket descriptor
`level` = livello (semantico) del protocollo (es. SOL_SOCKET, IPPROTO_TCP, ecc.)
`optname` = nome dell'opzione
`optval` = puntatore a un'area di memoria per valore
`optlen` = lunghezza (o puntatore) quarto argomento1


| Opzioni        | Descrizione                                         |
| -------------- | --------------------------------------------------- |
| SO_DEBUG       | abilita il debugging                                |
| SO_REUSEADDR   | riususo dell'indirizzo locale                       |
| SO_DONTROUTE   | abilita il routing dei mesaggi uscenti              |
| SO_LINGER      | ritarda la chiusura per messaggi pendenti           |
| SO_BROADCAST   | abilita la trasmissione broadcast                   |
| SO_OOBINLINE   | messaggi prioritari pari a quelli ordinari          |
| SO_SNDBUF      | setta dimensioni dell'output buffer                 |
| SO_RCVBUF      | setta dimensioni dell'input buffer                  |
| SO_SNDLOWAT    | setta limite inferiore di controllo di flusso out   |
| SO_PCVLOWAT    | limite inferiore di controllo di flusso in ingresso |
| SO_SNDTIMEO    | setta il timeout dell'output                        |
| SO_RCVTIMEO    | setta il timeout dell'input                         |
| SO_USELOOPBACK | abilita network bypass                              |
| SO_KEEPALIVE   | controllo periodico di connesione                   |
| SO_PROTOTYPE   | setta tipo di controllo                             |

#### Opzioni per le socket: riutilizzo del socket address (STREAM)
L'opzione SO_REUSEADDR modifica il comportamento della system call `bind()`.

Il sistema tende a non ammettere piu' di un indirizzo locale. Con l'opzione SO_REUSEADR, si forza l'uso dell'indirizzo di una socket **senza controllare l'unicita' dell'associazione**.

Se il demone termina, il riavvio necessita SO_REUSEADDR, altrimenti la chiamata a `bind()` sulla stessa porta causerebbe errore poiche' risulta gia' presente una connessione legata allo stesso socket address.

```C
int optval=1;
setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval));
bind(sd, res->ai_addr, res->ai_addrlen);
```

#### Opzioni per le Socket: TCP_NODELAY, TCP_QUICKACK e TCP_CORK
Le opzioni TCO_NODELAY, TCP_QUICKACK e TCP_CORK modificano il comportamento della socket stream:
- TCP_NODELAY disabilita l’algoritmo di Nagle che, per migliorare performance, cerca di consolidare numerose chiamate a write con buffer di piccole dimensioni in un numero minore di messaggi TCP di maggiori dimensioni. In pratica, l’algoritmo di Nagle forza un limite minimo di 200ms tra la trasmissione di due messaggi TCP consecutivi qualora essi non raggiungessero la dimensione massima consentita dalla rete (Maximum Segment Size, MSS).
- TCP_QUICKACK forza l’immediata trasmissione di ACK dopo la ricezione di un pacchetto (per default si implementa una politica delayed ACK di 40 msec per minimizzare messaggi ACK senza payload).
- TCP_CORK disabilita la trasmissione di messaggi TCP che non raggiungono la dimensione massima consentita dalla rete (MSS).

#### Altre opzioni per le Socket
**Controllo periodico della connessione**
- Il protocollo di trasporto puo' inviare messaggi di controllo periodici per analizzare lo stato di una connessione (SO_KEEPALIVE)
- Se problemi $\rightarrow$ connessione e' considerata abbattuta
- i processi avertiti da una SIGPIPE *chi invia i dati*
- da end-of-file *chi li riceve*

**Dimensioni buffer di trasmissione/ricezione**
- Opzioni SO_SNDBUF e SO_RCVBUF
- Aumento della dimensione buffer di trasmissione $\rightarrow$ invio messaggi piu' grandi senza attesa
- massima dimensione possibile 65535 byte

##### Gestione SIGPIPE
Il comportamento di default delle socket per la gestione degli errori purtroppo e' mal definito. Se un processo effettua una `write()` su una socket chiusa, anziche' ricevere un errore -1, viene lanciato un SIGPIPE che di default termina il processo.
**Ricordarsi sempre di disabilitare SIGPIPE in tutti i programmi**.
`signal(SIGPIPE, sigin);`
