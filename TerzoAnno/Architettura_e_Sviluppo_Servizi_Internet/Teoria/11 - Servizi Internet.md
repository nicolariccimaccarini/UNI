## I Servizi del Livello Applicazione
Servizi di tre tipi fondamentali:
- **Terminale remoto** $\rightarrow$ accesso a nodi remoti
- **File transfer** $\rightarrow$ trasferimento file tra nodi diversi
- **Comandi remoti (applicazioni)** $\rightarrow$ esecuzione di comandi remoti, anche specializzati, e riferimenti a servizi remoti NEWS, MAIL, gopher, WWW

Proprieta' fondamentali:
- Trasparenza
- Modelli Client/Server ed evoluzioni
- Standardizzazione

Importante distinguere tra:
- **Programmi applicativi** $\rightarrow$ Client che si interfacciano con l'utente
- **Protocolli applicativi** o **di trasferimento** $\rightarrow$ protocolli che regolano lo scambio di messaggi tra Client e Server
- **Protocolli** di specifica del **formato dei dati** $\rightarrow$ definiscono formato messaggi
- **Protocolli di comunicazione** $\rightarrow$ protocolli di trasporto TCP o UDP


| Esempio World Wide Web                                | Esempio telnet                                           |
| ----------------------------------------------------- | -------------------------------------------------------- |
| Programma applicativo $\rightarrow$ Firefox, IE, etc. | Programma applicativo $\rightarrow$ telnet (comando)     |
| Protocollo applicativo $\rightarrow$ HTTP             | Protocollo applicativo $\rightarrow$ telnet (protocollo) |
| Formato dei dati $\rightarrow$ HTML                   | Formato dei dati $\rightarrow$ NVT                       |
| Protocollo di comunicazione $\rightarrow$ TCP         | Protocollo di comunicazione $\rightarrow$ TCP            |
Altri programmi e protocolli: File Transfer Protocol (**ftp**), Simple Mail Transfer Protocol (**smtp**), etc.

### telnet e rlogin
telnet e rlogin permettono il collegamento (sessione login) con macchina remota.
**Il terminale locale diventa un terminale del sistema remoto**

- **telnet** standard in TCP/IP (Internet) $\rightarrow$ gestisce eterogeneita' di S.O., HW, etc.
- **rlogin** per i sistemi UNIX (remote login)

I **programmi applicativi** telnet e rlogin hanno interfaccia verso utente da linea di comando (sono comandi di UNIX).
Il **protocollo di comunicazione** e' TCP/IP.

ATTENZIONE: per motivi di sicurezza l'uso di telnet e rlogin e' caldamente sconsigliato. Al loro posto si preferisce usare ssh (secure shell), una re-implementazione di rlogin che prevede la cifratura del canale di comunicazione.

#### Protocollo applicativo di telnet
**Client** stabilisce una connessione TCP con **Server** (porta 23), quindi accetta caratteri da utente e li manda al Server e contemporaneamente accetta caratteri dal server e li visualizza sul terminale utente.

**Server** accetta la richiesta di connessione dal Client e inoltra dati da connessione TCP al sistema locale.

Caratteristiche principali del protocollo applicativo di telnet:
- **gestione eterogeneità** tramite interfaccia standard **NVT**
- **Client** e **Server negoziano** le opzioni del collegamento (ASCII a  bit o a 8 bit)
- comunicazione **simmetrica**

##### Network Virtual Terminal (NVT)
Problema nella rete internet: mancanza di standardizzazione dei terminali. I terminali differiscono per il set di caratteri, la codifica dei caratteri, la lunghezza della linea e della pagina, i caratteri di controllo (individuati da diverse escape sequence), ecc.

Soluzione: definizione di un **terminale virtuale**, detto **NVT**, cioe' una standardizzazione di un terminale, con un formato definito e standardizzato di rappresentazione e codifica dei dati, dei caratteri di controllo, delle funzioni.

I Client riceve l'input dal terminale utente, lo traduce in formato NVT e lo invia al Server, che a sua volta lo traduce nella propria rappresentazione nativa.

![[NVT.png]]

All'inizio del collegamento tra Client e Server, si usa la rappresentazione a  bit US ASCII.

##### Negoziazione
Tutti gli NVT supportano un insieme minimo di funzionalita', ma alcuni terminali hanno piu' funzioni rispetto al set minimo.

I due endpoint possono negoziare una serie di opzioni (funzioni extra rispetto all'insieme di base) reciprocamente accettabili (set di caratteri, modalita' eco, ecc.)

Possibilita' di **negoziare** la connessione, sia alla inizializzazione sia successivamente per selezionare le opzioni del protocollo di comunicazione (modalita' a linee o a caratteri, set di caratteri, echo, emulazione terminale noto es. VT220, ...)

Protocollo per negoziare le opzioni e' **simmetrico**, usa messaggi:
- **WILL X** $\rightarrow$ will you agree to use option X
- **DO X** $\rightarrow$ I do agree to let you use option X
- **DON'T X** $\rightarrow$ I don't agree to let you use option X
- **WON'T X** $\rightarrow$ I won't start using option X

##### Implementazione Telnet
![[ImplementazioneTelnet.png]]

**Pseudo-terminal** puo' essere una funzione del sistema operativo.

#### rlogin
Servizio di login remoto $\rightarrow$ login su un'altra macchina UNIX (porta standard 513)

Se l'utente ha una home directory in remoto accede a quel direttorio.
Altrimenti, l'utente entra nella radice della macchina remota.

Il servizio di rlogin UNIX supporta il concetto di trusted hosts.
Utilizzando i file `.rhosts` `/etc/hosts.equiv` per garantire corrispondenze tra utenti (e permettere uso senza password).

##### Terminali in UNIX
UNIX e' nato come SO multiutente per computer condivisi di grandi dimensioni a cui ciascun utente accedeva tramite un terminale fisico connesso all'unita' centrale. **Il supporto ai terminali e' quindi uno dei componenti fondamentali di UNIX**

Storicamente, un grande numero di tipologie di terminali fisici veniva connesso a macchine UNIX. La mancanza di standardizzazione rendeva difficile scrivere programmi portabili che utilizzassero le funzionalita' del terminale. Sono nati quindi i database **termcap** e **terminfo**, che descrivono come eseguire varie operazioni di controllo dello schermo per un'ampia varieta' di terminali, e le librerie **curses** per la realizzazione di TUI (terminal user interface). Ancora oggi, questi tre componenti sono installati in tutti i PC UNIX.

Successivamente, il mercato e' andato verso l'uso di pochi formati di terminale standard, come il famoso VT100 di Digital. Al giorno d'oggi, si usano terminali virtuali, come xterm (o xterm-256color).

##### Caratteristiche rlogin
Lavorando solo con macchine UNIX, **rlogin non ha bisogno di NVT**:
- esporta l'ambiente del Client verso il Server.
- conosce l'ambiente di partenza e quello di arrivo, ha nozione di `stdin`, `stdout` e `srderr` (collegati al Client tramite TCP)
- utilizza una sola connessione TCP/IP
- flow control $\rightarrow$ il Client tratta **localmente** i caratteri di controllo del terminale.

**out-of-band** signaling per i comandi dal server al client
**out-of-band** signaling implementato con TCP urgent mode.
**in-band** signaling per i comandi dal client al server (spedizione dimensione finestra)

##### Implementazione e pseudoterminal
![[ImplementazioneRlogin.png]]

Il supporto agli pseudoterminali di UNIX permette implementazioni molto semplici di rlogin.

##### Secure Shell (SSH)
Secure Shell e' una implementazione sicura di rlogin, concepita e progettata per offrire la massima protezione durante l'accesso remoto a un altro host sulla rete.

Comunicazione su un canale cifrato, con uso di protocolli di sicurezza molto robusti e supporto a tecniche criptografiche state-of-the-art.

Al di sopra di SSH sono definite funzioni come Secure Copy (SCP), Secure File Transfer Protocol (SFTP), X session forwarding e port forwarding (per incapsulare il traffico di altri protocolli non sicuri).

SSH ha tre componenti (layer) principali:
- **Transport Layer Protocol** $\rightarrow$ consente l'autenticazione, la privacy e l'integrita' del server con una perfetta forward security. Puo' fornire compressione opzionale e viene eseguito su una connessione TCP.
- **User Authentication Protocol** $\rightarrow$ autentica il Client sul Server e viene eseguito sul livello di trasporto.
- **Connection Protocol** $\rightarrow$ esegue il multiplexing del tunnel crittografato su numerosi canali logici, al di sopra dello User Authentication Protocol.

SSH implementa un sistema piuttosto sofisticato di gestione delle chiavi e dei relativi agenti.
- Supporto ad autenticazione a chiave pubblica (es. come su GitHub).
- Supporto a stoccaggio sicuro delle chiavi (es. su Yubikey) e a multi-factor authentication.
- SSH Agent

###### SSH Agent
I Client SSH in genere vengono eseguiti per la durata di una sessione di accesso remoto e sono configurati per cercare la chiave privata dell'utente in un file nella directory home dell'utente.

Per una maggiore sicurezza, e' comune archiviare la chiave privata in una forma crittografata, in cui la chiave di crittografia viene calcolata da un passphrase che l'utente ha memorizzato. Poiche' la digitalizzazione della passphrase puo' essere noiosa, molti utenti preferirebbero inserirla solo una volta per sessione di accesso locale.

Pertanto, gli utenti eseguono un programma chiamato ssh-agent che viene eseguito oltre la durata di una sessione di accesso locale, memorizza le chiavi non crittografate in memoria e comunica con i client SSH utilizzando una socket UNIX.

### Trasferimento file (ftp) 
**ftp** (file transfer protocol) stabilisce un collegamento con una macchina remota per il trasferimento (upload e download) di file. Come per telnet, ci sono problemi di **sicurezza**, legati alla trasmissione in chiaro delle password. Possibile uso di FTP over SSL o di alternative sicure come **scp** (trasferimento di file su canale ssh.)

- Vari **programmi applicativi**, da linea di comando o con interfaccia grafica.
- **Protocollo di comunicazione** TCP.
- **Protocollo ftp** con comandi (4 caratteri ASCII a 7 bit) e risposte (numeri a 3 cifre). Es:
	- `STOR local-file` $\rightarrow$ trasferisce un file locale sulla macchina remota
	- `RETR remote-file` $\rightarrow$ trasferisce un file remoto sul disco locale

Utente utilizza varie interfacce. per esempio da linea di comando:
- put $\rightarrow$ esegue STOR
- get $\rightarrow$ esegue RETR
- mget-mput
- help
- dir
- ls
- cl
- lcd
- ...

ftp usa due connessioni per ogni collegamento client/server, una di CONTROLLO e una di DATI. Per questo, in alcuni testi si dice che le informazioni di controllo sono trasmessi fuori out-of-band.

Il Server mantiene lo **stato** della connessione del Client.

#### FTP - Codifica risposte
Le **risposte** sono codificate con 3 cifre, e un testo descrittivo:

La prima cifra codifica le interazioni:
- 1xx $\rightarrow$ Positive Preliminary reply
- 2xx $\rightarrow$ Positive Completion reply
- 3xx $\rightarrow$ Positive Intermediate reply
- 4xx $\rightarrow$ Transient Negative Completion reply (il comando puo' essere ripetuto)
- 5xx $\rightarrow$ Permanent Negative Completion reply
- 6xx $\rightarrow$ Protected reply

La seconda cifra codifica le risposte:
- x0x $\rightarrow$ Sintassi
- x1x $\rightarrow$ Informazione
- x2x $\rightarrow$ Connessione
- x3x $\rightarrow$ Autenticazione e accounting
- x4x $\rightarrow$ Non specificato
- x5x $\rightarrow$ File system

La terza cifra specifica piu' precisamente.

#### Implementazione FTP
![[ImplementazioneFTP.png]]

Un processo **master** del server attende connessioni (processo **`ftpd`**, demone di ftp) e si crea uno **slave** per ciascuna connessione.

Possibili diverse implementazioni slave: singolo o multi processo, ma sempre su diverse connessioni TCP (controllo e dati).

Caso singolo $\rightarrow$ un processo gestisce le due connessioni controllo e dati.
Caso multi $\rightarrow$ un processo gestisce la connessione dati, uno quella di controllo.

#### FTP Attivo
![[FTPAttivo.png]]

#### FTP Passivo
![[FTPPassivo.png]]

#### Problemi di FTP
FTP Present numerosi problemi, al punto che potrebbe forse essere indicato come esempio di come NON progettare servizi Internet.

In FTP attivo il Client comunica il proprio indirizzo IPv4 al Server come argomento del comando PORT, errore di design molto grosso. Infatti, scrivere un indirizzo di livello 3 in un protocollo di livello  rappresenta una gravissima violazione del principio del layering alla base del modello ISO/OSI con serie conseguenze per il protocollo applicativo. In seguito all'arrivo di IPv6, si e' dovuto estendere il protocollo con due comandi (EPRT ed EPSV) che sostituiscono i comandi PORT e PASV.

Inoltre, la violazione del layering causa problemi nel caso in cui il Client si trovi in un ambiente di rete in cui vengono usati indirizzi IPv4 privati e NAT, poiche' il Server riceve dal Client un IP privato che ovviamente non e' raggiungibile.

In FTP passivo, il Server deve essere raggiungibile su un range di porte piuttosto esteso.

Questo crea molti problemi alla configurazione del firewall, in quanto non si puo' restringere il traffico in ingresso al Server a un insieme limitato di porte come invece richiederebbero le basilari test practice in ambito sicurezza.

Se un utente sbadato o un attaccante aprisse un server insicuro su una delle porte nel range di utilizzo da FTP non ci sarebbe alcuna protezione contro eventuali attacchi da parte del firewall.

In generale, FTP rappresenta un perfetto case study su come NON progettare protocolli applicativi.

### La posta elettronica
Il servizio di posta elettronica permette lo scambio di e-mail tra utenti in Internet. E' un servizio **asincrono** (a differenza di telnet e ftp), anche se si appoggia su TCP (porta 25).

Componenti del sistema di posta:
- Programmi applicativi $\rightarrow$ Thunderbird, Eudora, Outlook, etc.
- Protocollo applicativo o di trasferimento $\rightarrow$ SMTP
- Protocolli di accesso alle caselle di posta $\rightarrow$ POP3, IMAP
- Protocolli di specifica del formato dei dati $\rightarrow$ RFC822, MIME, etc.
- Protocollo di comunicazione $\rightarrow$ TCP.

#### Architettura del servizio di posta elettronica
I componenti principali sono:
- **User Agent (UA)**, che si interfacciano con l'utente
- **Mail Transfer Agent (MTA)**, i server di posta

E-mail parte da UA mittente e arriva a UA destinazione attraverso la rete dei MTA. Utilizzo del protocollo di trasferimento SMTP per la consegna della posta alla casella del destinatario (**modello push**);
- MTA usano SMTP
- UA in trasmissione usa SMTP

Utilizzo di protocolli specifici, come POP3 o IMAP, per la ricezione della posta, ovverosia per il trasferimento della posta dalla casella di posta dell'utente allo UA che ne permette la consultazione (**modello pull**).

![[ArchitetturaPostaElettronica.png]]

#### Posta elettronica e DNS
Il funzionamento del sistema di posta elettronica dipende dal supporto offerto dalla risoluzione di nomi di tipo **Mail eXchange (MX)** del DNS.

In particolare, attraverso la risoluzione di nomi di tipo MX, il DNS permette a un Server SMTP di scoprire **i nomi logici dei Server SMTP di un determinato dominio**.

Senza il supporto alla risoluzione MX, sarebbe impossibile scoprire quale sia il Server SMTP di un particolare dominio.

Si noti che i Server SMTP di un dominio sono tipicamente replicati. Piu' precisamente, abbiamo **almeno 2 server: 1 master e 1+ slave**.

#### Formato dei messaggi
Un messaggio, un e-mail, e' composto da un header e da un body

**Header**. L'intestazione e' composta da una serie di righe, ognuna composta da un tipo : valore, tra i quali:
- From: indirizzo mittente
- To: mailbox destinatario
- Date: data di spedizione
- Subject: soggetto del messaggio
- Cc: copia destinatari
- Replay-To: indirizzo per la risposta
- Message-Id: identificatore unico del messaggio

**Body**. Il corpo del messaggio e' il testo dell'e-mail, in formato ASCII

**Indirizzi di posta elettronica**: destinatario come username e nome provider

**pseudonimi (aliases)** e **mail forwarding**.

##### Formato dei messaggi (MIME)
La RFC 822 standardizza messaggi di solo testo. **MIME** estende RFC 822 per consentire invio di dati multimediali (audio, immagini, video, documenti word, etc.), che devono essere transcodificati in u formato US-ASCII compatibile.

MIME ha 3 parti fondamentali:
- Righe di intestazione (integrano header di RFC 822) per definire il tipo di dato
- Definizione dei tipi di dati contenuti (Content-Type)
- Indica la transcodifica (Content-Trans-Encoding) usata per i vari dati, in modo che possano essere trasmessi in una e-mail (che usa solo caratteri ASCII). Conversione di formati binari in ASCII (ad esempio, tramite la codifica `base64`; per conversioni da testo UTF-8 ad ASCII spesso si usa codifica di *quoted-printable*)

##### Protocollo applicativo SMTP (RFC 2821)
Standard per il trasferimento della mail, con messaggi codificati tra client e server.

![[ProtocolloApplicativoSMTP.png]]

Comandi cliente - Risposte Server
**sender**      `MAIL FROM: nome mittente`
**receiver**    `250 OK`

**sender**      `RCTP TO: nome destinatario`
**receiver**     `250 OK  abilitato` 

**sender**       `DATA`
			`linee di testo del messaggio`
**sender**       `< cr-lf>.< cr-lf>  fine messaggio`
**receiver**     `250 OK`

I ruoli tra sender e receiver (o client e server) possono essere invertiti per trasmettere la posta diretta nel verso opposto.

I **comandi** sono stringhe ASCII, tra cui: HELO, MAIL, RCPT, DATA, QUIT

Le **risposte** sono codificate con 3 cifre e un testo descrittivo:
- La prima cifra codifica le interazioni
	- 1xx $\rightarrow$ Comando accettato
	- 2xx $\rightarrow$ Risposta positiva completa
	- 3xx $\rightarrow$ Risposta positiva intermedia
	- 4xx $\rightarrow$ Risposta negativa transitoria (il comando puo' essere ripetuto)
	- 5xx $\rightarrow$ Risposta negativa permanente
- La seconda cifra codifica le risposte
	- x0x $\rightarrow$ Sintassi
	- x1x $\rightarrow$ Informazione
	- x2x $\rightarrow$ Connessione
	- x3x e x4x $\rightarrow$ Codici non specificati
	- x5x $\rightarrow$ Mail System (stato del receiver)

La terza cifra specifica piu' precisamente.

### SINCRONIZZAZIONE CLOCK - Network Time Protocol (NTP)
NTP permette di sincronizzare il clock tra una molteplicita' di nodi su scala globale, con accuratezza di pochi millisecondi (uso di Precision Time Protocol, PTP, invece per applicazioni che richiedono accuratezza sub msec, su reti locali).

Organizzare gerarchia di server su diversi livelli (strata), con pochi server di livello piu' alto sincronizzati con orologi precisissimi e "ufficiali" e bassi carichi di servizio, e molti server di livello piu' basso che sopportano carichi piu' elevati. 

NTP usa transazioni di richiesta clock, in cui un Client richiede richiede l'ora corrente da un Server, inserendo il proprio tempo con la richiesta. Il server aggiunge il suo tempo al pacchetto dei dati e lo ritrasmette al client.

Quando riceve il pacchetto, il Client puo' ricavare due informazioni essenziali:
- il tempo di riferimento al Server
- il tempo trascorso (misurato dall'orologio locale)
Affinché un segnale passi dal Client al Server e viceversa. Iterazioni ripetute di questa procedura consentono al Client di rimuovere gli effetti del jitter di rete e quindi ottenere un valore stabile per il ritardo tra l'orologio locale e lo standard dell'orologio dio riferimento sul Server

Questo valore puo' quindi essere utilizzato per regolare l'orologio locale in modo che sia sincronizzato con il Server. Ulteriori iterazioni di questo scambio di protocollo possono consentire al client locale di correggere continuamente l'orologio locale per indirizzare lo sfasamento dell'orologio locale.

![[NTP.png]]

Sincronizzazione con molti Server allo stesso tempo e sintesi delle informazioni con procedure ben definite e resistenti alla perdita di pacchetti.

NTP usa UDP sulla porta 123.
Il server NTP e' stateless e risponde a ciascun pacchetto NTP del client ricevuto in modo transazionale semplice aggiungendo campi al pacchetto ricevuto e ritrasmettendo il pacchetto al mittente originale, senza riferimento a precedenti transazioni NTP.

### ISTANT MESSAGING - XMPP
XMPP e' un protocollo che permette la comunicazione real-time di messaggi e di informazioni di stato (assente, occupato, ...).
- I client si registrano presso server locali. 
- Routing di messaggi e architettura simile a SMTP.
- RFC 3920, 3921, ...
- Porta standard TCP 5222

#### Applicazioni di XMPP
XMPP e' pensato per molti tipi di applicazioni:
- Instanti messaging
- Group chat
- Gaming
- Controllo sistemi
- Location-based services
- Middleware e cloud computing
- Data syndication (RSS, aggiornamenti di stato su siti di Web 2.0)
- VoIP (Google Voice)
- Servizi di identita'

#### Protocollo XMPP
Lo scambio di messaggi in XMPP e' asincrono. Il client apre una sessione con il server, gli comunica la propria presenza, e poi inviare e ricevere messaggi.

Il protocollo di comunicazione usato da XMPP e' basato su XML. Esempio:
``` XML
Client: <stream:stream>
Client: <presence/>
Client: <message from=“studente@unife.it”
				 to=“mauro.tortonesi@unife.it”>
			<body>Quando si terrà l'esame scritto?</body>
		</message>
Server: <message from=“mauro.tortonesi@unife.it”
				 to=“studente@unife.it”>
			<body>Martedì prossimo.</body>
		</message>
Client: </stream:stream>
```

### X Window
X Window e' un sistema per l'accesso in modalita' grafica ad applicazioni che risiedono su macchine remote.

Architettura client-server con inversione dei ruoli: il server e' la macchina locale e il client e' la macchina su cui eseguono le applicazioni remote.

### VoIP e Multimedia Streaming
SIP e RTP vengono usati come protocolli di controllo e di trasporto dei dati in applicazioni Voice over IP (VoIP) e di Multimedia Streaming.

#### Real-Time Transport Protocol (RTP)
Real-Time Transport Protocol (RTP) e' un protocollo utilizzato per servizi di comunicazione in tempo reale su Internet, come audio e video interattivi.

RTP fornisce diverse funzioni:
- identificazione del payload type
- numerazione sequenziale
- timestamping
- monitoring
- ...

Basato sul protocollo UDP, viene usato con RTP Control Protocol (RTCP) che monitora la qualita' del servizio.

#### Session Initiation Protocol (SIP)
Session Initiation Protocol (SIP) e' un protocollo applicativo usato per creare, modificare, e terminare sessioni interattive di comunicazione tra uno o piu' partecipanti, come chiamate VoIP, multimedia streaming, e videoconferenze.

SIP instaura o termina chiamata video o vocali e permette di modificare le caratteristiche di chiamata in corso, di inviare ulteriori partecipanti e aggiungere o cancellare stream multimediali. Tradizionalmente basato su UDP (porta 5060), recentemente viene utilizzato anche su TPC e TLS.

Elementi principali: user agent, proxy server registrar, redirect server, session border controller, gateway. URI identificano ogni elemento di una rete SIP:
`sip:1-555-123-4567@mysip.com`

