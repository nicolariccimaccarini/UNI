## La comunicazione tra processi in una rete di calcolatori
Un'**applicazione distribuita** e' costituita da processi distinti per localita' che comunicano e cooperano attraverso lo scambio di messaggi per ottenere risultati coordinati

Quali sono gli aspetti principali per la realizzazione di un'applicazione distribuita?
- Identificazione dei processi (nomi)
- Primitive di comunicazione tra processi
- Sincronizzazione dei processi
- Comunicazioni con/senza connessione
- Affidabilita'
- Eterogeneita' e Formato dei dati
- Modelli di interazione (Client/Server e altri modelli)

### Identificazione dei processi comunicanti
Necessita' di definire un sistema di identificazione per la diverse unita'.
Il primo problema da affrontare riguarda l'**identificazione dei processi comunicanti** nella rete.
Per ogni processo bisogna definire un **nome globale** univoco e sempre non ambiguo. es.
	"nome" macchina + "nome" processo all'interno della macchina

Questo problema e' risolto dai protocolli di comunicazione sottostanti, che nel caso di Internet sono i protocolli di trasporto TCP/UDP e di rete IP
	Nome macchina = indirizzo IP (o nome logico)
	Nome processo = pid e' inadeguato, uso di porte

#### Identificazione dei processi comunicanti in Internet
- Una macchina e' identificata univocamente da un **indirizzo IP** (4 bytes, es 192.167.215.12)
- La **porta** e' un numero di 16 bit che rappresenta un identificativo univoco di servizio, che rende possibile identificare un processo senza dover conoscere il suo pid
- I messaggi sono consegnati su una specifica porta di una macchina, non direttamente a un processo
- Un processo si lega a una porta per ricevere o spedire messaggi

### Primitive di comunicazione
Molte primitive e modelli di comunicazione con scelte diverse su due dimensioni:
- **designazione** dei processi sorgente e destinatario della comunicazione
	- schemi **diretti simmetrici**
	- schemi **diretti asimmetrici**
	- schemi **indiretti**
- **tipo di sincronizzazione** tra i processi comunicanti
	- comunicazione **sincrona**
	- comunicazione **asincrona**

Queste due caratteristiche sono ortogonali (le soluzioni sono tra loro indipendenti)

#### Designazione di sorgente e destinatario - schemi diretti simmetrici
I processi si nominano **esplicitamente**:
``` C
send(messaggio) to Pdest
recive(&messaggio) from Psorg
```

es.
``` C
. . . .
pipe(p);
if ((pid = fork()) == 0) { /*FIGLIO*/
	close(p[0]);
	num = servizio();
	write(p[1], &num, dim);
	exit(0);
}
close(p[1]);
read(p[0 ris, dim);
. . . .
```

#### Designazione di sorgente e destinatario - schemi diretti asimmetrici
Il mittente **nomina esplicitamente** il destinatario ma questi al contrario non esprime il nome del processo da cui desidera ricevere messaggi.
Schema molti a uno (modello Client/Server). I processi cliente specificano il destinatario delle loro richieste. Il processo servitore e' pronto a ricevere messaggi da qualunque cliente.

``` C
Pi(Cliente) Pj(Servitore)
... ...
send (msg) to Pj receive (&request, &Pi)
	<esecuzione del servizio>
receive (&ris) from Pj send (response) to Pi
```

Gli schemi asimmetrici sono utili per comunicazioni sia da uno a molto che da molti a molti.

### Designazione di sorgente e destinatario schemi indiretti
``` C
send(msg, M)

receive(msg, M)
```

La comunicazione avviene **tramite un oggetto mailbox M**.
Il supporto mette a disposizione delle chiamate a sistema per
- creare mailbox
- inviare e ricevere messaggi alla/dalla mailbox
- distruggere la mailbox

### Tipi di sincronizzazione tra processi - Modalita' Sincrona e Asincrona
In un'applicazione distribuita, ogni processo esegue senza conoscere lo stato di esecuzione degli altri. Quando due processi comunicano, sorge un problema di sincronizzazione.

La sincronizzazione e' legata all'uso di primitive Sincrone o Asincrone.

Una primitiva sincrona BLOCCA il processo fino al termine della sua esecuzione.

Il blocco e lo sblocco delle primitive e' ovviamente realizzato e gestito dal supporto alla comunicazione senza intervento del programmatore, che deve pero' selezionare le primitive con la modalita' piu' opportuna per l'applicazione.

#### Sincronizzazione tra processi - Modalita' Sincrona
Il processo si **blocca** in attesa del completamento dell'operazione richiesta:
- **send sincrona** $\rightarrow$ processo mittente si blocca fino a che il messaggio viene ricevuto dal destinatario
- **receive sincrona** $\rightarrow$ processo destinatario si blocca fino alla ricezione del messaggio

In questo caso, un messaggio ricevuto contiene informazioni corrispondenti allo **stato attuale** dell'altro processo.

Possibili problemi di **deadlock**. Uso di timeout o di multithreading.

#### Sincronizzazione tra processi - Modalita' Asincrona
Il processo **continua la sua esecuzione** immediatamente dopo che l'operazione e' stata invocata:
- **send asincrona** $\rightarrow$ processo mittente continua esecuzione dopo la send, senza sapere se il destinatario ha ricevuto il messaggio.
- **receive asincrona** $\rightarrow$ processo destinatario esegue la receive senza bloccarsi ma nel buffer di ricezione non e' detto che ci sia un messaggio.
  Se il messaggio non e' arrivato:
	- polling $\rightarrow$ si invoca continuatamente la receive
	- gestione ad eventi $\rightarrow$ il supporto IPC notifica al processo ricevitore la presenza di messaggio da ricevere

Non c'e' garanzia che il messaggio sia stato ricevuto.
Il messaggio ricevuto non contiene informazioni che possano essere associate allo **stato attuale** del mittente.

### Sincronizzazione e Buffering
Tipicamente si effettua il buffering dei messaggi che porta a una semantica di sincronizzazione leggermente diversa per le primitive send e receive.

Piu' precisamente, prima della trasmissione o della consegna all'applicazione dei dati letti i messaggi scambiati attraverso primitive IPC risiedono in una memoria temporanea all'interno del kernel. Questo consente di accorpare le operazioni di I/O con notevole impatto sulla performance dell'applicazione.

Il buffering comporta un tradeoff $\rightarrow$ a migliori performance corrisponde un minore controllo.

#### Sincronizzazione e Buffering: caso send
![[sincBuffSend.png]]

#### Sincronizzazione e Buffering: caso receive
![[sincBuffReceive.png]]

### Comunicazione con/senza connessione
Applicazioni distribuite fanno uso di canali di comunicazione con connessione o senza connessione:
- **Con connessione** $\rightarrow$ viene stabilita una connessione tra i processi e chiamato. La connessione puo' essere sia logica che fisica, stabilita la connessione i dati possono fluire correttamente tra i processi.
- **Senza connessione** $\rightarrow$ non c'e' connessione tra i processi. Ogni messaggio deve singolarmente essere indirizzato e consegnato al processo destinatario.

### Affidabilita' della connessione
Il supporto di Inter Process Communication utilizzato puo' fornire diverse garanzie nella consegna dei messaggi:
- **Comunicazione affidabile** $\rightarrow$ nel caso in cui il supporto garantisca la consegna dei messaggi. In caso di perdita di messaggi o guasti, il supporto tipicamente ritrasmette il messaggio.
- **Comunicazione non affidabile** $\rightarrow$ in questo caso il supporto invia i messaggi senza verificarne la consegna.


![[riassunto1.png]]

## Internet e il Web come insieme di servizi
### Il modello Client/Server
Il modello Client/Server prevede due entita':
- l'entita' **Client** che richiede il servizio
- l'entita' **Server** che eroga il servizio

La parte Client del servizio:
- e' un processo lanciato direttamente dall'utente
- contatta attivamente il server
- non richiede dispositivi dedicati o sistemi operativi sofisticati

La parte Server del servizio:
- e' un processo specializzato dedicato all'erogazione del servizio
- puo' servire molti client contemporaneamente
- e' avviamo automaticamente al boot della macchina
- e' eseguito su un calcolatore "condiviso"
- attende passivamente le richieste dei Client
- accetta richieste da molti Client ma offre un solo servizio
- Richiede una macchina potente e un sistema operativo sofisticato

Il modello Client/Server risolve il problema del **rendez-vous** (sincronizzazione iniziale tra i processi comunicanti) definendo il **Server** come un processo sempre in attesa di richieste di servizio.

### Identificare i servizi
I Client devono poter **specificare il servizio** desiderato senza ambiguita'.
A questo scopo:
- I Server **registrano** il servizio
- I Client devono reperire **l'identificatore** del servizio
- I Client devono usare l'identificatore per **contattare** il Server e usufruire del servizio

### Il progetto del Client e del Server
Il Server deve accedere alle risorse del sistema:
- problemi di **autenticazione** utenti
- **autorizzazione** all'accesso
- **integrità** dei dati
- **privacy** delle informazioni
e deve gestire richieste contemporanee da molti Client (Server **concorrenti**)

### Tipi di interazione tra Client e Server
Due tipi principali di iterazioni:
- **connection oriented** $\rightarrow$ viene stabilito un canale di comunicazione virtuale prima di iniziare lo scambio dei dati
- **connectionless** $\rightarrow$ non c'e' connessione virtuale, ma semplice scambio di messaggi

La scelta tra i due tipi dipende dal tipo di servizio che si vuole realizzare.

Client e Server sono processi diversi, con diversi possibili comportamenti:
- **Modello pull** $\rightarrow$ il client richiede l'informazione al Server e in genere si blocca e aspetta la risposta
- **Modello push** $\rightarrow$ il client segnala il proprio interesse e poi fa altro. E' compito del Server di inviare l'informazione se e quando disponibile

In alternativa al modello push, il Client potrebbe eseguire un *polling* non bloccante.
Il modello pull e' Server-driven e **asincrono**, mentre il modello polling e' Client-driven e **asincrono**.

### Lo STATO dell'interazione tra Client e Server
Uno degli aspetti centrali nella realizzazione di un'applicazione Client/Server e' la tipologia dell'interazione per quanto riguarda la **gestione di richieste multiple di servizio**, che puo' essere di due tipi:
- **stateful** $\rightarrow$ viene mantenuto lo stato dell'interazione e quindi una richiesta di servizio dipende da quelle precedenti
- **stateless** $\rightarrow$ non si tiene traccia dello stato, ogni richiesta e' completamente indipendente dalle altre e auto contenuta

Lo stato e' quindi una sintesi, memorizzata da una delle parti, di come l'attuale sessione di servizio sta procedendo. 

#### Servizi Stateless
- Nei servizi stateless ogni messaggio deve contenere tutte le informazioni per svolgere la richiesta di servizio. Quindi, lo stato della sessione di servizio deve essere esplicitamente contenuto (tracciato) all'interno dei messaggi di richiesta.
- Lavorando con messaggi autocontenuti, i servizi stateless sono tipicamente piu' robusti.
- I modelli di interazione stateless semplificano il progetto del Server, ma la complessita' viene riportata sui Client e sull'infrastruttura di rete. 
- Un'interazione stateless e' possibile SOLO se il **protocollo applicativo** e' progettato con operazioni idempotenti.

#### Servizi Stateful
- Nei servizi stateful, Client e Server tengono traccia localmente, su un database, dello stato dell'iterazione. Tipicamente il database di riferimento e' quello del Server.
- Ogni messaggio puo' assumere che l'altra parte sia gia' a conoscenza dello stato di partenza, quindi si consumano meno risorse.
- I modelli di interazione  richiedono al Server di mantenere lo stato della interazione per ogni Client connesso, rendendo piu' difficile l'adozione di tecniche di scalabilita' orizzontale basate sulla replicazione del Server

#### Esempio di operazioni I/O idempotenti e non
Le system call `read` e `write` di UNIX non sono idempotenti perche' ogni chiamata va a cambiare l'I/O pointer che verra' usato per la chiamata successiva:
``` C
ssize_t read(int fildes, void* buf, size_t nbyte);
ssize_t write(int fildes, const void *buf, size_t nbyte);
```

Lo standard POSIX pero' introduce anche due funzioni equivalenti al `read` e `write` con semantica idempotente: `pread` e `pwrite`.
``` C
ssize_t pread(int d, void *buf, size_t nbyte, off_t offset);
ssize_t pwrite(int fildes, const void *buf, size_t nbyte, off_t offset);
```

#### Esempio di stateless vs stateful: NFS
Si considerino le differenze tra un file server stateless (Network File System versione 3) e stateful (Network File System versione 4).

NFS e' un servizio basato su chiamate a procedura remota (RPC), ovvero operazioni che eseguono lato server ma che sono invocabili remotamente dal cliente come se fossero una chiamata a funzione locale.

NFSv3 e' basato su operazioni **stateless**:
- Operazione LOOKUP idempotente per ottenere file handle 
  `file_handle = LOOKUP(dir_handle, filename)`
- Operazione READ idempotente (si deve sempre specificare I/O pointer offset)
  `data_and_metadata = READ(file_handle, offset, count)`
- Operazione WRITE idempotente (si deve sempre specificare I/O pointer offset)
  `result_metadata == WIRTE(file_handle, offset, count, data)`
- Necessita' del servizio ausiliario NLM (Network Lock Manager) per gestire file locking (fornito da due processori demone)

NFSv4 e' basato su operazioni **stateful**
- LLOKUP sostituita da operazioni OPEN e CLOSE stateful per aprire e chiudere file
- Funzioni di file locking implementate direttamente nel protocollo da operazioni OPEN, READ e WRITE (non necessario NLM)
- Nuova operazione COMPOUND rende possibile l'aggregazione di piu' richieste di I/O un solo messaggio

Operazioni di I/O piu' robuste in NFSv3.
Integrazione locking nel protocollo permette a NFSv4 di fornire migliori prestazioni.

### Gestione stato: livello di protocollo applicativo o di applicazione?
Si noti che non e' necessariamente necessario gestire lo stato a livello del protocollo applicativo. Si puo' anche implementare la gestione dello stato a livello di applicazione al di sopra di un protocollo applicativo stateless.

Nel servizio File Transfer Protocol (FTP), lo stato della sessione corrente viene modificato e tracciato a livello di protocollo applicativo. L'elemento piu' importante tracciato dallo stato del servizio e' la working directory su cui si sta operando lato server.

Altri servizi, come il World Wide Web, sono basati su un protocollo applicativo stateless. Quindi, un eventuale stato della sessione di servizio deve essere mantenuto a livello di applicazione.

#### Il problema dello stato nel Web
Tutto il **WWW** e' modellato sul paradigma Client/Server, il browser e' il Client e le pagine Web sono ospitate sui Server.
L'interazione tra browser e Server Web e' basata sul protocollo **HTTP** che e' **stateless**, ogni richiesta di pagina e' completamente indipendente dalle precedenti.

#### Lo stato nel Web
Ci sono servizi Web che devono essere offerti soo a certi utenti e altri che richiedono piu' "passi".
Tali servizi hanno bisogno di mantenere lo stato dell'interazione tra Client e Server.
Lo **stato viene mantenuto a livello dell'applicazione**, al di sopra di HTTP.

#### Stato permanente o soft
I modelli con stato hanno il server che puo'/deve mantenere la traccia dell'interazione.
Per quanto tempo e con che costi?

Si distingue lo stato in base alla durata massima:
- **stato permanente** $\rightarrow$ mantenuto per sempre
- **stato soft o a tempo** $\rightarrow$ rimane per un tempo massimo

Si pensa a un server Web che deve mantenere le risorse per reggere tutte le richieste dei clienti che hanno acceduto o a un server che deve riconoscere tutti i clienti che sono autorizzati ad accedere (username e password) tramite tabelle di riconoscimento.

### La concorrenza nell'interazione tra Client e Server
- **Lato Client** $\rightarrow$ I Client sono programmi sequenziali, eventuali invocazioni concorrenti supportate dal sistema operativo multitasking
- **Lato Server** $\rightarrow$ La concorrenza e' cruciale per migliorare le prestazioni di un Server
	- Un **Server iterativo** processa le richieste di servizio una alla volta. Possibile basso utilizzo delle risorse, in quanto non c'e' sovrapposizione tra elaborazione e I/O
	- Un **Server concorrente** gestisce molte richieste di servizio correttamente, cioe' una richiesta puo' essere accettata anche prima del termine di quella attualmente in corso di servizio.
	  Migliori prestazioni ottenute da sovrapposizione elaborazione e I/O.
	  Migliore complessita' progettuale.

### Quale tipo di server?
![[QualeServer.png]]
- Affidabilita' come terza dimensione della tabella
- La scelta del tipo di Server e' vincolata dai **protocolli** e dalla **tecnologia** realizzativa.

### Le prestazioni di un server
Dal punto di vista del Client, definiamo il tempo di risposta $T_R$ come il ritardo totale tra la spedizione della richiesta e l'arrivo della risposta dal server.
$$ T_R = T_S + 2T_C + T_Q $$
- $T_S \rightarrow$ tempo di elaborazione (di servizio) di una singola richiesta
- $T_C \rightarrow$ tempo di comunicazione medio
- $T_Q \rightarrow$ tempo di accomodamento medio

Con **lunghe code di richieste**, il tempo di risposta puo' diventare anche molto maggiore rispetto al tempo di elaborazione della richiesta

### Prestazioni di un Server Iterativo
Nel caso di **Server Iterativo**, che risponde a una singola richiesta alla volta e accoda le altre, il tempo di risposta e' circa proporzionale alla lunghezza della coda.

Problemi:
- limitare la lunghezza della coda
- le richieste a coda piena vengono rifiutate

### Prestazioni di un Server Concorrente
Caso di **Server Concorrente**, che risponde a piu' richieste contemporaneamente.

Concorrenza riesce a migliorare il tempo di risposta:
- se la risposta richiede un tempo significativo di attesa di I/O o di sospensione del servizio con possibilita' di iterleaving;
- se le richieste richiedono tempo di elaborazione molto variabili;
- se il Server esegue un sistema multiprocessore, cioe' con reale parallelismo
$$ T_R = T_S + 2T_C + T_Q + T_I + T_G $$
- $T_C \rightarrow$ tempo comunicazione medio
- $T_Q \rightarrow$ tempo di accomodamento medio (a volte trascurabile)
- $T_I \rightarrow$ tempo di interleaving con altri servizi concorrenti
- $T_G \rightarrow$ tempo di generazione di un eventuale servitore

### Il progetto del Client e del Server
Quale parte di logica applicativa mettere sul Client o sul Server? (Fat Client vs Thin Client).
![[FatVSThin.png]]

Le applicazioni Web moderne (Single Page Web Applications, Progressive Web App, ecc.) tendono - molto spesso solo per "cargo culting" - a seguire il modello Fat Client.

Nonostante il client continui ad avere la percezione di interagire con un unico server, le funzioni svolte dal server possono essere suddivise tra componenti software di vari livelli, per **distribuire il carico di lavoro** su diverse macchine.

La seguente figura mostra un'architettura 2-tier, con un server di front-end e uno di back-end:
![[Arch2Tier.png]]
