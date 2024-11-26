I modelli a scambio di messaggi hanno un **basso livello di astrazione** e caricano il programmatore di lavoro.
**RPC** e' un modello per mascherare i dettagli di basso livello facilitando il compito dei programmatori.
Scopo di **RPC** e' di far sembrare l'invocazione di un servizio remoto come se fosse una chiamata di procedura locale. I parametri dell'invocazione sono contenuti in un messaggio di richiesta, la risposta al servizio in un messaggio di risposta.
Tutti i linguaggi imperativi hanno chiamate a procedura, per cui RPC si integra bene con i tradizionali linguaggi di programmazione, e permette di uniformare programma concentrati e distribuiti.

![[RPC.png]]

Il client e' sospeso fino al completamento della chiamata.
I parametri devono essere passati per valore, visto che i processi Client e Server non condividono lo spazio di indirizzamento (un puntatore del Client non ha significato nello spazio di indirizzamento del Server).

## Differenze RPC e chiamata a procedura locale
Vi sono importanti differenze semantiche tra RPC e chiamata a procedura locale. In una chiamata remota:
- sono coinvolti due processi diversi che:
	- **non condividono** lo spazio di indirizzamento
	- hanno vita **separata**
	- possono eseguire su macchine **eterogenee**
- Si possono verificare **malfunzionamenti**, sia nei nodi, sia nell'interconnessione

## Implementazioni RPC
Vi sono molte diverse implementazioni di RPC, con caratteristiche significativamente differenti, tra cui:
- Sun/ONC RPC $\rightarrow$ meccanismo di RPC piuttosto diffuso in ambiente Unix/Linux standardizzato da Open Network Computing usato in NFS e NIS/YP
- DCE RPC $\rightarrow$ meccanismo di RPC alla base del Distributed Computing Environment (DCE) standardizzato da The Open Group
- MSRPC $\rightarrow$ implementazione Microsoft-specific di DCE/RPC usata in molti servizi di rete e middleware (tra cui DCOM) proprietari
- XML-RPC e SOAP $\rightarrow$ basati su standard Web (XML e HTTP)
- gRPC (Google Remote Procedure Call)

Attenzione a non confondere il meccanismo generale di RPC con una sua particolare implementazione.

## Come si realizza RPC
Utilizzo di **procedure stub**:
- il **Client** invoca uno **stub** che si incarica del trattamento dei parametri e richiede al supporto il trasporto del messaggio di richiesta
- il messaggio di richiesta arriva a un **Server stub**, che estrae da messaggio i parametri, invoca la procedura e al completamento del servizio rimanda il risultato al Client

![[RPC2.png]]

### Le procedure stub
Un Client ha una procedura stub locale per ogni procedura remota che puo' chiamare. 
Un Server ha uno stub locale per ogni procedura che puo' essere chiamata da un Client remoto.

Compiti degli stub:
- **marshalling** dei parametri $\rightarrow$ impacchettare i parametri di un messaggio
- estrazione parametri dai messaggi
- **trasformare la rappresentazione** dei dati
- accedere alle primitive di comunicazione per spedire e ricevere messaggi

Gli stub sono **generati automaticamente** a partire dalla specifica dell'interfaccia.

## Interface Definition Language (IDL)
Definizione di **linguaggi astratti** per la specifica del **servizio**.
IDL sono linguaggi usati per specificare le operazioni dell'interfaccia e i loro parametri.

Es:
``` Java
interface calcolatore {
	void moltiplica([in] float a, [in] float b, [out] float *res);
	void radice([in] float a, [out], float *res);
}
```

Una specifica IDL viene elaborata da un compilatore che produce sia il Client sia il Server stub.
I compilatori per IDL possono produrre stub in differenti linguaggi.

## Binding tra Client e Server
Il binding lega l'interfacciadi RPC usata da un Client all'implementazione fornita da un Server.
Sono possibili diverse soluzioni (statico vs dinamico) per il binding tra il Client e il Server:
- **binding statico** $\rightarrow$ il binding viene usato a tempo di compilazione rimane inalterato per tutto il tempo di vita dell'applicazione.
- **binding dinamico** $\rightarrow$ una scelta dinamica consente di ridirigere le richieste sul gestore piu' scarico o quello in quel momento disponibile nel sistema. In teoria si potrebbe pensare di eseguire un nuovo binding per ogni richiesta tra Client e Server, nella pratica si usa lo stesso binding per molte richieste e chiamate allo stesso Server.

### Fasi del Binding
Due fasi principali:
- **nome servizio** $\rightarrow$ il Client specifica a chi vuole essere connesso, in termini del nome identificativo del servizio (e' la fase di **naming**, fase statica). Ogni servizio, ogni iterazione, deve essere identificata da un nome unico.
- **indirizzo del Server** $\rightarrow$ il Client deve essere realmente collegato al servitore che fornisce il servizio nel sistema al momento della invocazione (e' la fase **addressing**, fase dinamica).

In RPC il binding avviene in due fasi:
- **Naming** $\rightarrow$ risolto associando staticamente un identificatore all'interfaccia del servizio
- **Addressing** $\rightarrow$ in questa fase si cercano gli eventuali servitori pronti per il servizio. 2 possibilita':
	1. Client puo' chiedere direttamente l'indirizzo al server basandosi su broadcast/multicast
	2. Client puo' interrogare un **name server** che registra tutti i servitori

Di solito il binding avviene meno frequentemente delle chiamate stesse, in genere si usa lo stesso binding per molte richieste e chiamate allo stesso server.

### Binding e Name Server
Il **name server** consente il collegamento tra il Client e il Server.

Operazioni di ricerca, registrazione, aggiornamento, eliminazione:
- **lookup** (servizio, versione, &server)
- **register** (servizio, versione, server)
- **unregister** (servizio, versione, server)

Attenzione: se il nome del server dipende dal nodo di residenza, ogni variazione dev essere comunicata al name server.

## Naming
Diverse possibili architetture:
- **Centralizzata** $\rightarrow$ Un server centrale mantiene la lista di tutti i servizi all'interno della rete
	- Single point of failure
- **Decentralizzata** $\rightarrow$ un server in ciascun host che mantiene una lista di servizi locali
	- Il chiamante deve sapere in anticipo quale host fornisce il servizio desiderato
- **Ibrida** $\rightarrow$ Un server centrale, possibilmente replicato, che mantiene una lista di fornitori di servizi

### Naming in Sun/ONC RPC: il port mapper
In Sun/ONC RPC il naming e' completamente decentralizzato, e viene realizzato tramite il port mapper.

`portmap` e' un servizio in ascolto sulla porta $111$ che fornisce le funzionalita' di lookup e registrazione servizi. Ogni servizio e' univocamente identificato da un program ID. I servizi possono avere versioni diverse

``` sh
$ rpcinfo -p
	program vers proto port
	100000 2 tcp 111 portmapper
	100000 2 udp 111 portmapper
	100003 2 udp 2049 nfs
	100003 3 udp 2049 nfs
	100003 4 udp 2049 nfs
	100003 2 tcp 2049 nfs
	100003 3 tcp 2049 nfs
	100003 4 tcp 2049 nfs
```

## Malfunzionamenti e RPC
Un'importante differenza tra RPC e chiamate di procedure locali riguarda i possibili **malfunzionamenti**

![[MalfunzionamentiRPC.png]]

Possibili malfunzionamenti:
- la macchina del **Client** puo' andare in **crash** durante la chiamata
- i **messaggi** possono essere **persi oppure duplicati**, a causa di malfunzionamenti sul canale di comunicazione
- la macchina del **Server** puo' andare in **crash** prima della richiesta o durante l'esecuzione del servizio prima di mandare la risposta

INOLTRE
- In caso di Server stateful, in seguito al crash del Server potrebbe **perdere** lo **stato** dell'interazione con il Client
- **Orfani**, sono cosi' dette le procedure di servizio correntemente in esecuzione, ma richieste da Client andati in crash. Per servizi molto pesanti sarebbe opportuno notificare il crash del Client al Server ion modo da terminare gli orfani.

A seconda delle misure che vengono prese per fronteggiare queste guasti, si ottengono **differenti semantiche** di chiamata di procedura remota.

### Semantica may-be
Nel caso in cui **non si prendano misure di tolleranza ai guasti**, si ottiene una semantica di chiamata di procedura di tipo **may-be**.

Se la chiamata fallisce (tipicamente per il termine di un time-out) il Client non puo' desumere cosa sia successo. Possibili casi:
- perdita messaggio di richiesta
- server crash
- procedura remota eseguita ma perdita messaggio di risposta

Questa semantica e' chiamata may-be in quanto in caso di fallimento della chiamata il Client NON e' in grado di dire se la procedura remota sia eseguita o meno.

Inoltre, la chiamata ha successo, la procedura remota potrebbe essere stata eseguita anche piu' di una volta.

Di semplice realizzazione ma solleva problemi di consistenza di stato sul Server.

### Semantica at-least-once
Nel caso in cui il **Client** stub decida di **riprovare a spedire il messaggio di richiesta** un numero N di volte. Se la chiamata ha successo, la procedura e' stata eseguita una o piu' volte (richieste e messaggi duplicati), da cui at-least-once (almeno una volta).

Se la chiamata fallisce il client puo' pensare a:
- malfunzionamento permanente di rete
- server crash

Questo tipo di semantica puo' andare bene nel caso di **procedure idempotenti**, cioe' che possono essere eseguite molte volte con lo stesso effetto sullo stato di interazione C/S.

Esempio, server stateless di NFS.

### Semantica at-most-once
Il **Server** potrebbe tenere traccia degli identificatori delle richieste e **scartare richieste duplicate**. Inoltre potrebbe memorizzare il messaggio di risposta e ritrasmetterlo fino al ricevimento di un **acknowledge da parte del Client**.

Questo permette di ottenere una semantica di tipo at-most-once (al massimo una), in quanto e' garantito che in ogni caso la procedura remota e':
- non eseguita
- eseguita solo parzialmente (in caso di server crash)
- eseguita una sola volta

### Semantica exactly-once
Il Server potrebbe implementare le procedure come **transizioni atomiche**, in modo che le procedure siano eseguite del tutto o per niente.

Questo permette di ottenere una semantica di tipo exactly-once (esattamente una), in quanto e' garantito che in ogni caso la procedura remota e':
- non eseguita
- eseguita solo una volta

Questa semantica rappresenta il **caso ideale**, le implementazioni di RPC solitamente presentano semantica di tipo at-most-once.

### Malfunzionamenti Client e orfani
In caso di crash dei clienti, si deve trattare i caso degli **orfani**, con diverse strategie:
- **sterminio** $\rightarrow$ ogni orfano risultato di un crash viene distrutto
- **terminazione a tempo** $\rightarrow$ ogni calcolo ha una scadenza, oltre la quale e' automaticamente abortito
- **reincarnazione** $\rightarrow$ il tempo viene diviso in epoche; tutto quello che e' relativo a un'epoca precedente e' considerato obsoleto

## Eterogeneita' e Conversione rappresentazione dati
Eterogeneita' Internet (macchine e reti). Il Client e il Server possono eseguire su architetture diverse che usano differenti rappresentazione dei dati:
- caratteri (ASCII, ISO8859-$*$, Unicode UTF-$*$, ...)
- interi (dimensione, completamento a 1 o 2)
- lunghezza (interi di 2 o 4 byte)
- reali (lunghezza exp e mantissa, formato, ...)
- ordine byte all'interno di una parola (little endian Vs. big endian)

Per comunicare tra nodi eterogenei due soluzioni:
1. ogni nodo converte dati per il destinatario (presentazioni)
2. concordare un formato comune di rappresentazione dei dati (flessibilita')

Importanza degli standard:
**XDR** (Sun Microsystem)
ASN.1/X.680 (ITU-T/OSI)
XML (W3C)

### eXternal Data Representation (XDR)
![[XDR.png]]

XDR fornisce un insieme di procedure di conversione per trasformare la rappresentazione **nativa** dei dati in una **rappresentazione esterna XDR** e viceversa.
XDR fa uso di uno **stream**, un buffer che permette di creare un messaggio con i dati in forma XDR.
I dati vengono inseriti/estratti nello/dallo stream XDR uno alla volta, operazioni di **serializzazione/deserializzazione**

Esempio (serializzazione):
``` C
...
XDR *xdrs;
char buf[BUFSIZE];
...
xdrmem_create(xdrs, buf, BUFSIZE, XDR_ENCODE);
...
i=260;
xdr_int(xdrs, &i);
```

La **deserializzazione** avviene nello stesso modo, con le **stesse routine**, usate in verso opposto, si specifica flag `XDR_DECODE` in `xdrmem_create()`.

Funzioni bult-in di conversione:
![[XDRBuilt-in.png]]

Funzioni per tipi composti:
![[XDRTipiComposti.png]]

Attenzione: nel caso di informazioni strutturate, XDR ne esegue la serializzazione con **pointer chasing** e **flattening**.

## RPC e livelli OSI
![[RPCLivelliOSI.png]]

