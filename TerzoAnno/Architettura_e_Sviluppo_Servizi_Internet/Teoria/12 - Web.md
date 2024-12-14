## World-Wide-Web (WWW)
Struttura in forma ipertestuale delle informazioni presenti su Internet (**trasparenza** della allocazione delle informazioni e **semplicità** di utilizzo).

WWW E' un insieme di protocolli e di standard usati per organizzare e accedere alle informazioni presenti sulla rete internet.

**Finalità**:
- Trasparenza accesso e allocazione
- Mezzo per reperire informazioni
- Supporto al lavoro collaborativo
- Nuovo mercato commerciale

**Ragioni del suo successo**:
- Diffusione globale e capillare di internet
- Uguale interfaccia utente indipendente dal tipo di Server, di Client o di macchina ($\Rightarrow$ facilita' d'uso)
- Sistema modulare ($\Rightarrow$ scalabilita')
- Facilita' di navigazione (uso di **browser** come Firefox, Chrome, Safari, ...)

### Principali standard del WWW
- Sistema di indirizzamento globale, **URL** (Universal Resource Locator)
- Protocollo applicativo, **HTTP** (HyperText Transfer Protocol)
- Linguaggio di strutturazione dei dati, **HTML** (HyperText Markup Language)
- Fogli di stile per formattazione dei dati, **CSS** (Cascading Style Sheets)
- Interfacce per applicazioni Web (**CGI** ed evoluzioni)
- Piattaforma di programmazione e run time environment per esecuzione programmi lato Client, **JavaScript**
- Moltissimi altri standard, in continua evoluzione (XHR, WebSocket, WebRTC, HTTP/2, HTTP Live Streaming, Dynamic Adaptive Streaming over HTTP, etc.)

#### Architettura WWW
![[ArchitetturaWWW.png]]

**Componenti**:
- Browser Web (Client per prestazione/gestione richieste)
- Server Web (recupero e invio informazioni)
- Applicazioni Web (esecuzione remota)
- Run time environment, application framework (sia Server che Client side), ...

#### URL - Uniform Resource Locator
Nomi unici per le risorse del sistema, indicati da Client per recupero Server.

**Uniform Resource Locator**:
- nodo contenente la risposta (documento o dati)
- protocollo di accesso alla risorsa (e.g. http, ftp)
- numero di porte TCP (porta di default del servizio)
- localizzazione della risorsa nel Server

`<protocollo>[://<host>][:<porta>][<percorso>]`

Sono riconosciuti i servizi Internet e relativi protocolli (http, ftp, telnet, nntp, mail, ...)

#### HTTP - HyperText Transfer Protocol 1.0 (RFC 1945)
**HTTP** e' il **protocollo applicativo** per i trasferimento delle informazioni tra Client e Server. Caratteristiche della versione 1.0:
- **request/response** $\rightarrow$ semplice richiesta e ricezione di dati
- **connessione non permanente** $\rightarrow$ in HTTP 1.0 la connessione TCP viene mantenuta solo per il tempo necessario a trasmettere i dati
- **stateless** $\rightarrow$ non mantiene nessuna informazione tra una richiesta e la successiva, a differenza di **FTP** che mantiene una sessione durante la connessione e ricorda, per es., la dir in cui l'utente si trova

Motivazioni progettuali:
- Diversi link nella stessa pagina possono puntare a pagine di altri Server e si ritiene inutile stabilire delle connessioni piu' durevoli e costose.

Problemi:
- in HTTP 1.0, una pagina con molte figure viene trasferita con una separata connessione TCP per ognuna di esse.
- la modalita' di funzionamento  stateless rappresenta un problema per l'implementazione di transazioni commerciali.

##### HTTP request/response
Formato del messaggio di richiesta: < indirizzo del server > | < metodo della richiesta> | < campi opzionali > | < path + nome file >

**Metodo di richiesta**:
- GET $\rightarrow$ richiesta di leggere una pagina Web
- HEAD $\rightarrow$ richiesta di leggere l'header di una pagina Web
- PUT/PATCH $\rightarrow$ richiesta di pubblicare/modificare una pagina Web
- POST $\rightarrow$ append dei dati (di solito HTML form)
- DELETE $\rightarrow$ rimuove una pagina Web

**Campi opzionali**:
- form $\rightarrow$ identita' dell'utente richiedente (debole forma di autenticazione degli accessi)
- referer $\rightarrow$ il documento contenente il link che ha portato al documento corrente

###### HTTP - Codifica risposte
Come in FTP e SMTP, le risposte sono codificate con 3 cifre:
La prima cifra codifica le interazioni
- 1xx $\rightarrow$ Informational responses (100 - 199)
- 2xx $\rightarrow$ Successful responses (200 - 299)
- 3xx $\rightarrow$ Redirection responses (300 - 399)
- 4xx $\rightarrow$ Client error responses (400 - 499)
- 5xx $\rightarrow$ Server error responses (500 - 599)

In HTTP, al codice di risposta seguono le informazioni sull'oggtto e l'oggetto stesso.

##### Interazione Client browser / Server web
1. Il browser (HTTP Client) riceve un URL dall'utente
2. Il browser richiede al DNS di risolvere il nome della macchina specificata nell'URL
3. Il DNS risponde con l'indirizzo IP
4. Il browser stabilisce una connessione TCP sulla porta 80 dell'indirizzo IP del Server
5. Il browser manda una richiesta con metodo GET `percorso_pagina_web`
6. Il Server risponde con la pagina Web richiesta.

Il Client HTTP puo' eseguire richieste seguendo protocolli differenti (es. FTP) su porte differenti

##### Evoluzioni di HTTP
- **HTTP 1.1** e' la release attuale di HTTP. Ha introdotto il concetto di "connessioni persistenti", in cui **una singola connessione TCP e' utilizzata per trasferimenti multipli** (molte GET)
- **HTTPS** fornisce un trasferimento sicuro delle informazioni, garantendo la riservatezza delle informazioni scambiate in una sessione HTTP. Fa uso di un sottostante canale cifrato realizzato con TLS
- **WAP** (Wireless Accesso Protocol) e' uno stack di protocolli per fornire l'accesso Web a dispositivi portatili, palmari, telefoni cellulari, ecc. Non solo e' un protocollo, ma tutta una suite per affrontare diversi aspetti. Contiene un linguaggio di markup (WML), un protocollo di trasporto, un protocollo di sicurezza, etc. Dei WAP proxy sono incaricati di trasformare le esistenti pagine HTML in pagine WML per la visualizzazione sui dispositivi mobili. WAP e' stato un fallimento, poiche' partiva da assunzioni di scenario rivelatesi sbagliate.
- **HTTP/2 e HTTP/3** nuove versioni di HTTP, recentissime e ancora in fase di adozione inziale. Protocolli binari, risp. su TCP e QUIC, con supporto a request multiplexing. 

#### HTML (HyperText Markup Language)
**HTML** e' il linguaggio di specifica delle informazioni che deriva da SGML. E' un **markup language**.
I linguaggi di markup definiscono delle aree di testo attraverso l'uso di **tag**. I diversi tag determinano come verra' trattato il testo incluso.
Caratteristica innovativa di **HTML** e' la possibilita' di creare e definire dei collegamenti (link) verso altri documenti, anche remoti.
**Tag** definiti **funzionalmente** e non visualmente.

#### CSS (Cascading Style Sheets)
**CSS** e' un linguaggio per la formattazione di documenti HTML.
Formattazione viene separata dalla presentazione per velocizzare lo sviluppo e facilitarne il riuso del codice.

### Dynamic Web Technologies
Le prime pagine apparse sul Web erano scritte in HTML ed erano statiche e immutabili. Evoluzione del Web verso una **maggiore dinamicità**.

Tipi di pagine:
- **Pagine statiche** $\rightarrow$ sono documenti Web il cui contenuto e' definito al momento della loro creazione e non cambia piu'. Qualunque richiesta di pagina statica fornisce sempre la stessa risposta.
- **Pagine dinamiche** $\rightarrow$ sono documenti Web che vengono creati dal Web Server solo quando un browser li richiede. In tale caso il Server Web attiva un'applicazione che crea il documento in modo dinamico e lo invia al browser. Il contenuto di un documento dinamico puo' variare di richiesta in richiesta.
- **Pagine attive** $\rightarrow$ una pagina attiva ha un documento che NON e' completamente specificato dal Server. E' un pezzo di programma che viene mandato dal Server al browser, dove viene eseguito localmente. Una pagina attiva puo' interagire con l'utente e con altre risorse ed entita', per cui il suo contenuto non e' mai statico.

#### Dynamic Web: Vantaggi e Svantaggi

|                      | Vantaggi                                                                                                                                                 | Svantaggi                                                                                                                                                                                                                                                                        |
| -------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Pagine Statiche**  | Semplicita'<br>Affidabilita'<br>Performance (e caching)                                                                                                  | Poco flessibili (modificate a ogni cambiamento)                                                                                                                                                                                                                                  |
| **Pagine Dinamiche** | Per informazioni con elevata frequenza di cambiamento (e-commerce, CMS, etc.)<br>Per il browser sono come delle pagine statiche                          | Computazione sul Server (Server piu' potenti)<br>Prestazioni piu' base (tempo di elaborazione, no cache)<br>Richiedono buone capacita' di programmazione<br>Interazione "clicca e aspetta"                                                                                       |
| **Pagine Attive**    | Interazione continua con l'utente<br>Minore carico sul Server<br>Modelli di programmazione innovativi e interessanti (Web components, declarative, etc.) | Riversano costi sui Client (browser piu' sofisticati e macchine piu' potenti)<br>Richiedono ottime capacita' di programmazione<br>Problemi di portabilita' (coinvolgono tutti i Client)<br>Problemi di sicurezza (eseguono sui Client)<br>Soluzione di elevatissima complessita' |
#### Dynamic Web: Implementazione
Il supporto alla dinamicita' (pagine dinamiche e attive) richiede delle estensioni all'architettura Client/Server del Web.
Nuovi componenti sul lato Client e/o sul lato Server per supportare pagine dinamiche e attive.

Tecnologie di supporto per pagine dinamiche:
- Applicazioni CGI
- Piattaforme di sviluppo applicazioni Web (PHP, Ruby on Rails, JSP, ASP, ...)

Tecnologie di supporto per pagine attive:
- JavaScript, XMLHttpRequest, ...

#### I tre tier piu' in dettaglio
- **Presentation Tier**
	- Il Web server effettua il parsing e la sanitizzazione delle richieste HTTP, serve i contenuti statici e inoltra le richieste di pagine dinamiche all'Application Server.
- **Application tier**
	- L'Application Server si occupa di eseguire la business logic dell'applicazione Web e di generare le pagine dinamiche.
- **Data tier**
	- Il DB fornisce le funzioni di gestione dei dati richieste dall'applicazione Web.

#### Tecnologie a supporto pagine dinamiche
**Common Gateway Interface (CGI)**
- CGI e' uno standard per interfacciare un Server Web con applicazioni esterne (residenti sulla macchina)
- CGI fornisce all'utente la capacita' di eseguire un'applicazione sulla macchina Server remota 
- CGI furono prima tecnologia per pagine dinamiche. Importanza storica, molte altre tecnologie ora in uso.

##### Programmazione CGI
CGI permette a utenti di seguire applicazione sul nodo dove risiede il Server Web.
Applicazioni CGI possono essere scritte in qualunque linguaggio.
Si attiva un programma Unix, modello filtro con variabili predefinite.

CGI come Interfaccia tra Server Web e applicazione CGI, attenzione a:
- variabili di ambiente
- linea di comando
- standard input
- standard output

**Variabili di ambiente**
- utilizzate dal Server per dare informazioni di servizio ad applicazioni CGI
- SERVER_SOFTWARE $\rightarrow$ nome e versione del Server HTTP
- SERVER_NAME $\rightarrow$ nome nodo Server o suo indirizzo
- GATEWAY_INTERFACE $\rightarrow$ versione interfaccia CGI cui il Server aderisce
- REQUEST_METHOD $\rightarrow$ metodo invocato nella richiesta
- REMOTE_HOST, REMOTE_ADDR, AUTH_TYPE, REMOTE_USER, CONTENT_TYPE, CONTENT_LENGHT, ...

**Linea di comando**
- per richieste di tipo ISINDEX, per richiedere del testo nei documenti. Le parole da ricercare sono inserite dal Server sulla linea di comando dell'applicazione CGI.

**Standard Input**
- il Server ridirige sull'ingresso dell'applicazione CGI i dati ricevuti dal Client (browser). Il numero di byte e' nella variabile d'ambiente CONTENT_LENGHT, il tipo dei dati MIME nella CONTENT_TYPE.

**Standard Output**
- l'applicazione CGI manda il risultato dell'elaboraione sullo standard output verso il Server, che a sua volta prepara i dati e li spedisce al Client.

###### CGI: problemi, limiti e costi
A ogni richiesta, viene attivato un processo per gestione CGI (overhead di generazione).
Evoluzioni: **FAST CGI**prevedono un processo gia' attivo per ogni servizio CGI specificato.

#### Oltre CGI: Interfacce Moderne tra Tier 1 e 2
Con lo sviluppo delle tecnologie Web, sono nate interfacce molto piu' moderne, sofisticate e performanti di CGI per la connessione tra Web Server e Application Server:
- Varianti avanzate di CGI (es. `FastCGI` Process Manager di PHP)
- HTTP stesso, con Application Server che opera in modalita' reverse proxy
- Moduli di estensione dei Web server specifici per una particolare piattaforma di sviluppo.

All'interno dell'Application Server, le interfacce tra l'ambiente di esecuzione e le applicazioni vere e proprie sono tipicamente platform-specific

#### Java Server Pages (JSP)
JSP e' stata una delle prime tecnologie per la realizzazione di pagine dinamiche.
Parte della pagina HTML viene specificata utilizzando codice Java. Queste parti sono passate alla macchina virtuale integrata nel Server, permettendo lo sviluppo di pagine HTML dinamiche (usando servlet)

1. Pagine JSP sono estensioni di HTML che contengono parti dinamiche specificate in Java tra tag `<% %>`
2. Il codice Java passato alla macchina virtuale integrata nel Server per produrre una **servlet**
3. Si **compila** "on the fly" il codice Java e si **attiva** la servlet
4. Si produce la pagina HTML risultato.

##### Java Servlet
Le **servlet** sono estensioni di attivita' in esecuzione sul Server e integrabili facilmente con il Server Web (via JVM).
Le **servlet** sono componenti di codice Java residenti sul Web Server se **invocate** producono attivita' nella JVM eseguendo come processi leggeri.

$+$ Costi di attivazione molto limitati
$+$ Non usciamo dall'ambiente del Server
$+$ Possiamo gestire facilmente mutua esclusione o parallelismo

$-$ Architettura **eccessivamente complicata** (almeno per le limitate funzioni che offre)
$-$ Portabilita' piu' teorica che pratica

#### Java Server Pages
come collante per unire:
- codice HTML
- componenti riusabili (Enterprise JavaBeans)
- applicazioni remote (servlet)
- codice Java
- script basati su linguaggio Java

Le **JSP** sono portabili in quanto non assumono una specifica architettura di Web Server (come le ASP), ma solo la presenza di una JVM.

#### Altre piattaforme
**PHP**
- Tecnologia per generare pagine dinamiche, molto diffusa nel mondo Linux.

**Active Server Page (ASP)**
- Definite da Microsoft per integrare HTML e componenti script (VB, C#). Supportate da Microsoft **IIS** Internet Information System

**Ruby on Rails**
- Framework che rappresenta un punto di riferimento tecnologico del mercato. Basato sul linguaggio Ruby, ha avuto il merito di introdurre innumerevoli innovazioni che sono state successivamente recepite dai competitor.

**Node.js**
- Piattaforma di nuova generazione, basata sull'uso del linguaggio JavaScript (lato Server) e di modelli di programmazione ad eventi.

FARE DA 19 IN POI