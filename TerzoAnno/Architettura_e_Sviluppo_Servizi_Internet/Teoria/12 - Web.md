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

FARE DA 4 IN POI