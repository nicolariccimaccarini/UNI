## Definizione e Vantaggi delle Reti dei Calcolatori

Una rete dei calcolatori e' un insieme di sistemi autonomi che collaborano e comunicano per raggiungere obiettivi comuni. Le reti possono essere influenzati da fattori come la concorrenza, la mancanza di sincronizzazione e i guasti. Esempi comuni sono **internet** e **intranet**. I principali vantaggi delle reti includono:
- **Condivisione delle risorse**: permette a diversi sistemi di accedere alle stesse risorse hardware e software.
- **Affidabilità**: offre ridondanza e backup in caso di guasti
- **Applicazioni distribuite**: consente lo sviluppo di applicazioni che girano su piu' sistemi
- **Accesso facile online**: rende le risorse accessibili da diverse posizioni
- **Scalabilità**: Capacita' di gestire un numero crescente di richieste

Gli aspetti chiave da considerare sono la gestione dei guasti, la concorrenza, la trasparenza (nascondere la distribuzione) e la sicurezza.

---
## Cloud Computing

Il cloud computing permette di accedere a dati e applicazioni tramite internet, senza che questi risiedano sulla macchina locale. I dati e le applicazioni sono memorizzati su server remoti. I principali modelli di cloud computing sono:
- **IaaS (Infrastructure-as-a-Service)**: Affitto di server virtuali
- **PaaS (Platform-as-a-Service)**: Piattaforme per lo sviluppo e la gestione di applicazioni
- **SaaS (Software-as-a-Service)**: Accesso a software tramite internet
- **FaaS (Function-as-a-Service)**: Piattaforme per l'esecuzione di funzioni

Modelli evoluti includono **Edge Computing**, che sposta l'elaborazione dei dati vicino agli utenti, e **Fog Computing**, che estende l'edge computing integrando computazione, stoccaggio e gestione dei dati tra i dispositivi edge e il cloud.

---

## Applicazioni distribuite

Le applicazioni distribuite sono formate da processi separati che si scambiano messaggi per collaborare. Aspetti importanti sono:
- **Eterogeneità**: Differenza nei tipi di hardware e software3.
- **Apertura**: Facilità di estensione e cooperazione3.
- **Sicurezza**: Protezione dei dati e del sistema3.
- **Scalabilità**: Capacità di crescita senza perdere efficienza3.
- **Mobilità**: Supporto per utenti e applicazioni mobili3.

---

## Identificazione dei Processi e Tipi di Comunicazione
Un processo in remoto non puo' essere identificato direttamente, ma si usa il nome della macchina (indirizzo IP) e il nome del processo su quella macchina. Ogni macchina ha un indirizzo IP e una porta (numero che identifica il servizio). I messaggi vengono inviati alla porta corretta dove il processo e' in ascolto. I processi lavorano separatamente, senza conoscere le azioni degli altri. Quando due processi si scambiano messaggi, possono sorgere problemi di sincronizzazione. Per migliorare le prestazioni, i messaggi vengono temporaneamente memorizzati in un buffer prima dell'invio

---

## Modello Client/Server

Il modello prevede due entita':
- **Client**: richiede un servizio specificando il server
- **Server**: offre un servizio a piu' client (anche contemporaneamente)

La comunicazione puo' essere:
- **Diretta simmetrica**: i processi si nominano a vicenda
- **Diretta asimmetrica**: solo il mittente indica il destinatario
- **Indiretta**: la comunicazione avviene tramite una mailbox

La comunicazione puo' essere:
- **Sincrona**: il processo si blocca fino a quando il messaggio non e' inviato o ricevuto
- **Asincrona**: il processo continua a lavorare senza aspettare

La comunicazione puo' avvenire:
- **Con connessione**: viene stabilita una connessione stabile tra i processi
- **Senza connessione**: ogni messaggio viene inviato separatamente

La comunicazione puo' essere:
- **Affidabile**: il messaggio e' garantito come ricevuto
- **Non affidabile**: non c'e' garanzia che il messaggio venga ricevuto

Il server utilizza una comunicazione diretta e asimmetrica: il client specifica il server ma non viceversa. Il server gestisce il "rendez-vous" tra client e server. Il server deve accedere a risorse di sistema e ciò comporta problemi di sicurezza.

### Gestione dello Stato e Prestazioni del Server
Lo stato rappresenta l'interazione tra client e server. In alcune applicazioni, come il Web, lo stato e' gestito a livello di applicazione (es. cookie). Lo stato puo' essere mantenuto in modo permanente o temporaneo. Esistono server:
- **Connection-oriented**: si stabilisce la connessione virtuale prima dello scambio di messaggi
- **Connectionless**: non c'e' connessione, solo scambio di messaggi (es. email)

Modelli di richiesta:
- **PULL**: il client richiede e si blocca in attesa
- **PUSH**: il client segnala interesse e il server invia i dati quando pronti

La gestione dello stato puo' essere:
- **Stateful**: lo stato dell'interazione viene mantenuto, piu' efficiente ma complesso
- **Stateless**: non si tiene traccia dello stato, piu' sicuro ma meno efficiente

Esistono due tipi di server:
- **Iterativo**: elabora una richiesta alla volta
- **Concorrente**: Gestisce piu' richieste simultaneamente

---

## Modelli ISO/OSI e TCP/IP
Il modello ISO/OSI e' un modello a strati per gestire la complessita' delle comunicazioni. Ogni livello fornisce un servizio al livello superiore usando un proprio protocollo. Il modello TCP/IP e' una versione semplificata, concentrata sul trasporto di rete. Il modello TCP/IP e' chiamato a "clessidra" perche' centralizza le comunicazioni sul protocollo IP. Ogni servizio e' associato a una porta, accessibile tramite indirizzo IP e numero di porta.

### Livello di Trasporto: TCP e UDP
Il livello di trasporto include livelli come TCP e UDP
- **TCP (Transmission Control Protocol)**: Protocollo con connessione, affidabile, bidirezionale, con controllo di flusso e della congestione. La connessione avviene tramite "3-way handshake". Se un messaggio non viene ricevuto entro un tempo prefissato, viene ritrasmesso. La connessione viene chiusa con un segnale di "fine". Il controllo del flusso permette di rallentare l'invio dei dati nel caso il ricevitore non sia abbastanza veloce.
- **UDP (User Datagram Protocol)**: Protocollo senza connessione e non affidabile. I messaggi potrebbero arrivare o meno, non c'e' garanzia di consegna. La sua semantica e' "may-be", cioe' i messaggi potrebbero arrivare o meno, ma non c'e' garanzia di ordine o di consegna. Piu' veloce ma meno sicuro rispetto a TCP

### Indirizzi IP:
- **IPv4** e' formato da 32 bit, divisi in due parti, uno per la rete e una per il dispositivo, la lunghezza e' variabile grazie al subnetting. Ci sono circa 4 miliardi di indirizzi possibili, ma non tutti sono utilizzabili (riservati a reti private). Per comunicare con dispositivi al di fuori della LAN, si utilizzano meccanismi come IP routing. Il principale problema di IPv4 e' il numero limitato di indirizzi disponibili.
- **IPv6**, creato per risolvere i problemi di IPv4, utilizza indirizzi piu' lunghi (128 bit), struttura piu' semplice e prevede diversi scope. Rivisti alcuni protocolli per irrobustirli e aggiunta di estensioni per supportare dispositivi a basso consumo energetico. Necessario aggiornamento di firmware di rete e software per passare a IPv6.
- **NAT** **(Network Address Translation)**: converte gli indirizzi IPv4 da privati a pubblici e viceversa. Ha degli svantaggi, come la difficolta' nel rendere accessibili i servizi privati all'esterno. Dispositivi chiamati middleboxes modificano il traffico e possono creare problemi alle applicazioni.

### TCP/IP e Mobilita'
TCP/IP progettato per reti fisse, non supporta bene i dispositivi mobili poiche' gli indirizzi IP sono sia identificativi che localizzatori.
Sviluppati diversi protocolli:
- **DHCP (Dynamic Host Configuration Protocol)**: assegna dinamicamente un indirizzo IP e configura il gateway e il DNS
- **Mobile IP**: scarso successo con versione IPv4, migliore la versione **Mobile IPv6**

---

## Naming e Discovery

I nomi sono organizzati in diversi livelli per identificare le risorse. I nomi vengono associati a indirizzi tramite un sistema di binding. 2 tipi di binding:
- **Statico**: eseguito una sola volta rimane invariato
- **Dinamico**: rieseguito periodicamente per ottimizzare l'efficienza

I sistemi di nomi gestiscono il legame tra nomi e risorse in una rete.

### DNS (Domain Name System)
E' un sistema decentralizzato che mappa i nomi logici in indirizzi fisici, organizzato in una gerarchia di domini. Ogni dominio puo' delegare la gestione dei sottodomini.
La risoluzione dei nomi converte il nome in un indirizzo fisico tramite un servizio che risponde alle richieste. DNS Resolver inoltra le richieste e memorizza i risultati nella cache per ottimizzare le prestazioni.
DNS utilizza un sistema di caching distribuito per velocizzare le risoluzioni. Ogni record DNS ha un TTL, dopo il quale deve essere rimosso dalla cache.
Vantaggi:
- Affidabilita'
- Prestazioni
- Indipendenza

### URL (Uniform Resource Locator)
E' un sistema di nomi a piu' livelli che puo' coinvolgere diversi sistemi di naming. I servizi di naming collegano i nomi alle informazioni associate. I client possono essere sia quelli che cercano la risorsa, sia le risorse che devono registrarsi nel sistema. I **servizi di directory** permettono di trovare informazioni usando attributi mentre i **servizi di discovery** sono usati in ambienti locali e aiutano a gestire cambiamenti rapidi di servizi e risorse.

---

## Eterogeneita' e Rappresentazione dei Dati
Le reti sono composte da sistemi molto diversi tra loro. Per risolvere il problema dell'eterogeneità, è necessario definire una rappresentazione comune dei dati. Due approcci:
- **Funzioni di conversioni**: ogni macchina converte i dati nel formato desiderato
- **Formato comune** si concorda un formato comune per i dati, come **XDR**

### XDR (eXternal Data Representation)
Consente la conversione dei dati tra formati diversi. Utilizza uno stream per creare messaggi in formato XDR. XDR puo' gestire anche dati compressi grazie all'IDL

### Standard di Codifica dei Caratteri
Lo standard per la rappresentazione dei caratteri era US-ASCII (a 7 bit). Successivamente, e' stato introdotto lo standard ISO 10646, che definisce il set di caratteri **UCS (Universal Character Set)**. 
**Unicode** e' uno standard che definisce vari formati di encoding:
- **UTF-8**: codifica piu' usata, compatibile con ASCII, e a lunghezza variabile (1-4 byte). Efficiente per i caratteri comuni, come quelli latini, e ideale per formati come XML e JSON
- **UTF-16**: usato da sistemi come Java e Windows, rappresenta la maggior parte dei caratteri con 2 byte (puo' arrivare a 4 con quelli comuni), ideale per caratteri non latini, come cinese o giapponese.
- **UTF-32**: codifica a 4 byte fissi per ogni carattere, struttura facilmente leggibile e semplice, ma molto inefficiente, soprattutto per lingue che utilizzano principalmente l'alfabeto latino. Meno popolare rispetto a UTF-8 e UTF-16

### XML e JSON
XML e' un linguaggio di descrizione dei dati, utilizzato per rappresentare e scambiare messaggi e per definire il formato dei dati. Progettato per facilitare la generazione automatica di codice per la generazione e la manipolazione dei dati strutturati.

JSON e' un formato leggero e compatto, ampiamente usato sul web che e' piu' performante e facile da processare rispetto a XML. Pensato per le applicazioni Web 2.0 e per lavorare con **JavaScript**.

### Protocolli Applicativi
Un protocollo definisce le regole per il formato dei messaggi, la gestione degli errori e la struttura dei dati scambiati tra Client e Server.
Per gestire la lunghezza variabile, esistono diverse modalita':
- **Terminated Data**: uso di un terminatore speciale per segnare la fine, possibilita' di terminated buffer con doppio terminatore.
- **Lenght-prefixed data**: precede i dati con un campo che indica la lunghezza, semplifica receiver ma aumenta complessita' lato server.
- **Tag, length and value**: simile a length prefixed ma con maggiori informazioni

---

## RCP (Remote Procedure Call)

RPC semplifica l'invocazione di un servizio remoto. In una chiamata remota, due processi separati sono coinvolti. Un client ha uno stub locale per ogni procedura remota che puo' chiamare, e un server ha uno stub locale per ogni procedura che puo' essere invocata. Gli stub vengono creati automaticamente partendo dalla definizione delle funzioni. L'**IDL (Interface Definition Language)** e' un linguaggio usato per descrivere i servizi. 
Il binding collega un client a un server. Il client invoca uno stub che prepara i parametri e invia la richiesta. Puo' esser statico, fisso durante l'intera durata dell'applicazione, o dinamico, cambiando in base alla necessita'.

### Malfunzionamenti e Semantiche RPC
I malfunzionamenti in un sistema RPC possono includere crash, perdita o duplicazione di messaggi e la gestione errata dello stato. Per formattare i guasti, esistono diverse semantiche di chiamata remota:
- **Semantica May-be**: piu' semplice, senza gestione dei malfunzionamenti. Se una chiamata fallisce il client lo scopre solo quando scade il time out, ma non sa cos'e' successo. Facile da implementare, ma puo' causare problemi di consistenza e sicurezza.
- **Semantica at-least-once**: Se il client non riceve risposta, ritenta N volte. La procedura puo' essere eseguita piu' di una volta ma almeno una volta e' garantita. Funziona bene con procedure idempotenti.
- **Semantica at-most-once**: Ogni richiesta ha un identificatore unico. Il server memorizza l'identificatore e ignora le richieste duplicate. Se il client non riceve risposta, il server invia di nuovo il messaggio finche' non riceve risposta.
- **Semantica exactly-once**: Difficile da implementare, richiede che il server tratti le procedure come transazioni atomiche, garantendo che vengano eseguite esattamente una volta.

XDR e' un sistema che converte i dati tra il formato nativo e il formato esterno XDR, utilizzando uno stream per serializzare e deserializzare i dati uno alla volta.

gRPC e' un framework per sviluppare applicazioni basate su RPC. Utilizza Protobuf per definire l'interfaccia di servizio.

---

## Java RMI

E' una versione Java del modello RPC, progettata per lavorare in un'architettura ad oggetti. Permette di rendere le chiamate remote simili a quelle locali. Utilizza interfacce Java, tipi di dati primitivi Java e serializzazione. Un oggetto remoto in Java RMI e' un oggetto in cui i metodi possono essere invocati in una JVM diversa.

### Architettura RMI
Include:
- **Stub**: Proxy locale per invocazioni verso oggetti remoti
- **Skeleton** riceve le invocazioni dallo stub e le esegue sul server
- **Remote Reference Layer (RRL)**: Gestisce la connessione
- **Transport Layer**: si occupa della localizzazione del server
- **Registry**: permette al server di pubblicare e al client di trovare i servizi

### Serializzazione
La serializzazione in Java permette di trasformare oggetti complessi in sequenze di byte, che possono essere scritte su uno stream. Il processo di **marshalling**(serializzazione) converte l'oggetto in byte (metodo `writeObject()`), il processo di **demarshalling** restituisce l'oggetto originale dai byte (metodo `readObject()`)

### Passaggio parametri
Gli oggetti serializzabili vengono copiati tramite serializzazione, mentre gli oggetti remoti sono rappresentati tramite uno stub. Un client puo' localizzare un server RMI tramite l'RMI Registry

### Garbage Collector
La garbage collection di oggetti distribuiti e' il processo di deallocazione automatica degli oggetti remoti che non sono piu' referenziati da nessun client. In Java RMI questo viene gestito tramite un algoritmo basato sul conteggio dei riferimenti.

---

## Servizi Internet
 L'accesso a internet avviene sempre piu' tramite dispositivi mobili. 
 I servizi a livello applicazione includono:
 - Terminale remoto
 - Trasferimento file
 - Comandi remoti

### Telnet e Rlogin
Telnet consente al client di stabilire una connessione TCP con il server sulla porta 23. Il client invia i caratteri al server e riceve i dati da mostrare sul terminale. Il server gestisce la connessione e inoltra i dati al sistema locale. Telnet usa l'interfaccia standard NVT (Network Virtual Terminal).

Rlogin su macchine UNIX non richiede NVT e funziona esportando l'ambiente del client al server, gestendo stdin, stdout e stderr tramite una sola connessione TCP/IP.

### Secure shell (ssh)
E' una versione sicura di rlogin. Utilizza una comunicazione crittografata e protocolli di sicurezza avanzati per proteggere i dati.
Offre funzioni come Secure Copy (SCP), Secure File Transfer Protocol (SFTP), X session forwarding e port forwarding per proteggere altri protocolli non sicuri.
3 componenti principali:
- Transport Layer Protocol
- User Authentication Protocol
- Connection Protocol

### FTP (File Transfer Protocol)
Utilizzato per trasferire file tra computer, sia in upload che in download, ma presenta problemi di sicurezza. Per migliorare la sicurezza di usano alternative come FTP over SSL o SCP (utilizza ssh).
Funziona su TCP e utilizza due connessioni, una di controllo e una di dati, il server gestisce lo stato della connessione e comunica con il client tramite comandi e risposte codificate in tre cifre.
Due tipi di FTP:
- **FTP attivo**: client si connette al server sulla porta 21 per inviare comandi, comunica indirizzo IP e numero di porta. Server apre una connessione alla porta dati del client.
- **FTP passivo**: il client si connette sulla porta 21 e chiede di entrare in modalita' passiva. Il server apre una porta casuale per il trasferimento dei file e il client si connette a quella porta.

In modalita' passiva si riducono i rischi di attacchi ma richiede l'apertura di molte porte non privilegiate. FTP ha problemi di progettazione, soprattutto in modalita' attiva. Spesso considerato come un esempio di cattiva programmazione per i servizi internet.

### Posta elettronica
Consente lo scambio di mail su internet utilizzando il protocollo TCP (porta 25). I principali componenti sono i programmi applicativi, il protocollo SMTP (modello push) per il trasferimento delle e-mail, e i protocolli POP3 o IMAP per riceverle (modello pull).
Il DNS e' usato per trovare i server SMTP di un dominio. Quando una mail vinee inviata, SMTP del mittente interroga DNS per ottenere i server SMTP del destinatario.
Il formato dei messaggi e' regolato da RFC 822, che definisce messaggi di solo testo. **MIME (Multipurpose Internet Mail Extensions)** estende RFC 822 per supportare l'invio di dati multimediali, include righe di intestazione che definiscono il tipo di dato, definizione dei tipi di dato, indicazione della transcodifica per inviare i dati via e-mail.

### SMTP
Usato per il trasferimento delle e-mail tra Client e Server con comandi in formato ASCII

### Sincronizzazione Clock: NTP
NTP sincronizza gli orologi tra i nodi con precisione di millisecondi, Utilizza una gerarchia di server e UDP sulla porta 123.

### XMPP
Protocollo per la comunicazione in tempo reale, utilizzato per messaggi, stato e altre applicazioni come chat, VoIP e servizi basati sulla posizione.

### X Window
Consente l'accesso grafico a applicazioni su macchine remote. Ha un'architettura client-server con i ruoli invertiti.

### VoIP e Multimedia Streaming
SIP e RTP sono protocolli usati per la comunicazione in tempo reale. RTP gestisce il trasporto di audio e video, mentre SIP avvia, modifica e termina le sessione interattive.

---

## World Wide Web - WWW

E' un insieme di protocolli per accedere alle informazioni su internet in formato ipertestuale. Gli standard includono URL, HTTP, HTML, CSS, CGI e JavaScript.

### HTTP
E' un protocollo applicativo per trasferire le informazioni tra client e server. HTTP 1.0 utilizza un modello request/response e connessioni non persistenti. I principali metodi sono GET, HEAD, PUT/PATCH, POST e DELETE. HTTP 1.1 introduce connessioni persistenti. HTTP 2 e 3 migliorano le prestazioni. **HTTPS** e' una versione sicura di HTTP con connessione cifrata tramite SSL.

### WAP (Wireless Access Protocol)
E' una suite di protocolli per l'accesso al Web tramite dispositivi mobili, include il linguaggio WML, protocolli di trasporto e sicurezza. Converte HTML in WML per dispositivi.

### HTML (Hyper Text Markup Language)
Derivato da SGML, definisce la struttura delle informazioni tramite tag, indicano come trattare il testo all'interno senza specificare l'aspetto fisico.

### CSS (Cascading Style Sheets)
Usato per formattare documenti HTML, separando la struttura del contenuto dalla sua presentazione. Rende lo sviluppo piu' veloce e il codice piu' riutilizzabile.

### Architettura C/S Web
L'architettura Web e' evoluta, passando da sistemi semplici a sistemi piu' complessi. Esistono pagine statiche, dinamiche, attive. 
L'architettura web a tre livelli comprende:
- Presentation Tier
- Application Tier
- Data Tier

Tecnologie per pagine dinamiche includono 
- **CGI**: permette di eseguire applicazioni esterne sul server web. Possono essere scritte in bari linguaggi e forniscono risultati al server, che li invia al client.
- **JSP**: Consente di inserire codice Java nelle pagine HTML per crearle dinamicamente.
- **Java Servlet**: componenti Java che eseguono attivita' su server web tramite JVM.
- **Ajax**: combinazione di tecnologie per creare pagine web attive senza ricaricare l'intera pagina.

### Stato nel Web
Problema: gestire lo stato tra Client e Server. Soluzione: cookie, hidden forms fields lato client, e session ID unico lato server

### Web Caching
Il caching e' fondamentale per migliorare le prestazioni. Esistono diverse tipologie di cache: del browser, proxy trasparenti, reverse proxy e cache server. Il caching si basa su header HTTP come Last-Modified, If-Modified-Since, Etag, If-None-Match e Cache-Control.

### CDN
Content Delivery Network e' una rete di server distribuiti che ottimizza la distribuzione di contenuti su larga scala. Riduce la latenza e migliora la velocita' di accesso. Fondamentali per migliorare l'efficienza e l'affidabilita' della distribuzione dei contenuti, sia multimediali, che file, a livello globale.

### Motori di ricerca
La creazione di un indice del web avviene in due fasi:
- ricerca: motori di ricerca esplorano il web
- indicizzazione: informazioni estratte dalle pagine e memorizzate con l'URL

Google utilizza l'algoritmo `PageRank`, che assegna importanza alle pagine piu' linkate, considerando anche i link provenienti da siti rilevanti.

---

## Sicurezza informatica

La sicurezza IT comprende vari aspetti: autenticazione, autorizzazione, riservatezza, disponibilita', integrita' e paternita'. 
I concetti chiave includono: vulnerabilita', exploit, attacco e asset. 
Standard di riferimento sono Cybersecurity Framework del NIST e ISO 27001. La fondazione OWASP promuove la sicurezza delle applicazioni.

### Crittografia
La crittografia rende un messaggio incomprensibile. Esistono sistemi crittografici a chiave segreta e a chiave pubblica.
- **Chiave segreta**: utilizza una sola chiave per cifrare e decifrare. Algoritmi come DES e AES. Modalita' come ECB. CBC e CTR
- **Chiave pubblica**: utilizza una copia di chiavi, una pubblica e una privata. Esempio: RSA. Piu' scalabile (richiede meno chiavi per numero di utenti uguale).

### Funzioni di hash
Trasformano un input in un output di lunghezza fissa. Sono facili da calcolare, difficili da invertire e difficili da trovare collisioni. Usate per password di hashing, integrita' dei messaggi e firma digitale.

### Kerberos e PKI
Kerberos usa chiavi segrete con validita' temporale. Le PKI gestiscono la creazione, distribuzione e revoca di chiavi e certificati.

### TLS (Transport Layer Security) 
TLS garantisce comunicazioni sicure tra client e server. Le fasi di TLS includono negoziazione degli algoritmi, scambio di chiavi e comunicazione cifrata

### Posix P1003.6 
Gestisce la sicurezza nei sistemi operativi. Include Access Control List (ACL) e Mandatory Access Control (MAC)

### Internet e intranet
Il collegamento delle reti aziendali a internet comporta rischi di sicurezza. Per proteggere i sistemi si usano firewall e VPN.

### Firewall
Un firewall filtra il traffico tra due reti. Tipi di firewall:
- packet filtering
- stateful inspection
- proxy server
- application proxy
- next-generation firewall
Il firewall puo' essere posizionato in diversi punti della rete, con diverse architetture. La DMZ (Demilitary Zone) ospita diverse risorse pubbliche.

### Honeypot, IDS, IDPS, SIEM e VPN
- Honeypot: attira gli attacchi informatici
- IDS (Intrusion Detection System): Monitora la rete per rilevare attivita' dannose
- IDPS (Intrusion Prevention System): Rileva e previene gli attacchi
- SIEM (Security Information and Event Manager): Integra i dati di vari sistemi di sicurezza
- VPN (Virtual Private Network): crea una rete privata sicura sopra internet.