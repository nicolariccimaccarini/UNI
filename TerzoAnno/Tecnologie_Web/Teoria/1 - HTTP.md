## Modello ISO/OSI
L'Open System Interconnection (modello ISO/OSI) e' uno standard formalizzato nel 1978 dal principale ente di standardizzazione internazionale, l'International Organization for Standardization (ISO) che stabilisce gli standard di comunicazione per l'architettura logica di rete.
Il modello ISO/OSI e' un insieme di protocolli di comunicazione di rete suddiviso in 7 livelli:
- Applicazione
- Presentazione
- Sessione
- Trasporto
- Rete
- Dati
- Fisico

## Protocollo HTTP
- Il protocollo HTTP (HyperText Transfer Protocol) insieme al linguaggio HTML e agli URL costituiscono il nucleo del World Wide Web (WWW)
- Le specifiche di questo protocollo sono gestite da World Wide Web Consortium
- Ad oggi la versione piu' recente e' la versione 3 e si indica con HTTP/3

### Gli identificatori
Il protocollo HTTP fa uso degli URI (Uniform Resource Identifier) che sono una sintassi usata sul Web per identificare le **risorse** sulla rete Internet
Gli URI si suddividono in:
- URL $\rightarrow$ Uniform Resource Location
- URN $\rightarrow$ Uniform Resource Name

#### URL
Gli URL sono particolari URI che identificano una risorsa immediatamente utilizzabile da un programma, ed hanno il seguente schema
```
<protocol>://<host>[:<prot>] [<path>] [?<query>]
```
Gli URL contengono tutte le informazioni necessarie per accedere alla risorsa, ma sono fragili cioe' possono perdere il loro "valore" se soggetti a modifiche anche non sostanziali, ad esempio cambiare il nome di una directory

#### URN
Un URN e' un nome stabile e definitivo di una risorsa, che fornisce un'informazione certa e affidabile sulla sua esistenza e accessibilita'.
Gli URN devono essere trasformati da un apposito servizio, negli URL attualmente associati alle risorse. La mappa quindi, deve essere aggiornata ogni volta che la risorsa viene spostata.

## Protocollo HTTP Request
Il protocollo funziona su un meccanismo client/server in cui due entita' si scambiano messaggi di richiesta e risposta.
Il messaggio di richiesta viene inviato dal client al server e' composto da 3 parti:
- Request
- Header
- Body

### HTTP Request
La riga di richiesta e' composta da:
- metodo (o verbo)
- URI e versione del protocollo
es. `GET /uri/risorsa HTTP/1.1`

I metodi previsti per la versione 1.1 del protocollo sono:
- GET
- POST
- HEAD
- PUT
- DELETE
- TRACE
- OPTIONS
- CONNECT

### HTTP Header
Nell'header della richiesta HTTP vengono inserite delle indicazioni sul formato della richiesta atteso ed altre informazioni, ad esempio:
- User-Agent
- ContentType
- Accept
- Authentication

Queste informazioni vengono inserite come elenco di Chiave:Valore

### HTTP Body
Il corpo del messaggio contiene le informazioni che vogliamo trasferire al server

## Protocollo HTTP Response
il messaggio di risposta che viene inviato dal server al client e' composto anch'esso da 3 parti:
- Header
- Status 
- Body

### HTTP Header
Come per la richiesta nell'header della risposta vengono inserite delle indicazioni sul formato dei dati inviati altre informazioni, ad esempio Content-Type.
Anche queste informazioni vengono inserite come elenco di Chiave:Valore.

### HTTP Status
Lo status che viene inviato dal server al client e' composto da una riga contenente un codice a 3 cifre suddivise in "categorie". 
Ogni categoria inizia con un codice specifico e determina cosa contiene la risposta:
- 1xx $\rightarrow$ messaggi informativi
- 2xx $\rightarrow$ ok (richiesta soddisfatta, nel body ci sara' il contenuto)
- 3xx $\rightarrow$ redirezione ad un altra risorsa
- 4xx $\rightarrow$ errori nella richiesta
- 5xx $\rightarrow$ errore da parte del server

### HTTP Body
Il corpo del messaggio contiene le informazioni che abbiamo richiesto al server

## Esempio Richiesta
![[esempioRichiesta.png]]

## Esempio Risposta
![[esempioRisposta.png]]

## HTTP Limiti
Il protocollo HTTP ed il linguaggio HTML sono stati originariamente progettati con lo scopo di visualizzare e navigare ipertesti.
In particolare il protocollo HTTP:
- E' **stateless** $\rightarrow$ ogni richiesta e' indipendente da quella precedente
- E' una tecnologia pull $\rightarrow$ per ottenere informazioni e' necessario che il client ne faccia esplicita richiesta
- Non descrive l'origine di una richiesta $\rightarrow$ click o visita diretta


## HTTP/2
Nel Maggio 2015 e' stato standardizzato e oggi e' supportato da tuti i browser.
La standardizzazione del protocollo HTTP/1.1 risale al 1997.
Non e' una ristruttura del protocollo HTTP/1.1 e garantisce la retrocompatibilita' lasciando inalterati gli header, status code e metodi. E' un 'wrap' del protocollo HTTP/1.1 sul protocollo SPDY di Google che riduce i tempi di latenza del protocollo HTTP/1.1
### HTTP/2 Performance
- **Multiplexing** $\rightarrow$ nella stessa connessione e' possibile avere richieste e risposte multiple che vengono "ricostruite" nell'host di destinazione. Questo permette di fare operazioni in parallelo e quindi di ridurre la latenza.
- **Compressione headers** $\rightarrow$ anche gli header HTTP sono compressi e il consumo di banda e' notevolmente ridotto
- Supporto per operazioni di **push** da parte del server, che puo' inviare dati al client prima che questo ne faccia richiesta.

## Il Protocollo IP
Il protocollo IP appartiene al gruppo dei protocolli di Rete.
Sul protocollo IP si basa l'identificazione dei nodi della rete, ad ogni nodo deve essere associato un indirizzo del protocollo IP che ha la seguente forma: XXX.XXX.XXX.XXX
Ogni indirizzo IP e' formato da 32 bit suddivisi in 4 gruppi da 1 byte.
Ogni byte puo' avere un valore da 0 a 255.

Esiste un sistema per risolvere i nomi di rete negli indirizzi IP e viceversa. Il sistema si chiama DNS
Esistono degli indirizzi IP "riservati", cioe' indirizzi ai quali non puo' essere associato nessun dispositivo di rete perche' per convenzione sono gia' utilizzati per altro, il piu' famoso e' 127.0.0.1 che identifica sempre la macchina locale.

## W3C
il World Wide Web Consortium (W3C) viene fondato nel 1994 da T. Berners-Lee come collaborazione tra MIT e il CERN, con lo scopo di:
- "Condurre il World Wide Web al suo pieno potenziale sviluppando protocolli e linee guida che assicurino una crescita di lungo periodo sul web"

Il W3C e' un consorzio internazionale in cui le organizzazioni membro, lo staff permanente del consorzio ed il pubblico lavorano insieme per sviluppare gli standard del Web.

### Ciclo di vita degli standard
Ogni standard, prima di diventare tale, subisce diversi passaggi evolutive della sua definizione:
- Working Draft (WD)
- Candidate Recommendation (CR)
- Proposed Recommendation (PR)
- W3C Recommendation (REC)

#### W3C - WD
Un documento allo stadio di Working Draft e' un documento che e' stato rilasciato dal W3C per essere rivisto dalla comunita'

#### W3C - CR
E' un documento che ha subito ampie revisioni e soddisfa i requisiti tecnici del gruppo di lavoro. Viene rilasciata per avviare la sperimentazione delle implementazioni

#### W3C - PR
Documento maturo, con ampia revisione e verificate possibilita' di implementazione in attesa dell'approvazione finale

#### W3C - REC
Documento di fatto, uno standard per il W3C che ne raccomanda il massimo utilizzo e diffusione

