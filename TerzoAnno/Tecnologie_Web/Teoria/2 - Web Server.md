## Modello Client-Server
Il sistema informativo Client-Server e' formato da due entita':
- Client $\rightarrow$ che richiede il servizio
- Server $\rightarrow$ che fornisce il servizio

Le due entita' riescono a comunicare tra loro attraverso una connessione di rete

Il **Server** svolge le operazioni necessarie per implementare un servizio, ad es.:
- Inviare/ricevere la posta elettronica
- Gestire le banche dati 
- Pubblicare documenti HTML

Il funzionamento di questo modello e' legato alla reperibilita' della rete di comunicazione fra i due "lati" che lo compongono.
La comunicazione fra le due parti e' legata anche al rispetto delle "regole" di comunicazione fra i due elementi che lo compongono, queste regole sono dette **Protocolli**.

E' un modello di comunicazione, diretta e asimmetrica
- Il **Client** contatta direttamente ed esplicitamente il server
- Il **Server** e' in grado di fornire moliti client contemporaneamente 

Il modello Client/Server risolve il problema del **rendez-vous** definendo il Server come un processo sempre in attesa di richieste di servizio.

I Client devono poter specificare il servizio desiderato senza ambiguita'
- I Server rendono pubblico un identificatore
- I Client devono reperire l'identificatore del servizio
- I Client devono usare l'identificaore per contattare il Server

La parte **Client** e' quella parte che l'utente vede e con la quale interagisce e fornisce un'interfaccia adatta a comunicare con gli esseri umani (gli utenti).
La funzione principale del Client e' quella di contattare il Server per poter fruire del servizio inviandogli i dati necessari.
Il Client, generalmente si occupa di verificare i dati inseriti dall'utente e gestisce le risorse come ad esempio lo schermo, la tastiera, ecc.

## Tipi di interazione
- **Connection Oriented** $\rightarrow$ viene stabilito un canale dio comunicazione virtuale prima di iniziare lo scambio dei dati (comunicazione telefoniche), ad esempio il servizio ssh.
- **Connectionless** $\rightarrow$ non c'e' connessione virtuale, ma semplice scambio di messaggi (sistema postale), ad esempio il servizio di posta elettronica

## Push e Pull
- **Modello Pull** $\rightarrow$ il Client richiede il servizio al Server:
	- Sincrono $\rightarrow$ il Client si blocca e attende la risposta del server
	- Asincrono $\rightarrow$ il Client non si blocca e controlla periodicamente se e' arrivata la risposta dal Server
- **Modello Push** $\rightarrow$ il Client segnala al Server il proprio interesse ad ottenere un servizio. Quando il servizio e' disponibile, il Sever contatta il Client inviando le informazioni

## Stato
- **Statefull** $\rightarrow$ viene mantenuto lo stato dell'interazione e quindi ogni messaggio dipende dal messaggio precedente
- **Stateless** $\rightarrow$ lo stato dell'interazione non viene mantenuto, quindi ogni messaggio e' indipendente dagli altri
Il compito di memorizzare lo stato e' affidato al Server.

## Protocollo HTTP
Il protocollo su cui si basa il Web e' il protocollo HTTP che e' un protocollo Stateless.
Ogni richiesta dal Client al Server deve essere indipendente e deve contenere tutte le informazioni per essere portata a termine senza dipendere dalla precedente.

Ci sono servizi che vengono comunemente forniti sul Web, ad esempio la posta elettronica oppure siti di e-commerce che per forza di cosa fra una pagina e  l'altra devono riconoscere chi sta visitando la pagina ha fatto il login, oppure tutti gli oggetti che sono stati messi nel carrello.
I limiti del protocollo HTTP si superano a livello dell'applicazione, cioe' l'applicazione che ha il compito di implementare meccanismi che vanno a superare queste limitazioni, ad esempio attraverso i cookies

## Server concorrenti
Mentre sul lato Client eventuali esecuzioni concorrenti sono gestire dal multitasking del sistema operativo, sul Server la concorrenza e' fondamentale per poter offrire un servizio a piu' client contemporaneamente.

Due tipi di modello:
- **Modello iterativo** $\rightarrow$ processa le richieste di servizio una alla volta. Basso consumo di risorse, semplicita' progettuale
- **Modello concorrente** $\rightarrow$ gestisce molte richieste di servizio alla volta. Alto consumo di risorse, maggiore complessita' progettuale.

### Prestazioni modello iterativo
Dal punto di vista del Client, definiamo il tempo di risposta $Tr$ come il ritardo totale tra l'invio della richiesta e l'arrivo della risposta dal Server.

- $Ts$ $\rightarrow$ tempo di elaborazione di una singola richiesta
- $Tc$ $\rightarrow$ tempo di comunicazione medio
- $Tq$ $\rightarrow$ tempo di accomodamento medio 

$$ Tr = Ts + 2Tc + Tq $$
Nel caso del Server iterativo, che risponde a una singola richiesta alla volta e accoda le altre, il tempo di risposta e' circa proporzionale alla lunghezza della coda.
Con lunghe code di attesa, il tempo di risposta puo' diventare anche molto maggiore del tempo di elaborazione della richiesta.

- Limitare la lunghezza della coda (non puo' essere infinita)
- Le richieste a coda piena vengono rifiutate (conseguenza)

### Prestazioni modello concorrente
Concorrenza riesce a migliorare il tempo di risposta:
- Se la risposta richiede un tempo significativo di attesa I/O
- Se le richieste richiedono tempi di elaborazione molto variabili
- Se il Server esegue in un sistema hardware multiprocessore

Considerando;
- $Tc$ $\rightarrow$ tempo di comunicazione medio
- $Tq$ $\rightarrow$ tempo di accomodamento medio (a volte trascurabile)
- $Tg$ $\rightarrow$ tempo di generazione di un eventuale servitore
$$ Tr = Ts + 2Tc + Tq + Tg $$
## Tecnologie Server Side

| Tecnologia              | Categoria                   |
| ----------------------- | --------------------------- |
| Apache, NGINX, IIS      | Web Server                  |
| MySQL, Cassandra        | Database                    |
| PHP, Perl, Python, Ruby | Linguaggi di programmazione |
Tutte queste tecnologie sono Server Side, ovvero tecnologie che risiedono e vengono eseguite sul Server allo scopo di fornire il servizio

Limiti protocollo HTTP:
- Protocollo Stateless
- Tecnologia Pull
- Linguaggio statico (con HTML non e' possibile eseguire condizioni o altri statement)

E' possibile superare queste limitazioni sfruttando altre tecnologie che risiedono sul **Server** e che vanno ad integrare le tecnologie gia' presentate:
- Database
- Linguaggi di scripting
Queste tecnologie "affiancano" il Server Web per implementare **Applicazioni Web**

Le tecnologie Server Side, oltre a risiedere sul Server, vengono anche eseguite sul Server con le risorse del server stesso.

Per realizzare una Web App, abbiamo bisogno di:
- Web Server
- Database Server
- Linguaggio di programmazione

### LAMP
Una delle soluzioni piu' utilizzate e' l'installazione di uno stack **LAMP**.
L'acronimo deriva da:
- GNU/**Linux** $\rightarrow$ sistema operativo
- **Apache** $\rightarrow$ il server web
- **MySQL** o MariaDB $\rightarrow$ il DBMS
- **PHP**, Perl o Python $\rightarrow$ i linguaggi di programmazione

## Cos'e' un Web Server
E' un software, in esecuzione su un server, in grado di gestire le richieste di trasferimento di pagine web verso un client. La comunicazione tra server e client avviene tramite protocollo HTTP, che utilizza la porta 80.
La versione sicura di HTTP e' HTTPS (HTTP + SSL) e usa la porta 443.

![[WebServerComeFunziona.png]]

## Pagine statiche e dinamiche

| Pagine statiche                                                         | Pagine dinamiche                                                                                         |
| ----------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| Contengono solo HTML                                                    | Contengono HTML insieme a istruzioni scritte in linguaggi di scripting server-side (PHP, Java, C#, ecc.) |
| Il contenuto non e' modificabile                                        | Il web server interpreta le istruzioni e genera "on the fly" il contenuto della pagina                   |
| Viene inviato lo stesso contenuto in risposta a tutte le richieste HTTP | L'HTML generato in risposta alle richieste HTTP puo' essere diverso                                      |
## Path translation
Per servire la pagina richiesta dall'utente i web server devono mappare il path inserito nell'URL richiesto:
- In una risorsa locale presente nel file system (per le pagine statiche)
- Oppure nel nome di un programma interno o esterno (per le pagine dinamiche)

### Retrieve di una pagina statica
L'URL inserito dall'utente `http://www.mysite.come/info/a.html` viene tradotto dallo user agent del client ed inserito nella richiesta HTTP.
```
GET /info/a.html HTTP/1.1
Host: www.mysite.come
```
Il web server presente sull'host www.mysite.com inserira' accodando il path della richiesta a quello della sua root. 
Nella risposta HTTP verra' inviato il file presente nel filesystem all'indirizzo `/var/www/path/<nomefile>.html`

![[retrievePaginaStatica.png]]

### Retrieve pagina dinamica
Il Web Server prima di restituire la pagina al client, esegue lo script PHP, Python, ecc...
![[retrievePaginaDinamica.png]]

## Tree structure
I file di configurazione di Apache sono organizzati secondo la seguente gerarchia, con radice localizzata nella directory `/etc/apache2/`.
I file log sono nella directory `/var/logs/apache2`

![[apacheStructure.png]]

### Directories
```
mods-enabled/
conf-enabled/
sites-enables/
```

Sono directories che contengono rispettivamente snippet per la gestione dei moduli, frammenti per la configurazione globale e i file per la configurazione dei virtual hosts.
Le configurazioni presenti nei file vengono attivate tramite un symlink dalle rispettive controparti
`*-available/`

### Files
`/etc/apache2/apache2.conf`
E' il file principale che unisce i vari pezzetti di configurazione inseriti negli altri file presenti nella directory di root e nelle sotto-directory `mods-enabled` oppure `conf-enabled`

`/etc/apache2/ports.conf`
e' sempre incluso dal file precedente e indica le porte che rimangono in ascolto in attesa delle connessioni da parte dei client.

## Configurazione Apache
Questa struttura gerarchica permette ad Apache di essere molto flessibile:
- nelle directory `*-available` sono presenti tutte le risorse disponibili, che rimangono semplicemente disattivate se non necessarie. Quando servono, e' sufficiente creare un link simbolico nelle rispettive directory `*-enabled` ed inserire l'include delle risorse desiderate nei file di configurazione

Comunemente un server ospita piu' siti, per farlo ci sono due possibilita':
1. subdirectories
	- all'interno della directory di root `/var/www/` si crea una directory per ogni sito che si vuole ospitare. E' una soluzione molto semplice e veloce per lavorare in locale ma, in produzione, presenta problemi relativi alla condivisione del contesto in quanto l'host risulta essere lo stesso per tutti i siti
2. virtual hosting
	- il **virtual hosting** e' un metodo utilizzato per ospitare sullo stesso server il sito web per piu' dio un nome di dominio, talvolta anche sullo stesso indirizzo IP. 
	  In Apache, i siti disponibili per l'attivazione sono contenuti nella directory `sites-available/`
	  Ogni file presente nella cartella corrisponde ad un sito per cui, all'installazione, e' presente un solo file chiamato `000-default.conf`

### Quindi se vogliamo abilitare un nostro sito...
1. Creiamo la directory che ospitera' i files del sito, ad esempio `mkdir /var/www/sito1/` in cui inseriamo una semplice pagina HTML chiamata `index.html`
2. Ci spostiamo nella directory `/etc/apache2/sites-available` e copiamo il contenuto del file `000-default.conf` del nuovo file `sito1.conf` `cp 000-default.conf sito1.conf`
3. Apriamo il file appena creato e modifichiamo parametri relativi al nostro sito
```
ServerName sito1
DocumentRoot /var/www/sito1/
```

4. A questo punto possiamo attivare il nostro sito eseguendo il comando 
   `$ a2ensite sito1`
   Ora se andiamo a vedere il contenuto della directory `sites-enabled/` vedremo che e' presente anche il link simbolico al "sito1"
5. L'ultimo passaggio per poter visitare il sito consisterebbe nella configurazione del DNS, per semplicita' possiamo semplicemente editare il file `hosts` presente nella directory `/etc/`. Questo file viene sempre letto dal browser prima di consultare il servizio DNS, quindi permette di definire staticamente il mapping degli indirizzi che vogliamo rimangano invariati (tipicamente nessuno)
6. Facciamo il reload di Apache `service apache2 reload`
7. Se il nostro browser e digitiamo nella barra degli indirizzi: http://sito1 viene visualizzato il contenuto della pagina `index.html` che abbiamo appena creato

## Servizi di Hosting
Esistono delle aziende il cui scopo e' fornire ambienti integrati in cui sono rese disponibili tutte le tecnologie Server Side di cui si ha bisogno per realizzare Web App.

### Shared Hosting
Soluzione piu' comune ed economica:
- Risorse condivise e limitate
- Configurazione limitate
- Nessuna manutenzione
- Economico

Esistono molte aziende che offrono un servizio di hosting condiviso molto affidabile per quelle attivita' chiave del business (es. e-commerce).

### Virtual Private Server
Soluzione professionale
- Risorse condivise e limitate
- Nessuna limitazione sulla configurazione
- Richiede che qualcuno si occupi della "manutenzione"
- Tutto sommato economico

### Dedicated Server
Soluzione professionale per chi ha esigenze particolari
- Risorse dedicate
- Nessuna limitazione sulla configurazione
- Richiede che qualcuno si occupi della "manutenzione"
- Generalmente molto costosa

### Cloud
Soluzione professionale per chi tenta di mediare fra costi e flessibilita'
- Risorse limitate **ma espandibili**
- Nessuna limitazione sulla configurazione
- Richiede che qualcuno si occupi della "manutenzione"
- Richiede competenze specifiche per la configurazione

Il termine Cloud e' usato per indicare una rete di server, dove ognuno dei quali ha una diversa funzione.
- Eseguire applicazioni
- Fornire servizi
- Archiviazione dei dati
- ...

Il vero vantaggio e' che come sviluppatori, non dobbiamo preoccuparci della gestione di un server, sara' il fornitore del servizio cloud a farlo per noi.

#### Componenti Cloud
- **Storage**
	- E' il componente che si occupa di memorizzare i dati. Generalmente e' composto da un NAS o da un SAN
- **Calcolo**
	- Sistema che gestisce la virtualizzazione dei processi (solitamente KVM)
- **Controller**
	- Il componente che gestisce tutta l'infrastruttura attraverso chiamate REST alle API.
- **IaaS** (Infrastructure As A Service)
	- Vengono virtualizzate tutte le risorse CPU, RAM, Disco, ...
	- Flessibilita' di un'infrastruttura fisica senza l'onere della gestione dalla parte dell'hardware
- **PaaS** (Platform As A Service)
	- L'utente si preoccupa solamente di sviluppare il software e di caricarlo sulla piattaforma
- **SaaS** (Software As A Service)
	- L'utente utilizza il software caricato sulla piattaforma