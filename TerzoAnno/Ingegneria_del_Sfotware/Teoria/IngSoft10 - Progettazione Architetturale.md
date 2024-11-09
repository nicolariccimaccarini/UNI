## Obiettivo
- La progettazione dell'architettura rappresenta la struttura dei dati e i componenti del programma necessari
- Considera lo stile architetturale che si intende usare per il sistema, la struttura e le proprieta' dei componenti e le relazioni tra i componenti
- Se il sistema e' grande e/o complesso si puo' avere un progettista di database (o data warehouse) per i dati e un "architetto di sistemi" per il resto del sistema
- Cosa vogliamo ottenere alla fine? Modello dell'architettura con le proprieta' e le relazioni fra i componenti

## L'architettura del software
- La definizione dell'architettura non ha lo scopo di creare del software operativo, piuttosto
	- analizzare l'efficacia del progetto nel soddisfare i vari requisiti
	- permette la valutazione di tutte le possibili alternative architetturali del sistema in una fase iniziale in cui le modifiche sono relativamente poco costose
- Quindi l'architettura non e' la stesura di codice ma una buona architettura facilita la successiva stesura del codice
- Architettura del software == progetto dei dati + progetto architetturale

## Perche' e' importante?
Sostanzialmente per 3 motivi chiave:
1. La rappresentazione dell'architettura e' il mezzo che le parti interessate allo sviluppo usano per comunicare
2. L'architettura mette in evidenza le decisioni progettuali preliminari: queste avranno un impatto nel successivo lavoro di sviluppo
3. L'architettura e' un modello relativamente conciso e facilmente comprensibile di come il sistema sara' strutturato e di come i vari componenti collaboreranno tra loro

## Progetto dei dati
- "La progettazione dei dati a livello dei componenti si concentra sulla rappresentazione delle strutture dati il cui accesso avviene direttamente da parte di uno o piu' componenti software"
- Sono stati individuati una serie di principi per la progettazione dei dati

## Principi di progettazione dei dati 
- Si applicano gli stessi principi di analisi sistematica che si usano per le funzionalita' e per il comportamento
- Bisogna individuare tutte le strutture di dati e le operazioni da svolgersi su ciascuna
	- nel fare la struttura tenere conto delle operazioni che si vuole svolgere sulla struttura stessa
- Occorre compilare un dizionario dei dati, e utilizzarlo per definire il progetto dei dati e del programma
	- class diagram $\rightarrow$ definisco oggetti-dato e le operazioni applicate
- Le decisioni di basso livello sul progetto dei dati devono essere rimandate alla ultime fasi di progettazione
	- raffinare per passi successivi $\rightarrow$ l'organizzazione e' definita durante l'analisi req., raffinata in quella fase e specificata nei dettagli poi
- La rappresentazione delle strutture dati deve essere nota solo ai moduli che hanno bisogno di utilizzarle direttamente 
	- information hiding e coupling
- E' vantaggioso pensare in maniera modulare, pensando da subito a possibili riutilizzi
	- sviluppare librerie di strutture dati e di operazioni
- La specifica e l'implementazione degli ADT deve basarsi su un progetto del software e su un linguaggio di programmazione
	- implementare un ADT complicato puo' dipendere dal supporto che un determinato linguaggio di programmazione offre

## Stili architetturali
- Uno stile architetturale e' composto da:
	- un insieme di componenti che implementano le funzionalita' richieste
	- un insieme di connettori che consentono la comunicazione (e quindi la cooperazione) tra i componenti
	- un insieme di vincoli che definiscono i modi in cui i vari componenti possono essere integrati fra loro per formare il sistema
	- dei modelli semantici che permettono al progettista di comprendere il funzionamento del sistema sulla base delle sue componenti
- Gli stili sono quasi infiniti, ma si possono classificare in un numero ridotto di stili base

### Architettura basata sui dati (a repository)
- Il sistema e' concentrato su un archivio di dati
- Le componenti accedono all'archivio operando indipendentemente tra loro
- L'archivio puo' essere passivo (tutte le operazioni sono in mano ai client) oppure attivo (l'archivio notifica ai client le variazioni nei dati)
- Il vantaggio e' l'indipendenza tra i vari moduli $\rightarrow$ si puo' aggiungere un modulo o intervenire su uno di essi senza che gli altri ne risentano
- L'accoppiamento tra le componenti puo' avvenire mediante un meccanismo a blackboard

#### Vantaggi
- modo efficiente di condividere grandi quantita' di dati
- i sottosistemi possono disinteressarsi di come i dati vengono condivisi, la gestione e' centralizzata

#### Svantaggi
- i sottosistemi devono concordare su un modello dei dati che inevitabilmente e' un compromesso tra esigenze diverse
- modificare la struttura (schema) dei dati e' difficilmente dispendioso
- non c'e' spazio per politiche specifiche di accesso ai dati
- bassa scalabilita' $\rightarrow$ il repository centrale spesso e' un collo di bottiglia

### Architettura Client-Server
- Modello per sistemi distribuiti, mostra come dati e processi sono distribuiti su un insieme di componenti
	- insieme di server autonomi che offrono servizi specifici
	- insieme di clienti ce richiedono questi servizi
	- una rete di comunicazione che permette ai clienti di accedere ai server

#### Stati funzionali nell'architettura client-server
- Presentation Layer
	- si occupa di presentare i risultati della computazione agli utenti del sistema e di gestire gli inputa da parte degli utenti
- Application processing layer
	- si occupa di offrire le funzionalita' specifiche dell'applicazione
- Data management layer
	- si occupa di gestire la comunicazione con il DBMS

#### Caratteristiche del modello client-server
Vantaggi
- la distribuzione dei dati e' molto semplice
- fa un uso effettivo del sistema di rete e puo' richiedere hw economico
- e' facile aggiungere nuovi server o fare l'upgrade dei server esistenti

Svantaggi
- non c'e' un unico modello condiviso dei dati, quindi ogni sottosistema fa uso di un modello proprio, e lo scambio di dati puo' essere inefficiente
- alcune attivita' di gestione dei dati devono essere replicate
- non c'e' un registro centrale dei nomi e servizi $\rightarrow$ puo' essere difficile sapere quali dati/servizi sono disponibili

### Architettura a flusso di dati (pipe-and-filter)
- Il sistema e' modellato sul flusso di dati, dalla fase di input a quella di output
- I moduli si comportano da filtri connessi da pipe di altri
- Ogni filtro si attende solo dati in input con un certo formato e produce dati in output di formato prefissato
- Ogni filtro lavora senza occuparsi di cosa lo precede o lo segue
- Se il flusso dati degenera in una unica catena di filtri, allora l'architettura si dice "a filtro batch sequenziale"

#### Caratteristiche dell'architettura a flusso di dati
Vantaggi
- e' facile da costruire computazioni complesse mediante concatenazione di filtri semplici
- ogni filtro e' una "scatola nera" riutilizzabile in altre situazioni
- se ben programmati, i filtri non condividono lo stato (basso accoppiamento)

Svantaggi
- i formati di dati in input e output di filtri collegati devono essere compatibili
- se la struttura di controllo non puo' essere "linearizzata", questo modello non e' adeguato

### Architettura a macchina astratta (a livelli)
- Usato per modellare l'interfaccia tra sottosistemi
- Organizza il sistema in un insieme di strati (o macchine astratte) ognuno dei quali offre un insieme di servizi
- Supporta lo sviluppo incrementale dei sottosistemi a livelli diversi $\rightarrow$ se l'interfaccia di un livello cambia, ne risulta affetto solo il livello adiacente

#### Caratteristiche dell'architettura a livelli
Vantaggi
- supporta lo sviluppo incrementale, livello dopo livello
- se si cambia l'interfaccia di un livello, solo quello adiacente ne risente

Svantaggi
- spesso puo' essere complicato strutturare il sistema in questo modo
- puo' essere restrittivo pensare che un livello possa interagire solo col precedente o successivo

## Scomposizione modulare
- Ulteriore raffinamento a livello strutturale $\rightarrow$ i sottosistemi sono scomposti in moduli
- Sottosistema - modulo $\rightarrow$ distinzione non chiara ma
	- sottosistema e' un sistema di diritto: sono composti da moduli, hanno interfacce ben definite per comunicare con altri sottosistemi
	- modulo di solito e' componente sottosistema e fornisce uno o piu' servizi ad altri moduli, o usa servizi di altri moduli; non e' indipendente

Due modelli di scomposizione:
- modello a oggetti, dove il sistema e' scomposto in oggetti che interagiscono
- modello data-flow, dove il sistema e' scomposto in modelli funzionali che trasformano input in output (modelli pipeline)

### Modelli a oggetti
- Si struttura il sottosistema come un insieme di oggetti con accoppiamento lasco e interfacce ben definite
- La scomposizione orientata ad oggetti significa identificare le classi degli oggetti, gli attributi (campi) e le operazioni (metodi)
- Quando vengono implementati, gli oggetti sono istanze di queste classi e qualche modello di controllo e' usato per coordinare i metodi

### Modelli data-flow
- Trasformazioni funzionali che producono un output a partire da un input
- Possono riferirsi ad un modello a "pipe" e a filtri
- Se c'e' un unica trasformazione sequenziale, e' un modello batch sequenziale: molto usato nei sistemi di gestione dei dati
- Non si presta bene a sistemi interattivi

### Modelli di controllo
- Descrivono il metodo con cui fluisce il controllo tra sottosistemi
	- controllo centralizzato $\rightarrow$ un sottosistema ha la responsabilita' del controllo globale e avvia e disattiva i sottosistemi
	- controllo basato a eventi $\rightarrow$ ogni sottosistema puo' rispondere a eventi generati da altri sottosistemi o dall'ambiente esterno

#### Controllo centralizzato
- Un sistema ha la responsabilita' di gestire il controllo complessivo
- modello "call-return"
	- gerarchia di procedure, il controllo inizialmente parte dalla routine alla base della gerarchia e si sposta verso il basso. Questo tipo di controllo si puo' applicare a sistemi sequenziali
- modello "manager"
	- una componente del sistema controlla l'interruzione, l'inizio e il coordinamento degli altri processi. Applicabile ai sistemi concorrenti. Puo' essere implementato in sistemi sequenziali come un blocco "case"

##### Modello call-return
- Si basa su una struttura gerarchica in cui un programma principale richiede una serie di procedure
- Il caso piu' classico e' quello di una struttura a subroutine annidate
- Le varie componenti possono anche essere attivate su nodi distribuiti: in questo caso l'architettura viene detta "a chiamata di procedure remote"
- Partendo da questo schema architetturale si sono evolute le architetture orientate agli oggetti

### Modello basato su eventi
- E' guidato da eventi generati dall'esterno, dove la temporizzazione degli eventi e' fuori dal controllo dei sottosistemi che gestiscono l'evento
- Due modelli "event-driven" principali
	- modello "broadcast" $\rightarrow$ un evento e' trasmesso a tutti i sottosistemi; ogni sottosistema in grado di gestire l'evento puo' farlo
	- modello "interrupt-driven" $\rightarrow$ usato nei sistemi real-time dove le interruzioni sono raccolti da un gestore di interruzioni e passate a un componente responsabile per processarle

#### Modello broadcast
- E' utile per integrare diversi sistemi collegati in rete
- Ciascun sottosistema si registra per ricevere particolari tipi di evento: quando questi eventi si verificano, il controllo viene passato ai sottosistemi registrati
- I sottosistemi decidono gli eventi di interesse; il gestore degli eventi deve solo registrare l'interesse dei sottosistemi per certi tipi di eventi e avvisarli quando ne accade uno

#### Modello interrupt-driven
- Usati in sistemi real-time dove la velocita' di risposta a un evento e la latenza sono essenziali
- Devono essere definiti tutti i tipi di interruzioni noti, e a ciascun tipo occorre associare un modulo capace di gestire quel particolare tipo di interruzione
- Ogni tipo e' associato a una locazione di memoria e uno switch hardware lo trasferisce al suo gestore
- Consentono rapidita' di risposta ma sono complicati da programmare e difficili da validare

## Organizzazione e raffinamento
Spesso si ha una serie di alternative: come chiarirsi le idee per scegliere uno stile architetturale?
- Controllo
	- come viene gestito il controllo all'interno dell'architettura?
	- esiste una gerarchia di controllo? qual'e' il ruolo dei componenti?
	- come viene diviso il controllo tra i componenti?
	- il controllo e' sincronizzato o i componenti sono asincroni?
	- ...
- Dati
	- i componenti come i passano i dati?
	- il data-flow e' continuo o i dati passano saltuariamente?
	- qual'e' la modalita' di trasferimento dei dati? (singolarmente o globalmente)
	- ...

## Architetture "domain-specific"
- Fino ad ora visti modelli generici
- Modelli di architettura di sistema che sono molto specifici a qualche dominio di applicazione
- Due tipi
	- modelli generici $\rightarrow$ sono astrazioni da un certo insieme di sistemi reali e contengono le caratteristiche principali di questi sistemi. Di solito sono modelli bottom-up
	- modelli di riferimento: sono modelli idealizzati, piu' astratti. Offrono informazioni su quella classe di sistemi che permettono di confrontare diverse architetture. Di solito sono modelli top-down.

## Modelli generici
- Consideriamo un compilatore. E' composto da:
	- analizzatore lessicale
	- tabella dei simboli
	- analizzatore sintattico
	- albero di derivazione
	- analizzatore semantico
	- generatore di codice
- Il modello generico di un compilatore puo' essere organizzato in base a diversi modelli architetturali

## Modelli di riferimento
- Derivano dallo studio del determinato dominio di applicazione piuttosto che dallo studio di sistemi esistenti
- Possono essere usati come base per l'implementare sistemi o per confrontare diversi sistemi
	- Hanno un ruolo "standard" rispetto al quale valutare un sistema
