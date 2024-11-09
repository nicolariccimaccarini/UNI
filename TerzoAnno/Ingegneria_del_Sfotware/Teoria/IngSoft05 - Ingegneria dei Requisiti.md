- Le attivita' con cui si stabilisce cio' che il committente chiede al sistema software e i vincoli che il sistema deve soddisfare in fase di sviluppo e in fase operativa

## Analisi dei requisiti
- Studio della realta' applicativa
- Identificazione dei confini tra l'applicazione da sviluppare e "resto del mondo"
- Comprensione degli attributi di qualita' richiesti
- attivita' esplorativa e incrementale
- **In questa fase non ci chiediamo come realizzeremo l'applicativo, ma ci concentriamo nell'elencare cosa deve fare**

### Requisiti?
Requirements: ci servono per specificare cosa costruire
- ci dicono "cosa", non "come"
- ci dicono il problema, non la soluzione
- riflettono (sono pertinenti) al system design, non al software design

### "Cosa vs. Come": e' relativo
- Input file processing e' il cosa, parsing e' il come
- Parsing e' il cosa, uno stack e' il come
- Uno stack e' il cosa, un array o una linked list e' il come
- Una linked list e' il cosa, un doubly linked list e' il come

### Perche' l'analisi dei requisiti?
- Alcune delle ragioni per cui l'analisi dei requisiti e' necessaria
	- comprendere precisamente che cosa richiede il sistema
	- comunicare questa comprensione in modo preciso a tutti gli sviluppatori coinvolti
	- controllare la produzione per assicurarsi che il sistema soddisfi le specifiche (comprese le eventuali variazioni in corso d'opera)

### Ruoli nell'analisi dei requisiti
- customers: spiegano cosa deve essere consegnato
- managers: controllano scheduling e avanzamento lavori
- designers: producono le specifiche del design
- programmatori: preparano la lista di implementazioni accettabili/output
- QA/testers: pensare a un insieme di test di base, per la validazione, verifica, ...

## Definizione vs Specifica dei requisiti
- **Definizione dei requisiti**
	- delle frasi in linguaggio naturale, con eventuale aggiunta di diagrammi dei servizi che il sistema deve offrire e dei suoi vincoli operativi. E' rivolta al cliente
- **Specifica dei requisiti**
	- documento strutturato che fissa una descrizione dettagliata dei servizi che il sistema deve offrire; di solito viene scritta come contratto tra il cliente e il fornitore del software
- **Specifica del software**
	- una descrizione dettagliata del software che serva come base per il disegno dell'architettura o per l'implementazione. E' rivolta agli sviluppatori

![[aChiSonoRivoltiRequisiti.png]]

## Descrizione dei requisiti
- Metodo classico per classificare i requisiti: due tipi di requisiti
	- *funzionali* $\rightarrow$ descrivono i servizi offerti dal sistema o le sue funzionalita'
	- *non funzionali* $\rightarrow$ vincoli sul processo di sviluppo o sul sistema in generale

- Requisiti di dominio?
	- derivano dal dominio di applicazione del sistema, non dagli utenti
	- terminologia propria del dominio di applicazione o si riferiscono a suoi concetti
	- possono essere nuovi requisiti funzionali o porre vincoli a altri requisiti

## Cosa intendiamo con "requisito"
- Grande visibilita', da una descrizione informale dei servizi che il sistema deve fornire, o dei vincoli che deve soddisfare, a una specifica funzionale dettagliata e formalmente definita.
- Il motivo di cio' e' che i requisiti servono a molteplici scopi
	- puo' essere la base di un'offerta per un lavoro: aperta a interpretazioni e modifiche
	- puo' essere parte fondamentale del contratto stesso: massimo livello di dettaglio, niente interpretazioni

## Requisiti: come ottenerli?
Cosa fare:
- parlare con gli utenti, o lavorare un po' con loro, per capire in che modo lavorano
- fate domande nelle varie fasi per "scavare a fondo", non date le cose per scontate
- pensate al perche' un utente fa qualcosa, non soltanto a cosa fa
- aspettatevi fin dall'inizio che i requisiti possono evolvere e cambiare

Cosa non fare:
- non siate troppo specifici o dettagliati (non a questo livello)
- non provate a pensare a tutto in anticipo (fallireste)
- non aggiungete features non richieste: non sono necessarie

## Imprecisione nei requisiti
- Cosa intendiamo con "viewers appropriati"?
- nelle intenzioni del committente, un programma specifico per ogni diverso  tipo di documento
- nell'interpretazione dello sviluppatore, un unico visualizzatore di testo che mostri il contenuto di qualsiasi tipo di documento

### Imprecisioni: quali le ragioni?
- I problemi nascono quando i requisiti non sono enunciati con precisione
- Requisiti ambigui possono essere interpretati in modo diverso da sviluppatori e utenti
- In principio i requisiti dovrebbero essere sia completi che consistenti
	- completi $\rightarrow$ dovrebbero includere la descrizione di tutto cio' che e' richiesto
	- consistenti $\rightarrow$ non ci dovrebbero essere conflitti o contraddizioni nelle descrizioni delle funzionalita' richieste
- In pratica e' impossibile produrre un document dei requisiti completo e consistente: sotto questo aspetto la prototipazione puo' essere molto utile per chiarire le cose

## Requisiti non funzionali
- Definiscono proprieta' e vincoli sul sistema
- Possono anche esser vincoli di processo che impongono l'uso di un particolare sistema di sviluppo, linguaggio di programmazione o libreria software
- I requisiti non funzionali possono anche essere piu' critici di quelli funzionali: se non soddisfatti il sistema puo' essere inutilizzabile 

- Product requirements
	- vincoli che specificano il comportamento del prodotto finale, velocita', reliability, ...
- Requisiti organizzativi
	- sono conseguenza di policy organizzative del cliente, standard di processo da usare, requisiti implementatavi, ... 
- Requisiti esterni
	- sono esterni al sistema e al suo processo di sviluppo, p.e. requisiti di interoperabilita', requisiti legislativi, ... 

## Problema dei requisiti di dominio
- Derivano dal dominio di applicazione
- Comprensibilita'
	- questi requisiti sono formulati nel linguaggio del dominio applicativo
	- non e' affatto detto che gli sviluppatori "parlino" questo linguaggio
- Implicita'
	- lo specialista di dominio puo' conoscere la sua area di lavoro cosi' bene da non pensare di dover esplicitamente richiedere alcune cose: per lui sono implicite

## Uso dei requisiti
1. definizione dei bisogni del committente
	- spesso non chiari neppure al committente
	- committente e realizzatore non si capiscono
2. definizione dei requisiti dell'implementazione
	- documento di riferimento dei progettisti
3. documento di riferimento per la manutenzione
	- distinguere chiaramente tra manutenzione correttiva e adattiva/perfettiva

## Fasi del processo dell'ingegneria dei requisiti
1. Studio fattibilita'
	- quali sono le necessita' che possono essere soddisfatte con la tecnologie e le risorse disponibili?
2. Analisi dei requisiti
	- quali servizi il sistema deve offrire, qual'e' il dominio di applicazione, performances richieste, vincoli hw, ...
3. Definizione dei requisiti
	- frasi in linguaggio naturale piu' eventuali diagrammi dei servizi che il sistema deve offrire e dei suoi vincoli operativi. E' rivolta al cliente
4. Specifica dei requisiti
	- documento strutturato che fissa una descrizione dettagliata dei servizi che il sistema deve offrire. E' alla base del contratto
5. Specifica del software
	- descrizione dettagliata del software che serva come base per il disegno dell'architettura o per l'implementazione. E' rivolta agli sviluppatori.

![[fasiProcessoIngReq.png]]

## Il documento dei requisiti
- Il documento dei requisiti definisce ufficialmente cio' che viene richiesto al sistema
	- cioe' cosa e' richiesto che gli sviluppatori producano
- Include sia la specifica che la definizione dei requisiti
- Non e' un documento di design
	- dovrebbe limitarsi a dire "cosa" il sistema deve fare, ma non il "come" lo deve fare

## Avvio del processo
- La tecnica piu' usata per individuare i requisiti consiste nel condurre riunioni o interviste col committente e gli utilizzatori finali
- Solitamente si parte da domande che conducano a fissare in termini generali il problema
	- chi c'e' dietro la richiesta di questo lavoro?
	- chi utilizzera' la soluzione?
	- quali saranno i vantaggi economici di una soluzione di successo?
	- esiste un'altra fonte per la soluzione di cui si ha bisogno?
- Serve per identificare gli stakeholder, i benefici misurabili e le alternative

- Secondo insieme di domande: consente all'analista di acquisire maggiore comprensione del problema, e al committente di comunicare le proprie percezioni riguardo una soluzione
	- come caratterizzare un "buon" output generato da una soluzione di successo?
	- quali problemi deve risolvere questa soluzione?
	- potete mostrare/descrivere l'ambiente in cui la soluzione dovra' operare?
	- esistono particolari richieste di prestazioni o vincoli generali che incidono sulla soluzione?

- L'ultimo gruppo di domande riguarda l'attivita' di comunicazione (meta-domande)
	- siete voi la persona corretta per rispondere a queste domande?
	- le mie domande sono rilevanti rispetto al problema?
	- sto facendo troppe domande?
	- ci sono altre persone in grado di fornirmi informazioni supplementari?
	- c'e' qualcos'altro che dovrei chiedere?



