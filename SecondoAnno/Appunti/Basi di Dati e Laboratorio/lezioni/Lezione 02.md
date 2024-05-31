
## Modelli dei Dati
- **Modelli dei Dati**: Un insieme di concetti per descrivere la struttura di un database e alcuni vincoli che il database deve rispettare.
- **Operazioni del modello dei dati**: Operazioni per specificare recuperi ed aggiornamenti del database in riferimento ai concetti del modello dei dati. Possono includere operazioni di base e operazioni definite dall'utente.

## Categorie dei modelli di dati
- **Concettuale (high-level, semantico)**: Forniscono concetti che sono vicini al modo in cui molti utenti percepiscono i dati. (Esempio: entita', oggetti).
- **Fisico (low-leve, interno)**: Forniscono concetti che descrivono dettagliatamente come i dati sono memorizzati nel computer.
- **Implementabile (rappresentazionali)**: Forniscono concetti che possono essere compresi dagli utenti finali ma che non sono troppo lontani dal modo in cui i dati sono organizzati dentro il computer.

## Schema vs. Istanze
- **Schema di un Database**: La descrizione del database. Include la descrizione della struttura e dei vincoli da rispettare.
- **Diagramma di schema**: Una visualizzazione a diagramma di (qualche aspetto) di uno schema.
- **Costrutto dello schema**: Una componente dello schema o un oggetto all'interno dello schema. (esempio: [STUDENTI, CORSI](obsidian://open?vault=Obsidian%20Vault&file=Basi%20di%20Dati%20e%20Laboratorio%2Fimg%2Fdb1.png))
- **Istanze del Database**: I dati veri e propri contenuti nel database in un particolare momento del tempo. Chiamata anche stato del database oppure occorrenza.

## Schema vs. Stato
- **Stato del Database**: Si riferisce al contenuto del database in un particolare momento (solitamente "adesso").
- **Stato inziale del Database**: Si riferisce al database quando viene caricato
- **Stato valido**: Uno stato che soddisfa le strutture ed i vincoli del database.
- **Distinzione**:
	- Lo **schema del database** non cambia molto spesso. Lo **stato del database** cambia ogni volta che il database viene aggiornato.
	- Lo **schema** viene chiamato anche **intensione**, mentre lo **stato** viene chiamato **estensione**.

## Architettura a 3 livelli (Three-Schema Architecture, ANSI/SPARC)
Proposta per supportare le caratteristiche dei DBMS di:
- Indipendenza tra programmi e dati
- Supporto di viste multiple d'utente
- Uso di un catalogo per memorizzare la descrizione (schema) del database

Obiettivo di separare l'applicazione dell'utente dalla base di dati fisica.
Definisce gli schemi del DBMS a tre livelli:
- **Schema interno**: al livello interno, per descrivere le strutture di memorizzazione fisica ed i percorsi di accesso ai dati. Tipicamente si usa un modello dei dati fisico.
- **Schema concettuale (logico)** al livello concettuale, per descrivere le strutture ed i vincoli per tutto il database per una comunita' di utenti. Usa un modello dei dati concettuale o implementabile.
- **Schema esterno** al livello esterno, per descrivere le varie strutture utente. Solitamente utilizza lo stesso modello dei dati del livello concettuale.

E' necessaria una [mappatura](obsidian://open?vault=Obsidian%20Vault&file=Basi%20di%20Dati%20e%20Laboratorio%2Fimg%2FA3L.png) tra i livelli di schemi per trasformare le richieste ed i dati.
I programmi applicativi fanno riferimento ad uno schema esterno e sono mappati dal DBMS sullo schema interno per l'esecuzione.

## Indipendenza dei dati
- **Indipendenza logica dei dati**: Capacita' di modificare lo schema logico senza dover cambiare gli schemi esterni e le loro applicazioni.
- **Indipendenza fisica dei dati**: Capacita' di modificare lo schema interno senza dover cambiare lo schema logico.

Quando uno schema di livello piu' basso viene modificato, solamente la mappatura tra questo schema e quelli di livello superiore devono essere modificate in un DBMS che supporti pienamente l'indipendenza dei dati.
I livelli superiori rimangono inalterati. Quindi i programmi applicativi non devono essere modificati perche' si riferiscono solamente allo schema esterno.

## Linguaggi dei DBMS
- **Data Definition Language (DDL)**: Usato dal DBA e dai progettisti per specificare lo **schema logico** del database. In molti DBMS, il DDL e' utilizzato anche per definire gli schemi interno ed esterno. In qualche DBMS, sono usati linguaggi separati per definire lo schema interno (**storage definition language, SDL**) e quello interno (**view definition language, VDL**).
- **Data Manipulation Language (DML)**: Usati per specificare le interrogazioni e gli aggiornamenti del database.
	- I comandi DML (**data sublanguage**) possono essere **incapsulati** in un linguaggio di programmazione classico (**linguaggio ospite**), come COBOL, C, Assembler.
	- In alternativa, i comandi DML **stand-alone** possono essere applicati direttamente (**query language**).

- **Linguaggi di alto livello** o **non procedurali**: sono **set-oriented** e specificano quali dati cercare e come estrarli. Sono anche chiamati linguaggi **dichiarativi**. Esempio: SQL!
- **Linguaggi di basso livello** o **procedurali**: operano su di un record alla volta; specificando come estrarre i dati ed includono costrutti come i cicli.

## Interfacce dei DBMS
- Interfacce basate su menu' dove le richieste vengono composte da piu' step
- Interfacce basate su App per dispositivi mobili
- Interfacce basate sui moduli (form) molto comuni sul Web
- Interfacce User-friendly:
	- grafiche (Point and Click, Drag and Drop, ecc.)

- Lingua parlata: es. Alexa, Siri, Cortana
- Parola chiave: es. motori di ricerca
- Interfacce parametriche che fanno uso di tasti funzione.
- Interfacce per i DBA:
	- Per creare accounts ed impostare le autorizzazioni
	- Per impostare i parametri del sistema
	- Per modificare gli schemi o i percorsi di accesso ai dati

## Architetture Centralizzate e Client-Server
- **DBMS Centralizzati**: tutte le funzionalita' sono raccolte in un singolo sistema; i programmi del DBMS, i programmi applicativi, le interfacce utente ed il database stesso sono su un unico computer.
### [Architetture Client-Server di base](obsidian://open?vault=Obsidian%20Vault&file=Basi%20di%20Dati%20e%20Laboratorio%2Fimg%2FClient-Server.png) 
- Server specializzati con funzioni specifiche
- Client
- DBMS Server

#### Server Specializzati
- File Servers
- Printer Servers
- Web Servers
- E-mail Servers

#### Clients
- Forniscono interfacce appropriate ed una versione client del sistema per accedere ed utilizzare le risorse del server. 
- I client possono essere delle macchine senza disco oppure dei PC o delle Workstation con installato solamente il programma client.
- Sono connessi ai server attraverso qualche tipo di rete (LAN: local area network, wireless network, etc.)

#### DBMS Server 
- Forniscono i servizi di interrogazione e di transizione ai client.
- Per questo a volte sono detti query and transaction server.

### Architetture Client-Server a due livelli per DBMS
- I programmi di interfaccia utente e gli applicativi sono eseguiti sulla macchina client.
- Un'interfaccia chiamata ODBC (Open Database Connectivity) fornisce una API (Application Program Interface) che consente ai programmi lato client di effettuare chiamate al DBMS.

La maggior parte dei produttori di DBMS forniscono i driver ODBC

- Un Programma lato client puo' connettersi a piu' DBMS.
- Sono possibili variazioni sul tema client: in qualche DBMS alcune funzionalita' proprie del server sono trasferite ai client, come ad esempio le funzioni di dizionario dati, di ottimizzazione e recovery. In questo caso il server viene indicato solamente come Data Server.

### [Architetture Client-Server a tre livelli](obsidian://open?vault=Obsidian%20Vault&file=Basi%20di%20Dati%20e%20Laboratorio%2Fimg%2FClient-Server-3L.png)
- Utilizzato per le applicazioni Web.
- Un livello intermedio chiamato Application Server o Web Server:
	- contiene il software per le connessioni web e le regole e la logica (vincoli) dell'applicazione usate per accedere ai dati necessari nel DBMS.
	- agisce come tramite per mandare i dati parzialmente processati tra server e client del DBMS.
- Ulteriori funzionalita' e sicurezze:
	- cifratura dei dati a livello server prima della trasmissione
	- decrittazione dei dati su client

## Classificazione dei DBMS
- **Basata sul modello dei dati utilizzato**:
	- Legacy: Reticolari, Gerarchici
	- Tradizionali: Relazionali
	- Emergenti: NOSQL, Key-Value, Document

- **Altre classificazioni**:
	- **Single-user** (usati tipicamente su PC) vs.  **multi-user** (la maggior parte dei DBMS).
	- **Centralizzati** (usano un singolo computer con un database) vs. **Distribuiti** (usano piu' computer e piu' database).

**Distributed Database Systems** have now come to be known as *client server based database systems* because they do not support a totally distributed environment, but rather a set of database servers supporting a set of clients.

## Ambienti distribuiti
- **DBMS Distribuiti omogenei**
- **DBMS Distribuiti disomogenei**
	- **Sistemi Multi-Database**
