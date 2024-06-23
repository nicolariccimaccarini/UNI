---

---
## 1 - Introduzione e Modellazione concettuale

### Definizioni
- **Database**: un insieme di dati correlati
- **Dati**: fatti che non possono essere memorizzati e che hanno un significato intrinseco
- **Mini-Mondo**: Un certo aspetto del mondo reale che viene proiettato in un database (es. voti di uno studente)
- **Database Management System (DBMS)**:  programma o insieme di programmi che permette la creazione e la manutenzione di un database elettronico
- **Database System**: il DBMS ed i dati contenuti nel database (a volte anche applicazioni per interagire con i dati)

### Tipi di Database
- Numerici e Testuali
- Multimediali
- Geographic Information System (GIS)
- Data Warehouses
- Real-Time and Active Database

### Funzioni di un DBMS
- Definire un database $\rightarrow$ tipi di dati strutture e vincoli
- Costruire un database $\rightarrow$ immagazzinare i dati
- Manipolare un database $\rightarrow$ interrogazioni, aggiornare, modificare, cancellare dati, ...
- Condividere un database $\rightarrow$ piu' utenti accedono al database
- Proteggere un database $\rightarrow$ controllo degli accessi, protezione dei dati, backup e recovery
- Operazioni sui dati $\rightarrow$ funzioni per eseguire operazioni sui dati
- Visualizzare i dati $\rightarrow$ presentazione dei dati in diversi formati

### Caratteristiche dell'approccio con database:
- **Natura autodescrittiva** di un database system:
	- esistenza di un catalogo in cui il DBMS memorizza una descrizione (meta-data) del database per permettere al DBMS di lavorare con diversi database
- **Isolamento (o indipendenza) tra programmi e dati**:
	- poter cambiare la struttura del database o la memorizzazione dei dati senza dover cambiare i programmi che accedono al DBMS
- **Astrazione dei dati**:
	- uso di un modello di dati per nascondere i dettagli di memorizzazione e rappresentazione dei dati all'utente
- **Condivisione dei dati e gestione delle transazioni con utenti multipli**:
	- utenti che accedono al database in modo concorrente con controllo degli accessi e delle transazioni (OLTP - online transaction processing, importante per i database)

### Utenti del database
possono essere:
- **Attori sulla scena**:
	- Amministratori $\rightarrow$ autorizzare accessi, coordinare e modificare l'uso e l'efficienza, acquistare software e hardware necessari
	- Progettisti $\rightarrow$ progettare la struttura del database e comunicare con gli utenti funali e capire le necessita'
	- Utenti finali (chi usa il database):
		- utenti occasionali (esperti)
		- utenti parametrici (maggior parte)
		- utenti esperti (esperti che lo usano per esigenze proprie)
		- utenti indipendenti
- **Lavoratori dietro le quinte**:
	- Progettisti ed implementatori di DBMS $\rightarrow$ creano funzionalita', interfacce, ...
	- Sviluppatori di strumenti $\rightarrow$ creano strumenti per facilitare progettazione, controllo, comunicazioni e prestazioni
	- Operatori per la manutenzione $\rightarrow$ mantenere efficiente hardware e software per il DBMS

### Vantaggi dei database
- Ridondanza ridotta nella memorizzazione dei dati
- Condivisione dati tra piu' utenti
- Controllo degli accessi ai dati
- Strutture di memorizzazioni efficienti per le query
- Servizi di Backup e recovery
- Interfacce multiple per gli utenti
- Rappresentazione di relazioni complesse tra i dati
- Garanzia sui vincoli di integrita'
- Standardizzazione dei dati
- Tempi di sviluppo ridotti
- Flessibilita' e scalabilita'
- Disponibilita' di informazioni aggiornate

### Quando non usare un database
- Costi eccessivi
- Non necessari
- Database non sufficiente

---
## 2 - Database System

### Modello dei dati
- Modello dei dati $\rightarrow$ insieme di concetti per descrivere la struttura di un database + i vincoli che deve rispettare
- Operazioni del modello dei dati $\rightarrow$ operazioni per specificare recuperi e aggiornamenti

### Categorie dei modelli dei dati
- **Concettuale** (high-level, semantico) $\rightarrow$ concetti vicini al modo in cui molti utenti percepiscono i dati
- **Fisico** (low-level, interno) $\rightarrow$ concetti che descrivono come i dati sono memorizzati nel computer
- **Implementabile** (mid-level, rappresentazionali) $\rightarrow$ concetti compresi dagli utenti ma non troppo lontani da come sono organizzati dentro il computer

### Schema vs Istanze vs Stato
- **Schema** $\rightarrow$ descrizione del database, include la descrizione della struttura e dei vincoli da rispettare
	- Diagramma di schema $\rightarrow$ rappresentazione grafica dello schema
	- Costrutto dello schema $\rightarrow$ componente o oggetto dello schema
- **Istanza** $\rightarrow$ dati effettivamente memorizzati in un database
- **Stato** $\rightarrow$ contenuto del database in un particolare momento
	- Stato iniziale $\rightarrow$ database quando viene caricato
	- Stato valido $\rightarrow$ stato che soddisfa le strutture ed i vincoli del database

| stato                                              | schema                        |
| -------------------------------------------------- | ----------------------------- |
| cambia ogni volta che il database viene aggiornato | non cambia molto spesso       |
| viene chiamato **estensione**                      | viene chiamato **intensione** |
### Architettura a 3 livelli
1. Supportare le caratteristiche dei DBMS di:
	- indipendenza tra programmi e dati
	- supporto di visite multiple d'utente
	- uso di un catalogo per memorizzare la descrizione (schema) del database
2. Separare l'applicazione dell'utente dalla base fisica dei dati
3. Definire gli schemi del DBMS a tre livelli:
	- **schema interno** $\rightarrow$ descrive le struttura di memorizzazione fisica ed i percorsi di accesso ai dati (modello fisico)
	- **schema concettuale (logico)** $\rightarrow$ descrive le strutture e i vincoli del database (modello concettuale o implementabile)
	- **schema esterno** $\rightarrow$ descrive le varie strutture utente (stesso modello del livello concettuale)

Per gli schemi e' necessario una mappatura tra i livello per trasformare le richieste ed i dati.

### Indipendenza dei dati
Quando uno schema di livello piu' basso viene modificato, si cambia solo la mappatura tra quello schema e quelli di livello superiore. I programmi applicativi non devono essere modificati.

- **Indipendenza logica** $\rightarrow$ cambiamenti dello schema logico senza influenzare gli schemi interni e le loro applicazioni
- **Indipendenza fisica** $\rightarrow$ cambiamenti dello schema interno (fisico) senza dover cambiare lo schema logico

### Linguaggi del DBMS
- **Data Definition Language (DDL)** $\rightarrow$ definizione dello schema logico, utilizzato a volte anche per lo schema interno ed esterno
- Se schemi definiti separatamente:
	- **Storage Definition Language (SDL)** $\rightarrow$ definizione dello schema interno (memorizzazione)
	- **View Definition Language (VDL)** $\rightarrow$ definizione dello schema esterno (visite)
- **Data Manipulation Language (DML)** $\rightarrow$ definire query e aggiornamenti del database
	- comandi incapsulati in linguaggi di programmazione (**data sublanguage**)
	- comandi applicati direttamente (**query language**)
- **Linguaggi di alto livello** (non procedurali) $\rightarrow$ specificano quali dati cercare e come estrarli (linguaggi dichiarativi, set-oriented, es. SQL)
- **Linguaggi di basso livello** (procedurali) $\rightarrow$ operano su un record alla volta, specificano come estrarre i dati e usano i cicli (es. C, Java, Python)

### Interfacce dei DBMS
- Interfacce basate su:
	- menu' (richieste composte da piu' step)
	- app per mobile
	- moduli (form, comuni in siti web)
- Interfacce user-friendly (grafiche)
- Lingua parlata (Alexa, Siri, Cortana)
- Parola chiave (motori di ricerca)
- Interfacce parametriche che fanno uso di tasti funzione.
- Interfacce per i DBA:
	- Per creare accounts ed impostare le autorizzazioni
	- Per impostare i parametri del sistema
	- Per modificare gli schemi o i percorsi di accesso ai dati

### Architetture Centralizzate e Client-Server
- **DBMS centralizzati** $\rightarrow$ unico server, tutti i client si collegano a lui
- **DBMS Client-Server** $\rightarrow$ server centrale, client si collegano a lui, server puo' collegarsi ad altri server

**Architettura Client-Server di base**
- server specializzati con funzioni specifiche
- client $\rightarrow$ interfaccia utente che usa risorse del server, sono connessi al server tramite rete (LAN, wireless, ...), possono essere PC, smartphone, tablet, ... con installato solo il programma client
- DBMS server $\rightarrow$ gestisce le query e transazioni ai clienti

**Architettura Client-Server a 2 livelli**
- programmi di interfaccia utente e applicativi eseguiti sulla macchina
- l'interfaccia ODBC (Open Database Connectivity) fornisce una API (Application Program Interface) che permette di collegare il client al server
- Un programma lato client puo' connettersi a piu' DBMS

**Architettura Client-Server a 3 livelli**
- utilizzato per le applicazioni web
- ha un livello intermedio (Web Server) che gestisce le richieste dei clienti e le inoltra al DBMS server e viceversa
- ha funzioni di sicurezza (cifratura dei dati prima della trasmissione e decrittazione sul client $\rightarrow$ https)

### Classificazione dei DBMS
- **Basata sul modello dei dati**:
	- Legacy: Reticolari, Gerarchici
	- Tradizionali: Relazionali
	- Emergenti: NOSQL, Key-Value, Document
- **Altre classificazioni**:
	- single-user vs multi-user
	- Centralizzati (un pc e un db) vs distribuiti (piu' pc e piu' db)

---
## 3 - Modello ER

## Database AZIENDA
- **Dipartimenti** (organizzazione dell'azienda) $\rightarrow$ nome, codice, data di creazione, codice responsabile
- **Progetti** (ogni dipartimento controlla $n$ progetti) $\rightarrow$ nome, codice, data di inizio, data di fine, budget, codice dipartimento
- **Dipendenti** $\rightarrow$ nome, cognome, data di nascita, indirizzo, telefono, codice fiscale, stipendio, data assunzione, codice dipartimento

### Modello ER
- **Entità** $\rightarrow$ semplici oggetti o cose del mini-mondo (es. DIPENDENTI, DIPARTIMENTI, PROGETTI)
- **Istanza** $\rightarrow$ singola occorrenza di un'entita' (es. Mario Rossi e' un'istanza di DIPENDENTI)
- **Attributo** $\rightarrow$ proprieta' usate per descrivere le entita' (es. nome e cognome di un dipendente)
- **Valore** $\rightarrow$ valore di un'attributo per un'istanza (es. Nome=‘Mario Rossi’, CF=‘MRRSSU73D24D548V’, Indirizzo=‘Via Paradiso 12’, ...)
- **Tipo di dati** $\rightarrow$ insieme di valori per un attributo (es. intero, stringa, data, ...)

### Tipi di attributi
- **Semplici** $\rightarrow$ ciascuna istanza ha un valore singolo, atomico per attributo (es. CF, sesso, ...)
- **Composti ()** $\rightarrow$ l'attributo puo' essere composto da varie componenti (es. Indirizzo (Via/Viale/Piazza, nome, numero, cap, paese))
- **Multivalore {}** $\rightarrow$ un'istanza puo' avere multipli per attributo (es. titolo di studio) 
- **Derivati** $\rightarrow$ può essere calcolato da altri attributi (età, data di nascita)
- **Data** $\rightarrow$ esiste un'attributo tutto suo (data di nascita, data di assunzione)

### Entità, Istanze e Attributi chiave
- Istanza dell'entita' $\rightarrow$ ogni elemento di un entita'
- attributo chiave $\rightarrow$ identifica univocamente un'istanza di un entita' (puo' essere anche compost es. targa di una macchina)
- Un'entita' puo' avere piu' di una chiave (es. l'entita' AUTO puo' avere due chiavi: Targa e NumeroTelaio)

### Notazione ER
![[NotazioneER.png]]

### Associazioni
- **Associazione** $\rightarrow$ mette in riferimento due o piu' entita' con uno specifico significato (es. DIPENDENTE lavora in un DIPARTIMENTO, PROGETTO e' assegnato a DIPARTIMENTO)
- **Istanza di associazione** $\rightarrow$ mette in riferimento due o piu' istanza di entita'
- **Grado di associazione** $\rightarrow$ numero di entita' partecipanti (es. LAVORA SU e' un associazione binaria (grado 2))
- **Associazione ricorsiva** $\rightarrow$ un'entita' e' associata a se stessa

### Entita' Deboli ed Associazioni Identificanti
- **Entita' debole** $\rightarrow$ entita' che non possiede un attributo chiave
	- deve partecipare a un'associazione identificante con un'altra entita' (non debole) identificante
- **Associazione identificante** $\rightarrow$ associazione che identifica un'entita' debole associandola ad un'entita' non debole

### Vincoli sulle associazioni
- **Cardinalita' massima** (quanti collegamenti possono esserci tra le entita' coinvolte):
	- 1:1 $\rightarrow$ uno a uno
	- 1:N o N:1 $\rightarrow$ uno a molti o molti a uno
	- N:M $\rightarrow$ molti a molti
- **Cardinalita' minima** (quanti collegamenti devono almeno esserci tra le entita' coinvolte):
	- 0 $\rightarrow$ partecipazione opzionale
	- 1 o piu' $\rightarrow$ partecipazione obbligatoria

### Attributi delle associazioni
- Un'associazione puo' avere degli attributi
- Piu' associazioni possono avere le stesse entita' coinvolte
- Spostamenti di un attributo tra due entita' associate;
	- 1:1 $\rightarrow$ l'attributo puo' essere spostato ad una qualsiasi delle entita' partecipanti
	- 1:N $\rightarrow$ l'attributo puo' essere spostato all'istanza partecipante di cardinalita' maggiore
	- N:M $\rightarrow$ l'attributo deve essere proprio dell'associazione 

### Vincoli strutturali
- **Rapporto di cardinalità** (di associazioni binarie)
		  *1:1*, *1:N*, *N:1*, oppure *M:N* 
	- MOSTRATO INDICANDO IL NUMERO APPROPRIATO SUL SEGMENTO TRA ENTITA' E ASSOCIAZIONE
- **Vincolo di partecipazione** (su ciascuna entita' partecipante)
		  *Totale* (dipendenza di esistenza) oppure *parziale*
	- MOSTRATO UTILIZZANDO UN SEGMENTO DOPPIO

## Notazione alternativa (min, max)
- Specificata su tutte le partecipazioni di un'entita' E da un'associazione R.
- Specifica che ciascuna istanza *e* dell'entita' E partecipa in almeno *min* ed al massimo *max* istanze di associazione dell'associazione R.

### Problemi principali
- Disegno
- Metodologia
- Il modello ER nella sua forma originale non sopporta:
	- sottoclassi
	- specializzazione/generalizzazione

---
## 4 - Modello EER
Modello ER esteso con concetti addizionali: sottoclassi/superclassi, specializzazione, categorie, propagazione (ereditarieta') degli attributi.
E' utilizzato per modellare applicazioni in maniera piu' accurata e piu' specifica.

### Sottoclassi e Superclassi
- Un'entita' potrebbe avere alcuni sottogruppi addizionali di istanze, aventi un significato particolare per il mini-mondo di interesse
	- Esempio: DIPENDENTE potrebbe essere ulteriormente suddiviso in SEGRETARIO, INGEGNERE, DIRETTORE, TECNICO, TEMPORANEO, TEMPO_INDETERMINATO, ...
		- Ciascuno di questi gruppi e' costituito da un sottoinsieme delle istanze dell'entita' DIPENDENTE
		- Ciascuno di questi gruppi viene chiamato una **sottoclasse** di DIPENDENTE
		- DIPENDENTE e' chiamata **superclasse** di ciascuna di queste sottoclassi
- Un'istanza di superclasse puo' appartenere a una o piu' sottoclassi

### Ereditarieta' degli attributi
- Un'entita' che sia membro di una sottoclasse eredita tutti gli attributi dell'entita' considerata come membro della superclasse
- Eredita inoltre anche tutte le associazioni

### Specializzazioni
- **Specializzazione** $\rightarrow$ processo di definizione di un insieme di sottoclassi di una superclasse
- Attributi specifici $\rightarrow$ attributi di una sottoclasse che non appartengono alla superclasse. Una sottoclasse puo' partecipare ad associazioni specifiche che non coinvolgono la superclasse
- Rappresentazione:
	- frecce dirette verso le sottoclassi

### Generalizzazione
- **Generalizzazione** $\rightarrow$ definire una superclasse che rappresenta un insieme di sottoclassi
- Le classi originali diventano sottoclassi della superclasse creata
- Rappresentazione:
	- frecce dirette verso la superclasse

### Vincoli di specializzazione/generalizzazione
- **Sottoclasse definita da una condizione** $\rightarrow$ se possibile determinare esattamente le istanza che diventeranno membri di una sottoclasse mediante una condizione
- **Specializzazione definita da un attributo** $\rightarrow$ se tutte le sottoclassi di una specializzazione hanno la condizione di appartenenza sullo stesso attributo
- **Sottoclasse definita dall'utente** $\rightarrow$ se nessuna condizione determina l'appartenenza
- **Vincoli di disgiunzione**:
	- **totale** (**o** = overlap) $\rightarrow$ la stessa istanza non puo' appartenere a piu' di una sottoclasse della specializzazione
	- **parziale** (**d**) $\rightarrow$ un'istanza puo' appartenere a una sola sottoclasse della specializzazione
- **Completezza**:
	- **totale** (doppia linea) $\rightarrow$ ogni istanza della superclasse deve appartenere ad almeno una sottoclasse della specializzazione
	- **parziale** (singola linea) $\rightarrow$ le stanze possono appartenere a nessuna sottoclasse

### Gerarchie e Reticoli
Una sottoclasse puo' avere in essa ulteriori sottoclasse specifiche su di essa (ed essere quindi superclasse di queste)
- **Gerarchia** $\rightarrow$ impone il vincolo che ciascuna sottoclasse abbia una sola superclasse (ereditarieta' singola)
- **Reticolo** $\rightarrow$ una sottoclasse puo' essere sottoclasse di piu' di una superclasse (ereditarieta' multipla)
- Proprieta':
	- Una sottoclasse eredita' gli attributi della superclasse diretta e anche delle superclassi precedenti.
	- si possono avere gerarchie o reticoli di specializzazione ma anche di generalizzazione
		- specializzazione $\rightarrow$ processo di raffinamento top-down
		- generalizzazione $\rightarrow$ processo di sintesi concettuale bottom-up

### Categorie (tipi di UNIONE)
- **Categorie** $\rightarrow$ quando una singola associazione superclasse/sottoclasse ha piu' di una superclasse
	- es. La categoria (sottoclasse) PROPRIETARIO contiene un *sottoinsieme* delle istanze provenienti dall'**unione** delle istanze delle entita' DITTA, BANCA, PERSONA (superclassi)
	- un membro della categoria deve esistere in almeno una delle sue superclassi

- Categorie:
	- contiene l'unione di istanze di superclassi
	- un'istanza deve esistere in almeno una superclasse
- Sottoclassi condivise:
	- contiene l'intersezione di istanze e superclassi
	- un'istanze deve esistere in tutte le superclassi

---
## 5 - Modello dei dati relazionale