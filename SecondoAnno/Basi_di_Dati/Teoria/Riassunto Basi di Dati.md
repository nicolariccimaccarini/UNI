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
Modello basato sul concetto di relazione (concetto matematico basato sulla teoria degli insiemi).

### Definizioni informali
- **Relazione** $\rightarrow$ tabella di valori rappresentata come un'insieme di righe o colonne.
	- riga $\rightarrow$ rappresenta un'istanza di un'entita' o di una associazione del mini-mondo
	- ogni riga ha un valore per elemento ad un insieme di elementi che la compongono che identifica univocamente la riga
	- si possono assegnare identificatori di riga (row-id) o dei numeri sequenziali per identificare le righe nella tabella
	- ciascuna colonna viene tipicamente individuata mediante il nome o l'interstazione della colonna o il nome dell'atributo

### Definizioni formali
Una relazione piu' essere definita in molti modi:
- **Schema di una relazione**: $R(A_1, A_2, \ldots, A_n)$
	- lo schema della relazione $R$ e' definito sugli attributi $A_1, A_2, \ldots, A_n$
	- Esempio:
		- CLIENTI (ID-Cliente, Nome-Cliente, Indirizzo, Tel)
		- CLIENTI e' una relazione definita sui quattro attributi ID-Cliente, Nome-Cliente, Indirizzo e  Tel, ciascuno dei quali possiede un **dominio**, o insieme dei valori validi.
- **Tuple**
	- Insieme ordinato di valori
	- Esempio:
		- Ciascuna riga nella tabella CLIENTI puo' essere vista come una tupla della relazione, composta da quattro valori.
		- <632895, “Giacomo Piva", "via Saragat 1, 44122, Ferrara", "+39 (0532) 974344">
	- Una relazione puo' essere trattata come un insieme di tuple (righe)
	- Le colonne della tabella sono chiamate attributi della relazione
- **Dominio**
	- Ha una definizione logica (es. "Numeri di Telefono" e' l'insieme dei numeri telefonici validi)
	- Puo' avere:
		- un tipo di dati (es. numeri di telefono $\rightarrow$ stringa di caratteri)
		- formato ("Numeri di Telefono" puo' avere il formato: `+dd (dddd) dddddddd` dove d e' una cifra decimale)

- **Stato della Relazione** (o relazione)
	- e' formata dal prodotto cartesiano degli insiemi dominio dove ciascun insieme ha dei valori provenienti dal dominio (usato per definire il ruolo dell'attributo in questione)

| Termini Informali      | Termini Formali     |
| ---------------------- | ------------------- |
| Tabella                | Relazione           |
| Colonna                | Attributo/Dominio   |
| Riga                   | Tupla               |
| Valori di una colonna  | Dominio             |
| Definizione di Tabella | Schema di Relazione |
| Tabella Popolata       | Estensione (Stato)  |
### Caratteristiche delle relazioni
- Le tuple non devono considerarsi ordinate anche se appaiono in forma tabulare
- Gli attributi $R(A_1, A_2, \ldots, A_3)$ ed i valori in $t=<v_1, v_2, \ldots, v_n>$ devono essere considerati ordinati
- Una definizione ancora piu' generale di una relazione non richiede alcun tipo di ordinamento
- Tutti i valori di una tupla sono considerati atomici (indivisibili) e NULL per i valori sconosciuti

### Vincoli di integrita'
Sono condizioni che devono essere rispettate da tutti gli stati di relazione validi.
3 tipi di vincoli:
- vincoli su chiave
- vincoli di integrita' delle entita'
- vincoli di integrita' referenziale

**Vincoli su chiave**
- Superchiave di $R$: insieme di attributi che identificano univocamente una tupla (per ogni coppia di tuple distinte $t_1$ e $t_2$ in $r(R)$, $t_1[SK] \ne t_2[SK]$).
- Chiave di $R$: superchiave minimale (se si rimuove un attributo dalla superchiave, non e' piu' una superchiave)
- Chiave primaria di $R$: chiave scelta come riferimento per la relazione tra varie possibili chiavi

**Integrità sulle Entità** 
- Gli attributi chiave primaria PK di ciascuno schema di relazione $R$ in $S$ non possono avere dei valori *null* in nessuna tupla $r(R)$

**Integrità Referenziale**
- Vincolo che coinvolge due relazioni
- Usato per specificare un riferimento tra tuple in due relazioni (relazione referenziale e relazione riferita)
- Tuple nella relazione referenziante $R_1$ hanno attributi *FK* (chiamati attributi chiave esterne) che fanno riferimento agli attributi chiave primarie *PK* della relazione riferita $R_2$.
- Un vincolo di integrita' referenziale puo' essere indicato in uno schema relazionale con un arco diretto da $R_1.FK$ a $R_2.FK$.

**Vincoli di integrità semantici** 
- Basati sulla semantica dell'applicazione
- Non possono essere espressi dal modello dei dati svincolato dall'applicazione/dati

### Operazioni di aggiornamento sulle relazioni
- **Inserimento** di una tupla (INSERT)
- **Cancellazione** di una tupla (DELETE)
- **Modifica** di una tupla (MODIFY, UPDATE)
- I vincoli di integrita' non devono essere violati dalle operazioni di aggiornamento delle relazioni
- Una serie di operazioni di aggiornamento possono essere raggruppate insieme
- Le operazioni di aggiornamento possono creare in automatico altri aggiornamenti
- Se un operazione di aggiornamento viola un vincolo di integrità:
	- si annulla l'operazione che causa la violazione (REJECT)
	- si esegue l'operazione, informando l'utente della violazione
	- si eseguono altri aggiornamenti per correggere la violazione (CASCADE, SET NULL)
	- si esegue una routine dall'utente per correggere la violazione

---
## 5b - dal modello concettuale al modello logico

### ER - Relazionale
1. **Entità**
	- per ogni entita' $E$ si crea una relazione $R$ con tutti gli attributi semplici di $E$
	- si sceglie uno degli attributi chiavi di $E$ come chiave primaria di $R$
		- se la chiave $E$ scelta e' composta, l'insieme degli attributi semplici che la formano saranno chiave primaria di $R$.
2. **Entità deboli**
	- per ogni entita' debole $W$ con entita' prioritaria, si crea una relazione $R$ con tutti gli attributi semplici di $W$ e come chiave esterna gli attributi chiave primaria di $E$
	- chiave primaria di $R$ $\rightarrow$ combinazione della chiave primaria dell'entita' $E$ e della chiave parziale dell'entita' debole $W$
3. **Associazioni binarie 1:1**
	- per ogni associazione si identificano le relazioni $S$ e $T$:
		- chiave esterna $\rightarrow$ si sceglie una delle relazioni e si include come chiave esterna la chiave primaria dell'altra relazione, si inseriscono poi tutti gli attributi di $A$ in $S$
		- unica relazione $\rightarrow$ si uniscono le due entita' e l'associazione in una singola relazione
		- relazione associazione $\rightarrow$ si crea una terza relazione $R$ per avere un riferimento per le due relazioni $S$ e $T$
4. **Associazioni binarie 1:N**
	- per ogni associazione $A$ si identificano le relazioni $S$ (lato N) e $T$ (lato 1):
		- si include come chiave esterna in $S$ la chiave primaria di $T$
		- si includono tutti gli attributi semplici di $A$ in $S$
5. **Associazione binarie N:M**
	- per ogni relazione $A$ si crea una relazione $S$ che rappresenti $A$
		- si includono come chiavi estere in $S$ le chiavi primarie delle due relazioni coinvolte in $A$, la loro combinazione sara' la chiave primaria di $S$
		- si includono tutti gli attributi di $A$ in $S$
6. **Attributi multivalore**
	- Per ogni attributo multivalore $A$ si crea una nuova relazione $R$ dove questa avra' un attributo corrispondente ad $A$ e l'attributo $K$ corrispondente alla chiave primaria di $A$
	- La chiave primaria di $R$ e' la combinazione di $A$ e $K$
7. **Associazioni N-arie**
	- per ogni associazione $A$ n-aria ($n > 2$) si crea una relazione $S$ per rappresentare $A$:
		- si includono come chiavi esterne in $S$ le chiavi primarie delle relazioni che rappresentano le n entità coinvolte in $A$
		- si includono tutti gli attributi di $A$ in $S$

### EER - Relazioni
8. **Specializzazioni e generalizzazioni**
	- Conversione di ogni specializzazione di $m$ sottoclassi $\{ S_1, S_2, \ldots, S_m \}$ e superclasse generalizza $C$ con attributi $\{ k, A_1, \ldots, A_n \}$ ($k$ attributo chiave di $C$) in uno schema relazionale secondo le seguenti opzioni:
		- **Opzione A**: relazioni multiple - superclasse e sottoclasse
			- si crea la relazione $L$ per $C$ con i suoi attributi $Attr(L)$ e la sua chiave primaria $PK(L)=k$
			- si crea la relazione $L[i]$ per ogni sottoclasse $S[i], \space 1<i<m$, con attributi $Attr(L[i]) = \{k\} \cup \{ \text{attr di S[i] e chiave primaria PK(L[i])=k} \}$ 
		- **Opzione B**: relazioni multiple - solo relazioni sottoclasse
			- si crea una relazione $L[i]$ per ogni sottoclasse $S[i], \space 1<i<m$, con attributi $Attr(L[i]) = \{ \text{attr di S[i]} \} \cup \{ k, A_1, \ldots, A_n \}$ e chiave primaria $PK(L[i])$
		- **Opzione C**: relazione singola con un attributo tipo
			- si crea una relazione $L$ con gli attributi $Attr(L) = \{k, A1, ..., An\} \cup \{\text{attr. di S1}\} \cup \ldots \cup \{\text{attr. di Sm}\} \cup \{t\}$ e chiave primaria $PK(L) = k$ 
			- l'attributo $t$ è chiamato tipo e indica a quale sottoclasse appartiene ogni tupla
		- **Opzione D**: relazione singola con molti attributi tipo
			- si crea una relazione $L$ con gli attributi $Attr(L) = \{k, A_1, \ldots, A_n\} \cup \{\text{attr. di S1}\} \cup \{\text{attr. di Sm}\} \cup \{t_1, t_2, \ldots, t_m\}$ e chiave primaria $PK(L)=k$
			- ogni $t[i], \space 1<i<m$ e' un attributo booleano (flag) che indica se la tupla appartiene alla sottoclasse $S[i]$

---
### 6 - Algebra e calcolo relazionale
### Algebra relazionale
- Algebra relazionale $\rightarrow$ l'insieme delle operazioni per il modello relazionale che consentono all'utente di specificare le interrogazioni fondamentali
- Risultato $\rightarrow$ nuova relazione, che piu' essere formata a partire da una o piu' relazioni
- Espressione dell'algebra relazionale $\rightarrow$ sequenza di operazioni dell'algebra relazionale, ha come risultato un'altra relazione che rappresenta il risultato di un'interrogazione del database

### Operazioni relazionali unarie
- **Operazione di selezione (SELECT)**: 
	- Usata per selezionare un sottoinsieme di tuple di una relazione che soddisfano ima condizione di selezione
	- Notazione: $\sigma_{\text{<condizione di selezione>}}(R)$ 
	- Proprieta':
		- produce una relazione $S$ che ha lo stesso schema della relazione $R$
		- e' commutativa
		- SELECT in cascata possono essere eseguite in qualunque ordine (per la proprieta' commutativa)
		- SELECT in cascata possono essere sostituite da una singola SELECT avente come condizione la congiunzione di tutte le condizioni precedenti
- **Operazione di proiezione (PROJECT)**:
	- Seleziona alcuni attributi (colonne) da una relazione (tabella) e scarta gli altri (filtro verticale) eliminando eventuali duplicati delle tuple
	- Notazione: $\pi_{\text{<elenco attributi>}}(R)$
	- Proprieta':
		- il numero di tuple nel risultato e' sempre $\le$ al numero di tuple in $R$
		- se l'elenco degli attributi include una chiave $R$, allora il numero di tuple risultanti e' = al numero di tuple in $R$
		- se $\text{<attributi2> = <attributi1>} \rightarrow  \pi_{\text{<attributi1>}}(\pi_{\text{<attributi2>}}(R)) = \pi_{\text{<attributi1>}}(R)$ 
- **Operazione di ridenominazione (RENAME)**:
	- Da la possibilita' di applicare un'operazione alla volta e creare relazioni con risultati intermedi con nomi diversi
	- Notazione: $\rho_{\text{<nuovo nome>}}(R)$

### Operazioni insiemistiche
- **Operazione di unione (UNION)**:
	- Unisce due relazioni in una includendo tutte le tuple di una e dell'altra. Le due tuple devono avere lo stesso numero di tuple (compatibili all'unione)
	- Notazione: $R \cup S$
	- Compatibilita' all'unione:
		- $R$ ed $S$ devono avere lo stesso numero di attributi e il dominio di ogni attributo di $R$ deve essere lo stesso di quello di $S$
		- la relazione risultante ha gli stessi nomi di attributi di $R$
- **Operazione di intersezione (INTERSECTION)**:
	- Restituisce una relazione che include tutte le tuple che sono presenti in entrambe le relazioni
	- Notazione: $R \cap S$
	- Compatibilità dell'intersezione: stessa compatibilità dell'unione
- **Operazione di differenza (MINUS)**:
	- Restituisce una relazione che include tutte le tuple che sono presenti in $R$ ma non in $S$
	- Notazione: $R - S$
	- Compatibilità dell'intersezione: stessa compatibilità dell'unione

**Proprieta' di UNION, INTERSECTION e MINUS**
- UNION e INTERSECTION sono commutative
- UNION e INTERSECTION sono associative
- MINUS non e' commutativa 

**Operazione di prodotto cartesiano (prodotto incrociato)**
- Restituisce una relazione con tutti gli attributi di $R$ e $S$ e tutte le possibili combinazioni di tuple di $R$ e $S$ 
- Notazione: $|R \times S|$ ha $n \times m$ tuple (n = n. tuple di $R$, m = n. tuple di $S$)
- $R$ e $S$ non devono essere compatibili all'unione

### Operazioni relazionali binarie
- **Operazione di JOIN**:
	- Restituisce una relazione data da un prodotto cartesiano seguito da una selezione
	- Notazione $\bf{R} \bowtie_{\text{<condizione di join>}} \bf{S}$ 
- **THETA-JOIN**:
	- JOIN con condizione arbitraria 
	- Notazione: $\bf{R} \bowtie_{\text{<condizione> AND <condizione> AND ... AND <condizione>}} \bf{S}$ 
- **EQUI-JOIN**:
	- JOIN con unica operazione, quella di uguaglianza, e si ha come risultato una o più coppie di attributi con gli stessi valori in ogni tupla
	- JOIN piu' comune
- **JOIN naturale**:
	- JOIN (indicata con $*$) che non include i duplicati in una condizione di uguaglianza. 
	- Se due relazioni hanno attributi con lo stesso nome, la condizione di JOIN diventa una ridenominazione

**Operazione di divisione (DIVISION)**:
- Date due relazioni $R(Z) \div S(X)$ e $Y = Z-X$ ($Y$ e' l'insieme degli attributi di $R$ che sono in $S$), il risultato della divisione e' una relazione $T(Y)$ con le tuple $t$ di $R$ tali che, per ogni tupla in $S$, esiste almeno una tupla in $R$ che abbia gli stessi valori per gli attributi in $X$ e $Y$

### Altre operazioni relazionali

**Funzioni aggregate e raggruppamento**: 
- operazioni che permettono di calcolare valori aggregati (es. SUM, AVERAGE, MAXIMUM, MINIMUM, COUNT) su gruppi di tuple

**Uso dell'operatore funzionale** $\mathcal{F}$:
- Operatore di funzione aggregata dove risulta una selezione con gli attributi di raggruppamento piu' un attributo per ogni elemento della lista di funzioni
- Notazione: $\text{<attributi raggruppamento>} \quad \mathcal{F} \space \text{<lista funzioni>}(R)$ 
	- $\text{<attributi raggruppamento>}$: lista di attributi della relazione R
	- $\text{<lista funzioni>}$ lista di coppie (funzione, attributo)

**Operazioni di chiusura ricorsiva**:
- Operazioni che permettono di eseguire operazioni ricorsive su relazioni.
- Esempio: cercare tutti i supervisori di un dipendente e a tutti i livelli (supervisore, supervisore del supervisore, ecc.)

**Operazione di JOIN esterna (OUTER JOIN)**:
- Quando si vuole includere le tuple che non hanno corrispondenza in una delle due relazioni (a differenza della JOIN che include solo le tuple che hanno corrispondenza in entrambe le relazioni)
	- join esterna sinistra (**LEFT OUTER JOIN**): $\bf{R} \bowtie_\text{LEFT} \bf{S}$ mantiene tutte le tuple della prima relazione $R$ (se non c'è corrispondenza con la seconda relazione $S$, gli attributi di $S$ sono NULL)
	- join esterna destra (**RIGHT OUTER JOIN**): mantiene tutte le tuple della seconda relazione (di destra) $S$ nel risultato di $\bf{R} \bowtie_\text{RIGHT} \bf{S}$.
	- join esterna totale (**FULL OUTER JOIN**): mantiene tutte le tuple di entrambe le relazioni in $\bf{R} \bowtie_\text{FULL} \bf{S}$.

---
## 7 - Normalizzazioni
Obiettivo $\rightarrow$ valutare la qualita' della progettazione di schemi relazionale

Esamineremo la loro bonta':
- **A livello logico** (o concettuale) $\rightarrow$ come gli utenti interpretano gli schemi di relazione e il significato dei loro attributi
- **A livello di implementazione** (o di archiviazione fisica) $\rightarrow$ come le tuple in una relazione di base sono memorizzate e aggiornate

### Linee guida informali per la progettazione di schemi di relazione
1. **Assegnazione di una semantica esplicita agli attributi delle relazioni**
	- Il significato che viene dato agli attributi deve essere non ambiguo e semplice da spiegare
	- Non bisogna mischiare attributi provenienti da piu' entita' e associazioni

2. **Riduzione dei valori ridondanti nelle tuple**
	- Si vuole ridurre al minimo lo spazio di memoria occupato dalle relazioni
	- Anomalie di aggiornamento:
		- Anomalie di inserimento $\rightarrow$ non riesco ad inserire l'informazione perche' non e' completa
		- Anomalie di aggiornamento $\rightarrow$ devo aggiornare piu' tuple che contengono lo stesso valore
		- Anomalie di cancellazione $\rightarrow$ si eliminano informazioni importanti

3. **Riduzione del numero di valori nelle tuple**
	- I valori nulli si presentano per due problemi:
		- Spreco di memoria
		- Sono ambigui

4. **Impossibilita' di generare tuple spurie**
	- **Tuple spurie** $\rightarrow$ tuple che rappresentano informazioni non valide

### Dipendenze funzionali
- **Definizione**: dipendenza funzionale
	- Presa la relazione $R = \{A_1, \ldots, A_m\}$, $X, Y, \subseteq R$ sottoinsiemi di attributi di $R$, allora: $$ \forall t_1, t_2, \space \text{ SE } \space t_1[X] = t_2[X], \space \text{ ALLORA } \space t_1[Y] = t_2[Y] $$
	- Si dice che i valori della componente $Y$ di una tupla sono determinati dai valori della componente $X$ o che i valori della componente $X$ determinano funzionalmente i valori della componente $Y$.
- **Osservazioni**
	1. se $X$ e' un attributo chiave, allora $X \rightarrow Y$ e' valida per ogni insieme non vuoti di attributi $Y$ di $R$.
	2. se $X \rightarrow Y$ in $R$, non e' detto che $Y \rightarrow X$
- **Sintesi**:
	- Una dipendenza funzionale e' una proprieta' del DB che ci permette di descrivere i vincoli sugli attributi presenti in una relazione

**Regole di inferenza** (data una relazione $R = \{A_1, \ldots, A_m\}$ e dati $X, Y, \space e, \space Z \subseteq R$ come insiemi non vuoti di attributi)
1. **Riflessività** $\Rightarrow$ se $Y \subseteq X$, allora $X \rightarrow Y$
2. **Aumentazione** $\Rightarrow$ se $X \rightarrow Y$, allora $X \cup Z \rightarrow Y \cup Z$ 
3. **Transitività** $\Rightarrow$ se $X \rightarrow Y$ e $Y \rightarrow Z$, allora $X \rightarrow Z$
4. **Unione** $\Rightarrow$ se $X \rightarrow Y$ e $Y \rightarrow Z$, allora $Y \cup Z$
5. **Decomposizione** $\Rightarrow$ se $X \rightarrow Y \cup Z$, allora $X \rightarrow Y$ e $X \rightarrow Z$
6. **Pseudo-transitività** $\Rightarrow$ se $X \rightarrow Y$ e $W \cup Y \rightarrow Z$, allora $W \cup X \rightarrow Z$ 

### Normalizzazione
**Definizione**: 
- Processo di analisi degli schemi di relazione forniti, basato sulle loro dipendenze funzionali e chiavi primarie, per raggiungere le proprieta' di
	- minimizzazione della ridondanza
	- minimizzazione delle anomalie di inserimento, cancellazione e modifica

**Proprietà**:
- Proprieta' di JOIN non additiva $\rightarrow$ garantisce che non vengano generate tuple spurie dopo la decomposizione
- Proprieta' di preservazione delle dipendenze $\rightarrow$ le dipendenze funzionali vengono rispettate anche nelle relazioni scomposte

**Alcune definizioni**:
- Superchiave di uno schema relazionale $R = \{A_1, \ldots, A_m\} \rightarrow$ insieme non vuoto di attributi $S \subseteq R$ tale che non possono esistere due tuple $t_1$ e $t_2$ in $r(R)$ per cui $t_1[S] = t_2[S]$
- Superchiave minimale $\rightarrow$ minimo numero di attributi necessario affinché ogni tupla sia distinta
- Attributo primo di $R$ $\rightarrow$ attributo membro di una qualche chiave candidata di $R$

**Forma normale (1NF)**:
- Domini degli attributi devono avere solo valori atomici
	- aumento della ridondanza
	- si crea una nuova relazione per ogni attributo multivalore
	- si scompone gli attributi composti in attributi semplici
- Deve esistere una chiave primaria

**Seconda forma normale (2NF)**:
- Deve essere 1NF
- Ogni attributo non primo $A$ di $R$ dipende da una dipendenza funzionale piena della chiave primaria di $R$

**Dipendenza funzionale piena**:
- Una dipendenza funzionale $X \rightarrow Y$ e' una dipendenza funzionale piena se la rimozione di un qualunque attributo da $X$ significa che la dipendenza non e' piu' valida 

**Terza forma normale (3NF)**:
- Deve essere 2NF
- Nessun attributo non primo di $R$ dipende transitivamente dalla chiave primaria

**Proprieta' della dipendenza transitiva**:
Una dipendenza funzionale $X \rightarrow Y$ di uno schema relazionale $R$ e' una dipendenza transitiva se:
	1. esiste un insieme di attributi $Y$ in $R$ che non e' ne una chiave candidata ne un sottoinsieme di una chiave di $R$
	2. vale: $X \rightarrow Y$ e $Y \rightarrow Z$

**Forma normale di Boyce-Codd (BCNF)**:
- se per ogni dipendenza funzionale non triviale $X \rightarrow A$ in $R$, vale che $X$ e' una superchiave di $R$

**IMPORTANTE**
- OGNI 2NF E' IN 1NF
- OGNI 3NF E' IN 2NF
- OGNI BCNF E' IN 3NF
	- essendo la BCNF piu' restrittiva della 3NF, no e' vero che ogni 3NF e' in BCNF