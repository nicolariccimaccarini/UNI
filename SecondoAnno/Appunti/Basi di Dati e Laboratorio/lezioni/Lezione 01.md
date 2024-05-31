
## Definizioni 
- **Database**: Un insieme di dati correlati
- **Dati**: Fatti che non possono essere memorizzati e che hanno un significato intrinseco
- **Mini-Mondo**: Un certo aspetto del mondo reale (i dati del quale vengono archiviati in un database). Per esempio, i voti degli studenti agli esami, i libri di una biblioteca, ...
- **Database Management System**: Programma (o insieme di programmi) che permette la creazione e la manutenzione di un database elettronico.
- **Database system**: il DBMS ed i dati contenuti nel database. A volte anche le applicazioni (esterne al DBMS) per interagire con i dati.

## Tipi di Database ed Applicazioni
- Database Numerici e Testuali
- Database Multimediali
- Geographic Information System (GIS)
- Data Warehouses
- Real-time and Active Databases

## Funzioni di un DBMS
- **Definire** un database: in termini di tipi di dati, strutture e vincoli
- **Costruire** un database: immagazzinare i dati in un mezzo di memorizzazione
- **Manipolare** un database: eseguire interrogazioni per recuperare dati, aggiornare, modificare e cancellare i dati, generare report, ...
- **Condividere** un database: consentire a piu' utenti o applicazioni di accedere contemporaneamente ai dati (mantenendo i dati validi e consistenti)

Altre caratteristiche:
- Protezione o misure di sicurezza per prevenire accessi non autorizzati ai dati
- Procedure "attive" per eseguire operazioni sui dati internamente
- Sistemi di visualizzazione e presentazione dei dati

## Caratteristiche dell'approccio con database
- ### Natura autodescrittiva di un database system:
  Esiste un catalogo in cui il DBMS memorizza una descrizione del database. Questa descrizione viene chiamata meta-data. Ciò consente al DBMS di lavorare con diversi database. Differenza enorme rispetto all’utilizzo di file.
  
- ### Isolamento tra programmi e dati:
  Caratteristica chiamata anche **indipendenza tra programmi e dati**. Consente di cambiare le strutture o le operazioni di memorizzazione dei dati senza dover modificare i programmi di accesso del DBMS.
  
- ### Astrazione dei dati:
  Viene utilizzato un **modello dei dati** per nascondere all'utente i dettagli sulla memorizzazione e per presentare una rappresentazione concettuale del database.
  
- ###  Supporto di viste multiple dei dati:
  Ciascun utente puo' vedere il database da prospettive diverse (**viste**). Ciascuna vista descrive solamente i dati interessati dall'utente.
  
- ### Condivisione dei dati e gestione delle transazioni con utenti multipli:
  Permette ad un insieme di utenti di operare in modo **concorrente** sul database. Il controllo della concorrenza del DBMS garantisce che ciascuna **transazione** sia eseguita correttamente oppure annullata. L'OLTP (Online Transaction Processing) e' un aspetto di grande rilievo per le applicazioni con database!.

## Utenti del database
Gli utenti possono essere suddivisi tra quelli che effettivamente utilizzano e controllano il contenuto del database (Attori sulla scena) e quelli che lavorano per la manutenzione del DBMS ma che non sono direttamente interessati al database in se' (Lavoratori dietro le quinte).

- ### Attori sulla scena:
	- **Amministratori**: Responsabili di autorizzare gli accessi al database, di coordinare e monitorarne l'uso, di acquistare (scegliere) il software e l'hardware necessario, di controllarne l'utilizzo e monitorare l'efficienza delle operazioni.
	- **Progettisti**: Responsabili di definire il contenuto, la struttura, i vincoli e le funzioni o le transazioni del database. Devono comunicare con gli utenti finali e capire le loro necessita'.
	- **Utenti finali**: Coloro che utilizzano effettivamente il database. Esistono diverse tipologie
		- *Utenti occasionali*: Accedono occasionalmente al database. Esperti
		- *Utenti parametrici*: La maggior parte degli utenti finali. Usano funzioni o transazioni predefinite per accedere al database in maniera ripetuta ed effettuare aggiornamenti, inserimenti, interrogazioni.
		- *Utenti esperti*: Scienziati, ingegneri, analisti che usano direttamente le potenzialita' del DBMS per soddisfare le proprie esigenze.
		- *Utenti indipendenti*

- ### Lavoratori dietro le quinte:
	- **Progettisti ed implementatori di DBMS**: Coloro che lavorano sui programmi che costituiscono il DBMS e ne forniscono funzionalita', interfacce, accesso ai dati, ...
	- **Sviluppatori di strumenti**: Coloro che forniscono gli strumenti necessari a facilitare la progettazione, il controllo, la comunicazione e l'ottimizzazione delle prestazioni del database.
	- **Operatori per la manutenzione**: Coloro che si occupano di mantenere efficiente hardware e software per il DBMS.

## Vantaggi dei database
- Controllo della ridondanza nella memorizzazione dei dati e nel lavoro di sviluppo e intrattenimento.
- Condivisione dei dati tra piu' utenti.
- Controllo degli accessi ai dati.
- Strutture di memorizzazione tali da garantire un efficiente interrogazione dei dati.
  
- Servizi di Backup e Recovery.
- Interfacce multiple a diverse classi di utenti
- Rappresentazione di relazioni complesse tra i dati
- Garanzia sui vincoli di integrita'
	- Integrita' referenziale
	- Univocita'
	- Regole di Business

## Storia 
- **Prime applicazioni di database**: Modelli gerarchico e reticolare, introdotti a meta' degli anni 60 ed utilizzati per tutti gli anni settanta (ancora oggi)
- **Sistemi basati sul modello relazionale**: Il modello relazionale e' stato introdotto nel 1970. Il piu' usato al giorno d'oggi. Ricerche e esperimenti iniziati in IBM ed universita'. Si e' affermato negli anni '80.
- **Applicazioni orientate agli oggetti**: Gli OODBMS sono stati introdotti alla fine degli anni 80, inizio anni 90 per affrontare le necessita' di data processing complesso in sistemi CAD (ed altro) mediante l'uso di linguaggi e metodologie ad oggetti. L'uso non si e' ancora affermato.
- **Scambio di dati su Web**:Applicazioni specifiche per visualizzare sul Web (in pagine html) dati estratti da un database. Numerose tipologie. Uso per il commercio elettronico ma anche per applicazioni semplici (weblog, ...). Evoluzione rapida; XML.




