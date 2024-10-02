**Definizione**: insieme di sistemi distinti per localita' che comunicano e cooperano attraverso lo scambio di messaggi, per ottenere risultati coordinati.

Conseguenze:
- **Concorrenza** $\rightarrow$ in qualunque momento moltissime attivita' (processi) possono essere in esecuzione (anche in diverse localita')
- **Nessun tempo globale** $\rightarrow$ non e' possibile sincronizzare degli orologi di una moltitudine di macchine in rete
- **Fallimenti indipendenti** $\rightarrow$ molte cause di fallimento, oltre a crash di macchine possibili anche problemi di rete. Una macchina isolata dalla rete potrebbe peraltro continuare la computazione ...

### Esempi
- Internet 
- Web
- Sistemi intranet

### Motivazioni
- Condivisione delle risorse
- Affidabilita' per tollerare guasti (replicazione)
- Applicazioni intrinsecamente distribuite
- Facile accesso al sistema
- Scalabilita'

## Aspetti dello scenario
- **Eterogeneità** $\rightarrow$ Grande varieta' di hardware, reti, protocolli, sistemi operativi, linguaggi di programmazione
- **Apertura** $\rightarrow$ Un sistema aperto pubblica e documenta le proprie interfacce, facilita estensibilita' e cooperazione con altri componenti. (Verso Open Source)
- **Sicurezza** $\rightarrow$ Autenticazione, autorizzazione, integrita', privacy, disponibilita', paternita'. Problemi complicati dalla condivisione del supporto di comunicazione
- **Mobilita'** $\rightarrow$ La mobilita', in tutte le sue forme e accezioni, e' ormai una caratteristica irrinunciabile. Mobilita utenti, terminali, applicazioni
- **Scalabilità** $\rightarrow$ Un sistema e' scalabile se rimane efficace ed efficiente al crescere delle risorse gestite e degli utenti collegati (costi, algoritmi decentralizzati, compatibilita')

![[scalabilita.png]]

## Principali problemi
- **Gestione guasti** $\rightarrow$ Il problema principale e' che i guasti possono essere parziali e distribuiti (rilevamento guasti, nascondere guasti, progetto tollerante ai guasti, recupero dati dai guasti, ridondanza)
- **Concorrenza** $\rightarrow$ Molte applicazioni e servizi devono essere concorrenti per rispondere a richieste di molti client contemporaneamente
- **Trasparenza** $\rightarrow$ nascondere la reale distribuzione a utenti e programmatori (per semplicita' di programmazione). Varie forme di trasparenza:
	- *accesso* (uso i stesse operazioni per accedere risorse locali o remote)
	- *locazione* (risorse accedute senza bisogno di sapere dove sono)
	- *concorrenza* (molti processi eseguono contemporaneamente senza interferenze)
	- *replicazione* (risorse replicate senza conoscenza da parte utente)
	- *guasti* (si nascondono alcuni guasti)
	- *mobilita'* (spostamento risorse o utenti senza alterare la funzione del servizio)
	- *performance* (potenziamento del sistema per migliori prestazioni)
- **Sicurezza** $\rightarrow$ Le vulnerabilita' sono molte di piu' rispetto a un sistema isolato stand-alone

## Principali Trend nello Sviluppo nelle Reti di Calcolatori e dei Servizi Internet
- Cloud Computing
	- Migration to the Cloud
	- Cloud Federation
	- Edge + Fog + Cloud Computing $\rightarrow$ Compute Continuum
- Containerization and Microservices
- Smart Cities
- Industry 5.0
- AI approaches for optimization of communications 

### Cloud Computing
- "Cloud" e' una metafora della rete internet
- Con "Cloud computing" si indica la possibilita' di utilizzare applicazioni e dati che non risiedono nella macchina locale, ma che arrivano attraverso la rete internet
- Applicazioni e dati si trovano in server remoti, acceduti via rete
- Diversi paradigmi:
	- IaaS $\rightarrow$ Infrastructure as a Service
	- PaaS $\rightarrow$ Platform as a Service
	- Saas $\rightarrow$ Software as a Service
	- Faas $\rightarrow$ Function as a Service

Principali fornitori:
- Amazon con Amazon Web Service (AWS) $\rightarrow$ primo ad usare cloud computing
- Google con Google App Engine e tutta la suite Google Apps
- Microsoft con la proposta Windows Azure e Office 365
- Salesforce con prodotti CRM (Custom Relationship Management)

#### Evoluzioni del Cloud Computing
- Il Cloud Computing sta evolvendo anche i modelli lievemente diversi come Edge Computing. In questo modello si sposta la capacita' computazionale piu' vicino agli utilizzatori (per ridurre la latenza, banda, etc., migliorando cosi' velocita' e performance)
- **Edge Computing** $\rightarrow$ trasferimento dell'analisi dei dati il piu' vicino possibile alla sorgente, senza necessita' di trasferirli in Cloud per il processamento
- **Fog Computing** $\rightarrow$ estensione del modello Edge Computing che considera anche allocazione e gestione integrale dei servizi di computazione, stoccaggio, e disseminazione dei dati tra i dispositivi Edge e Cloud.

## Le Reti di Calcolatori e Sistemi Distribuiti
I termini "reti di calcolatori" e "sistemi distribuiti" indicano aree di studio e problematiche diverse:
 - Una **rete di calcolatori** e' un insieme di computer autonomi e indipendenti che possono comunicare attraverso una rete di interconnessione.
 - I **sistemi distribuiti** sono la naturale evoluzione delle reti di calcolatori, dove il principale beneficio e' rappresentato dalla **trasparenza**. Tutte le operazioni svolte dagli utenti sembrano essere locali anche quando vengono svolte in modo remoto.