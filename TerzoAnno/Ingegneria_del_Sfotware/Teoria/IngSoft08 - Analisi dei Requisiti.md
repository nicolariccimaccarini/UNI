## Definizione e specifica
- Tecniche per analizzare, definire e specificare i requisiti dei sistemi software
- Lo scopo e' capire meglio il problema che si sta cercando di risolvere: capirne l'impatto, cio' che vogliono i clienti, come lo useranno gli utenti finali
- Varie fasi: avvio, raccolta, elaborazione. In piu' la negoziazione: priorita', cosa e' fondamentale, tempi...
- Il prodotto finale e' una documentazione scritta del problema

## L'analisi dei requisiti
- **Coinvolge sicuramente** lo staff tecnico: collaborando con il committente per individuare il dominio applicativo, i servizi che il sistema deve offrire e i vincoli operativi
- Sono **quasi certamente coinvolti** anche utenti finali, manager, ingegneri coinvolti nella manutenzione, esperti del dominio, ecc.
	- queste sono gli stakeholders (controparti)

### Problemi classici dell'analisi dei requisiti
- Gli stakeholders non hanno le idee chiare sul cosa vogliono
- Gli stakeholders esprimono i requisiti usando una loro terminologia (di solito la terminologia e' legata al dominio applicativo)
- Tra gli stakeholders ci possono essere requisiti contrastanti
- Possono esserci altri fattori (politica interna, gestione) che influenzano i requisiti del sistema
	- ricordarsi che i requisiti possono cambiare e che possono anche saltar fuori nuovi stakeholders

## Requisiti vs progettazione e costruzione
- Progettare e costruire sw puo' essere difficile, richiede anche creativita' e puo' essere divertente
- E' coinvolgente: spesso si inizia a realizzare sw prima di avere un'idea chiara. Il presupposto e' che le cose diventeranno piu' chiare andando avanti. Quello che conta e' produrre sw funzionante  e tutto il resto e' secondario.
- Ing. requisiti deve essere adatta ai bisogni del processo, del progetto, del prodotto e delle persone coinvolte
- In alcuni casi si puo' abbreviare, in altri conviene sia rigorosa: il team di sviluppo deve adattare il proprio approccio all'ing. requisiti
	- il team deve realmente comprendere i requisiti di un problema prima di tentare di risolverlo

## Attivita'
- Comprensione del dominio
	- gli analisti devono "padroneggiare" il dominio applicativo
- Raccolta dei requisiti
	- interagire con gli stakeholders per identificare i loro bisogni (e quindi i requisiti)
- Classificazione
	- prende l'insieme non strutturato dei requisiti e organizzarli in modo coerente
- Risoluzione dei conflitti
	- a causa del numero di stakeholders coinvolti, possono esserci dei conflitti nei requisiti
- Assegnazione delle priorita'
	- non tutti i requisiti sono uguali $\rightarrow$ alcuni sono piu' importanti di altri; bisogna stabilire delle classi di priorita'
- Validazione dei requisiti
	- i requisiti vanno ricontrollati per vedere se sono completi, consistenti e in accordo con quello che gli stakeholders vogliono realmente dal sistema

## Avvio
- La componente business degli stakeholders definiscono:
	- un business per creare l'idea
	- tentano di identificare l'ampiezza e profondita' del mercato
	- primo studio approssimativo di fattibilita'
	- identificano una descrizione dello scope del progetto

### Avvio - stakeholders
- Stakeholder $\rightarrow$ chiunque tragga benefici diretti o indiretti dal sistema che viene sviluppato
	- manager che si occupano delle attivita' commerciali
	- manager che si occupano del prodotto
	- personale marketing
	- clienti interni/esterni
	- utenti finali
	- consulenti
	- ingegneri sw
	- addetti supporto e manutenzione
	- ...
- Ognuno di loro ha un punto di vista differente, avra' benefici differenti dal sistema, avra' rischi differenti se il progetto non va in porto.
- I responsabili della raccolta dei requisiti devono farsi un elenco delle persone a cui sara' richiesto di collaborare alla definizione dei requisiti
	- l'elenco cresce mano a mano che si parlera' con loro: ognuno dovra' rispondere alla domanda "con chi altro sarebbe utile parlare?"

### Avvio - punti di vista
- I requisiti vengono necessariamente esplorati da piu' punti di vista
	- marketing e' interessato a funzionalita' e caratteristiche che potranno interessare il mercato potenziale
	- manager e' interessato a rientrare nel budget e a rispondere a specifiche finestre di mercato
	- utente finale e' interessato a funzionalita' che ritiene familiari o facili da apprendere/utilizzare
- Ogni stakeholder contribuira' con il proprio punto di vista
- Potranno esserci requisiti incoerenti o in conflitto tra loro
- **Lo scopo di questa fase e' catalogare le informazioni raccolte, non decidere**

### Avvio - collaborazione
- Come cercare di ottenere collaborazione da tutti?
- Cominciamo con identificare aree comuni e aree di conflitto o contenenti incoerenze
- Per queste ultime un modo di procedere consiste nei "punti di priorita'"
	- ogni stakeholder ha un certo numero di punti di priorita' che puo' spendere nei vari requisiti
	- i punti spesi non possono essere riutilizzati
	- in questo modo si ottiene una indicazione dell'importanza globale di ciascun requisito secondo i vari punti di vista

### Avvio - prime domande
- Lo scopo e' definire una solida conoscenza del problema, di chi vuole una soluzione, della natura della soluzione desiderata e delle comunicazione/collaborazioni tra cliente e sviluppatore
- Dovrebbero essere domande aperte, non devono influenzare la risposta

## Raccolta
- Sembra semplice $\rightarrow$ basta chiedere al cliente, utenti, ... quali sono gli obiettivi del prodotto, cosa si vuole ottenere, in quale modo il prodotto risponde ai bisogni stabiliti e come verra' utilizzato
	- problemi di scope $\rightarrow$ limiti del sistema mal definiti o cliente specifica dettagli tecnici non necessari che confondono invece di chiarire gli obiettivi generali del sistema
	- problemi di comprensione $\rightarrow$ cliente non ben sicuro di cosa serve, ha idea vaga delle limitazioni, non perfetta conoscenza del dominio del problema, problemi a comunicare i bisogni, omette cose "ovvie", requisiti ambigui o non verificabili, ...
	- problema di volubilità $\rightarrow$ i requisiti cambiano del tempo

### Raccolta collaborativa
- L'approccio piu' usato e' Facilitated Application Specification Technique (FAST)
- Durante l'avvio si e' definito lo scope del problema e la percezione globale di una soluzione, e gli stakeholders hanno preparato la "richiesta del progetto"
- Prima delle riunioni, i partecipanti devono preparare elenco di oggetti che fanno parte del sistema, elenco di oggetti che devono essere prodotti dal sistema, elenco di oggetti che vengono utilizzati dal sistema per le proprie funzioni; in piu' elenco dei servizi che manipolano o interagiscono con gli oggetti e dei vincoli
	- elenco non esaustivo, ma deve riflettere la sua percezione del sistema
- A questo punto si presentano i singoli elenchi: ogni elemento dovrebbe poter essere manipolato separatamente. No dibattiti e critiche in questa fase.
- Creare un elenco combinato: si eliminano i doppioni
- L'obiettivo e' arrivare ad un elenco consensuale per ciascun argomenti: oggetti, servizi, vincoli, prestazioni
- A questo punto si puo' procedere sviluppando delle "mini-specifiche", oppure sviluppando dei casi d'uso dell'utente
- In generale ogni utente crea anche un elenco di criteri di validazione. Anche per i criteri di validazione serve creare un elenco consensuale.

## Definizione e specifica dei requisiti
- Definizione dei requisiti
	- descrizione delle funzionalita' del sistema e dei vincoli operativi, orientata al cliente
- Specifica dei requisiti
	- descrizione precisa e dettagliata delle funzionalita' e dei vincoli del sistema. Serve per comunicare agli sviluppatori cosa il sistema deve fare
	- la specifica viene utilizzata come base per il contratto di sviluppo del sistema

### Definizione dei requisiti
- Deve specificare il comportamento che il sistema manifesta verso l'esterno senza definire un modello computazionale
- Include requisiti funzionali e non funzionali
	- funzionali $\rightarrow$ descrivono le funzionalita' che il sistema deve fornire
	- non funzionali $\rightarrow$ vincoli sui servizi e funzionalita' fornite, o sul processo di sviluppo o sull'ambiente esterno; o proprieta' che il sistema non deve avere

#### Requisiti non funzionali
- Proprieta' di comportamento del sistema: affidabilita', tempi di risposta, vincoli sull'I/O, ...
- Possono essere piu' critici dei requisiti funzionali, perche' possono rendere inutile il sistema se non vengono soddisfatti
- Tipologia dei requisiti non funzionali
	- requisiti di prodotto
	- requisiti di processo
	- requisiti esterni

##### Classificazione dei requisiti non funzionali 
- **Requisiti di prodotto**
	- specificano che il prodotto deve comportarsi in un certo modo
- **Requisiti di processo**
	- sono conseguenza di scelte di tipo organizzativo
- **Requisiti esterni**
	- derivano da fattori esterni al sistema e al suo processo di sviluppo

#### Come si scrive la definizione dei requisiti
- Normalmente in linguaggio naturale usando anche tabelle o diagrammi se necessario
- Questo modo e' universalmente comprensibile ma puo' creare tre tipi di problemi
	- **mancanza di chiarezza** $\rightarrow$ e' difficile scrivere con precisione senza rendere un documento illeggibile
	- **confusione dei requisiti** $\rightarrow$ requisiti funzionali e non funzionali tendono ad essere mescolati tra loro
	- **amalgama dei requisiti** $\rightarrow$ requisiti diversi potrebbero essere descritti insieme

#### Regole di stesura
- Aderite ad un formato standard: che permette di evitare omissioni e semplifica i controlli incrociati
- Raggruppate i requisiti che sono legati tra loro
- Evidenziate i requisiti principali
- Associate motivazioni ai requisiti

#### Motivazione del requisito
- La motivazione del requisito serve per aiutare lo sviluppatore a capire il dominio di applicazione e a capire perche' il requisito e' descritto in quella forma
- E' particolarmente importante qualora si debbano modificare i requisiti: la presenza della motivazione di un requisito riduce la probabilita' che il cambiamento alteri l'interazione originaria e che si producano effetti inaspettati

#### Elaborazione
- Si prendono le informazioni ottenute e le si espandono e raffinano
- Occorre descrivere il problema in modo da creare solide basi per la progettazione. Se si oltrepassa questo punto, vuol dire che siamo gia' partiti a progettare il sistema

#### Negoziazione
- Vari motivi
	- requisiti di conflitto
	- come far combaciare le richieste con le risorse disponibili
- Puo' essere necessario valutare compromessi tra funzionalita', prestazioni e altre caratteristiche del prodotto da un lato e costi o tempi di uscita sul mercato dall'altro
- Lo scopo di questa fase e' di sviluppare un piano di progetto che risponda ai bisogni del cliente riflettendo i vincoli del mondo reale
- Il trucco? Cercare di ottenere vantaggi per tutte le parti
- Libri sull'abilita' di negoziazione si contano a decine, e non si applicano solo al campo dell'ingegneria del sw
- In generale
	1. Riconoscere che non e' una gara $\rightarrow$ entrambe le parti devono ritenere di aver vinto, ma entrambe devono scendere a compromessi
	2. Stabilire una strategia $\rightarrow$ decidere cosa si vuole ottenere, cosa vuole ottenere l'altra parte e come si puo' dare in modo che non accada
	3. Ascoltare attentamente $\rightarrow$ non formulare una risposta mentre l'altra parte sta ancora parlando
	4. Concentrarsi sugli interessi dell'altra parte $\rightarrow$ una posizione rigida aumenta i conflitti, non li risolve
	5. Evita di scendere sul piano personale 
	6. Essere creativi
	7. Essere pronti a impegnarsi

#### Specifica dei requisiti
- Con la specifica dei requisiti aggiungiamo dettagli alla definizione
- Di solito la specifica e' scritta in linguaggio naturale, ma...
	- il linguaggio naturale assume che chi scrive e chi legge usino le stesse parole per esprimere gli stessi concetti
	- una specifica in linguaggio naturale puo' ammettere varie interpretazioni
	- e' difficile trovare requisiti collegati tra loro, o il linguaggio naturale non puo' essere sufficiente a partizionare i requisiti

##### Alternative al linguaggio naturale
- Linguaggio naturale "strutturato"
	- definire forme standard o template per esprimere le specifiche
- Linguaggi di Descrizione di Programmi (PDL)
	- una specie di linguaggi di programmazione: definiscono i requisiti fornendo una visione operazionale del sistema
- Requirements Specification Languages
- Notazione grafiche (SADT)
- Specifiche formali
	- reti di petri
	- sistema Z
	- sistema B
	- logiche temporali


## Gestione
- I requisiti possono cambiare, anzi la necessita' di cambiare i requisiti puo' essere richiesta per tutta la durata del sistema
- Bisogna poter identificare, controllare e tracciare i requisiti e i cambiamenti a mano a mano che si procede nello sviluppo del progetto
- Problema molto simile alla gestione della configurazione del software: quindi soluzione simile
	- assegna a ogni requisito un identificatore
	- sviluppa delle tabelle di tracciabilita'
		- tracciabilita' delle funzionalita' $\rightarrow$ relazioni tra requisito e funzionalità
		- tracciabilita' dell'origine $\rightarrow$ origine di ogni requisito
		- tracciabilita' e dipendenze $\rightarrow$ relazioni tra requisiti

### Gestione - Tracciabilita' dei requisiti
- I requisiti in qualche modo associati devono essere collegabili in qualche modo
- La tracciabilita' e' una proprieta' della specifica dei requisiti che si riflette nella facilita' di trovare requisiti collegati
- Di solito la tracciabilita' si ottiene numerando i requisiti e utilizzando questi numeri per inserire riferimenti incrociati o creare indici

## Validazione
- Esaminare le specifiche per garantire che tutti i requisiti sw siano stati descritti in modo non ambiguo
- Che siano state rilevate le incoerenze, omissioni, errori, ...
- Come? Revisione tecnica formale

### Validazione - Verifica dei requisiti
- I requisiti devono essere scritti in modo da poter essere facilmente verificati
- Evitate termini vaghi
- dovete quantificare

## Analisi viewpoint-oriented
- Guardare al problema con gli occhi dei vari "Attori", di tutti quelli che sono in qualche modo coinvolti nel sistema
- View-points $\rightarrow$ produttori/consumatori di dati, utenti/realizzatori di servizi, interni/esterni al sistema, ...
- Perche' e' importante avere piu' prospettive?
	- raramente c'e' un unico modo "corretto" di analizzare i requisiti di un sistema
	- quindi non c'e' una singola vista corretta del sistema, ma tanti modi ugualmente corretti che lo rappresentano

### Modello di processo VORD (Viewpoint Oriented Requirements Definition)
- Identificazione dei viewpoints
	- individuare dei viewpoint che ricevono servizi dal sistema e identificare i servizi specifici che il sistema deve offrire a ogni viewpoint
- Strutturazione dei viewpoint
	- combinare i viewpoint a gruppi, in modo gerarchico; servizi comuni sono forniti al piu' alto livello di gerarchia
- Documentazione dei viewpoints
	- raffinare la descrizione dei viewpoint e dei servizi identificati
- Mapping viewpoint-sistema
	- definire gli oggetti che rappresentano i viewpoint (modello OO)

## Specifiche date in linguaggio naturale strutturato
- Forma limitata di linguaggio naturale $\rightarrow$ impone un grado di uniformita' alla specifica
- Metodo:
	- descrizione della funzione o entita' da specificare
	- descrizione degli input e da dove provengono
	- descrizione degli output e dove vanno a finire
	- indicazione di altre entita' richieste
	- pre e post condizioni
	- effetti collaterali (se ce ne sono)
- N.B. $\rightarrow$ e' come la specifica di uno use case

## Definizione dei requisiti con PDL
- I requisiti possono essere definiti in modo operazionale usando un linguaggio simile ad un linguaggio di programmazione ma piu' aggressivo
- Appropriato in due casi
	- quando un'operazione e' specificata come sequenza di azioni e l'ordine e' importante
	- quando si devono specificare le interfaccia hw e sw
- Svantaggi
	- puo' non essere abbastanza espressivo da definire i concetti del dominio in modo adeguato
	- si puo' fare generare confusione tra specifica e progettazione

