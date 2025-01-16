## In breve
- Cascata: DPIII
	- Definizione dei requisiti $\rightarrow$ Progettazione del sistema e software $\rightarrow$ Implementazione e testing delle singole unita' $\rightarrow$ Integrazione e testing di sottosistema $\rightarrow$ Installazione e manutenzione
- RAD: CP(MC)D
	- Comunicazione $\rightarrow$ Progettazione $\rightarrow$ Modellazione $\rightarrow$ Costruzione $\rightarrow$ Deployment 
- Evolutivo: D-S-S-V
	- Definizione massina $\rightarrow$ Specifica $\rightarrow$ Sviluppo $\rightarrow$ Validazione
- Formale: DSTI
	- Definizione dei requisiti $\rightarrow$ specifica formale $\rightarrow$ trasformazione formale $\rightarrow$ integrazione e test
- Riuso: AAPI
	- Analisi dei componenti $\rightarrow$ Adattamento dei requisiti $\rightarrow$ Progettazione del sistema $\rightarrow$ Integrazione
- Spirale: CPASCrV
	- Comunicazione con il cliente $\rightarrow$ Pianificazione $\rightarrow$ Analisi dei rischi $\rightarrow$Strutturazione $\rightarrow$ Costruzione e release $\rightarrow$ Valutazione del cliente
- RUP: IECT
	- Inception $\rightarrow$ Elaboration $\rightarrow$ Construction $\rightarrow$ Transition
- Agile: PI $\rightarrow$ PS, SF $\rightarrow$ D, C, C-RISP $\rightarrow$ A
	- Persone e interazione $\rightarrow$ processi e strumenti, Software funzionante $\rightarrow$ Documentazione, Collaborazione al di la del contratto, Capacita' di risposta ai cambiamenti $\rightarrow$ Aderire al progetto

## Domande note

==**Descrivere il modello di sviluppo a cascata illustrandone la struttura, descrivendo sinteticamente le attività associate alle varie fasi e inoltre indicando quali sono i principali problemi legati al modello e le sue caratteristiche di visibilità**==
Il modello a cascata si compone delle seguenti 5 fasi:
1. Definizione dei requisiti $\rightarrow$ si lavora alla specifica dei requisiti dopo aver avuto un dialogo con il committente
2. Progettazione del sistema $\rightarrow$ si effettua la progettazione dell'architettura del sistema e delle sue componenti
3. Implementazione e testing delle singole componenti
4. Integrazione e  testing delle singole componenti
5. Installazione e manutenzione $\rightarrow$ si installa il sistema e se ne si prende cura
E' possibile passare da una certa fase a quella precedente, tuttavia il cambiamento dei requisiti in corso d'opera e' estremamente costoso.
Dal punto di vista della visibilita', il modello di sviluppo a cascata e' buono poiche' produce deliverables ad ogni fase.
Il rischio di questo modello di sviluppo dipende dalla familiarita' del team nei confronti del problema da affrontare.

==**Descrivere il modello di sviluppo a spirale illustrandone la struttura, descrivendo sinteticamente le attività associate alle varie fasi e inoltre indicando quali sono i principali problemi legati al modello e le sue caratteristiche di visibilità.**==
Il modello di sviluppo a spirale prevede i seguenti step, che si reiterano numerose volte fino alla release del software:
1. Comunicazione con il cliente
2. Progettazione del sistema
3. Analisi dei rischi
4. Strutturazione
5. Costruzione e release 
6. Valutazione del cliente
Questo modello e' indicato per progetti di grandi dimensioni. La visibilita' e' buona perche' ogni spicchio della spirale produce documentazione.
I rischi sono minimizzati dalle numerose iterazioni, pero' la erronea valutazione dei rischi puo' ripercuotersi successivamente.

**==Descrivere il modello di sviluppo evolutivo illustrandone la struttura, descrivendo sinteticamente le attività associate alle varie fasi e inoltre indicando quali sono i principali problemi legati al modello e le sue caratteristiche di visibilità.==**
Lo sviluppo evolutivo da importanza alla produzione dei prototipi usa-e-getta a partire da definizioni dei requisiti di massima, ossia si da molta importanza alla scrittura del codice, il quale evolve secondo le direttive del cliente. Le fasi sono:
1. Definizione di massima
2. Specifica, che produce la versione iniziale
3. Sviluppo, che produce la versione finale
4. Validazione, che produce la versione finale
La visibilita' e' scarsa a causa della produzione di documenti carente, il che' puo' elevare il livello di rischio (che pero' allo stesso tempo e' ridotto grazie allo sviluppo di numerosi prototipi). E' fondamentale che il prodotto finale non sia un prototipo: una volta raggiunti gli obiettivi di sviluppo e' opportuno riscrivere il software per la release. Questo modello si presta particolarmente per progetti di piccola dimensione op con una vita attesa corta.

**==Descrivere il modello di sviluppo trasformazionale (o formale) illustrandone la struttura, descrivendo sinteticamente le attività associate alle varie fasi e inoltre indicando quali sono i principali problemi legati al modello e le sue caratteristiche di visibilità.==**
Il modello di sviluppo formale e' adeguato nel momento in cui si prefigge, ad esempio, di codificare linguaggio matematico.
1. Definizione dei requisiti
2. Specifica formale
3. Trasformazione formale
4. Integrazione e test
Il rischio e' legato all'esperienza degli sviluppatori e della necessita' di tecnologie avanzate. La visibilita' e' buona, ogni fase dovrebbe produrre documentazione affinché il processo di sviluppo possa svolgersi.

**==Descrivere il modello di sviluppo basato sul riutilizzo illustrandone la struttura, descrivendo sinteticamente le attività associate alle varie fasi e inoltre indicando quali sono i principali problemi legati al modello e le sue caratteristiche di==**
Il modello di sviluppo basato su riutilizzo si basa sull'uso di componenti off-the-shelf gia' scritte e testate per la produzione software.
Le fasi di questo modello sono:
1. Analisi dei comportamenti
2. Adattamento dei requisiti
3. Progettazione del sistema
4. Integrazione
La visibilita' e' buona, pero' puo' essere macchinoso scrivere documentazione per componenti off-the-shelf. Il rischio e' ridotto, dato che si presuppone la correttezza dei componenti riutilizzati. Questo modello si presta molto allo sviluppo di software ad oggetti.

==**Descrivere il modello di sviluppo RAD (Rapid Application Development) illustrandone la struttura e inoltre indicando vantaggi e svantaggi.**==
Il modello RAD e' particolarmente indicato nei casi in cui si deve sviluppare un progetto facilmente partizionabile in cui non vi sia la necessita' di utilizzare interfacce appositamente definite. Inoltre non e' il modello indicato qualora si utilizzassero tecnologie innovative con alto rischio di incontrare problemi in corso d'opera.
Le fasi che compongono questo modello sono:
1. Comunicazione
2. Pianificazione
3. Modellazione
4. Costruzione 
5. Deployment
In particolare, la modellazione e la costruzione sono ad opera di ciascun team incaricato di lavorare su una partizione del progetto.

==**RUP**==
Il Rational Unified Process e' un modello che si rifa' a UML per la progettazione di sistemi che prende elementi da vari modelli generici e fornisce buone prassi in fatto di progettazione e specifica. E' particolarmente indicato per sistemi che fanno uso di cicli. Le sue fasi sono:
1. Inception: il progetto ha inizio con il dialogo con il cliente, il quale fornisce i requisiti del sistema; si propone un'architettura di base e un piano per la natura iterativa e incrementale del progetto
2. Elaboration: comunicazione con il cliente, modellazione, si ampliano gli uses cases e si espande la rappresentazione dell'architettura
3. Construction: si sviluppa il software
4. Transition: il software e' pronto per essere installato nell'ambiente reale
Queste milestones, se non producono risultati soddisfacenti, devono esser reiterate oppure si deve abortire il progetto

==**Agile**==
Il modello di progettazione Agile, o Extreme Programming, e' un modello molto recente e si basa su quattro punti:
1. Le persone e le iterazioni sono piu' importanti di software e processi
2. Il software funzionante e' piu' importante rispetto alla documentazione
3. La comunicazione e' fondamentale
4. La capacita' di risposta ai cambiamenti e' piu' importante rispetto ad aderire al progetto, gli sviluppatori dovrebbero suggerire modifiche.
Questo modello ha la particolarita' di prevedere che lo sviluppo sia condotto da due sviluppatori, uno che scrive codice e l'altro che assiste, in maniera tale da favorire la concentrazione e la buona programmazione. Inoltre presenta costi dovuti al cambiamento dei requisiti molto inferiori rispetto a quelli del modello a cascata.

==**Che cosa si intende con i termini “verifica” e “validazione” di sistemi software?Cosa vuole dire effettuarle in modo “statico” o “dinamico”?**==
Per validazione si intende il processo che risponde alla domanda "si sta costruendo il prodotto giusto?", ossia si chiede se il software e' implementato in maniera tale da soddisfare le richieste dell'utente.
Per verifica si intende il processo che risponde alla domanda "si sta costruendo il prodotto nel modo giusto?", ossia ci si assicura che il software soddisfi le specifiche imposte dal cliente.
Quando si effettuano test statici si analizza il codice in maniera statica per ricercare problemi (la si puo' effettuare ancor prima che il sistema sia implementato). 
I test dinamici invece prevedono il collaudo del software; E' impossibile effettuare la validazione solo in maniera dinamica.

==**Uno dei principi fondamentali della progettazione in interfacce utente richiededi ridurre il ricorso alla “memoria a breve termine”. Che cosa si intende e com'è possibile realizzare in pratica questo principio?**==
Affinché si possa raggiungere questo scopo, e' importante che l'interfaccia grafica a cui e' sottoposto l'utente sia di facile lettura e conforme a standard assodati in termini di estetica e usabilita', in maniera tale che non si tratti di un processo di apprendimento, bensi' di riconoscimento. Inoltre l'UI dovrebbe sempre fornire all'utente informazioni circa le operazioni svolte in precedenza e in maniera progressiva. Un'altra ragione per ridurre la mode di informazioni che l'utente deve ricordare e' che ad una maggiore memorizzazione corrisponde una maggiore propensione a commettere errori nella interazioni con il sistema.

==**Cosa si intende con “analisi dei cammini critici” durante la pianificazione di un progetto? Cosa implica il cammino critico e che strumento si può usare per studiare la situazione?**==
Il cammino critico e' la successione di attivita' legate allo sviluppo del software per cui un ritardo su una qualsiasi delle attivita' interessate risulta in un ritardo nella consegna. Per l'analisi dei cammini critici si intende lo studio della disposizione temporale dei compiti all'itnerno di un progetto volta ad ottimizzarla per mezzo di una ri-schedulazione dei compiti che tenga conto della disposizione  del personale, delle dipendenze tra le attività (parallelismo) e dei tempi di consegna. Attraverso l'analisi dei cammini critici si puo' ridurre la durata di sviluppo del progetto.
Uno strumento utile per lo studio dei cammini critici e' il diagramma di Pert.
E' molto oneroso compiere quest'analisi quando il progetto e' gia' in esecuzione.

==**Durante la fase di progettazione architetturale, cosa si intende con “scomposizione modulare”? Potete fare qualche esempio?**==
Per scomposizione modulare si intende il raffinamento a livello strutturale per cui i sottosistemi sono scomposti in moduli: cio' comporta una riduzione del tempo di sviluppo, ma richiede che il sistema sia effettivamente partizionabile. Alcuni esempi di scomposizione modulare sono il modello a oggetti, che fa uso di interfacce ben definite e modelli di controllo, e il modello data-flow, il quale e' riconducibile all'architettura "pipe-and-filter" e si presta molto alla gestione dei dati (ma non a sistemi interattivi).

==**Come valuto se un rischio e' grave o meno?**==
Per ogni rischio bisogna valutare l'impatto, le conseguenze e la probabilita' che si verifichi una situazione problematica che impedisca al programma di funzionare adeguatamente. Non tutti i rischi, e fallimenti che ne derivano sono uguali, per esempio il rischio di un fallimento corruttivo e' molto piu' grave del rischio di un fallimento transiente recuperabile a parita' di probabilita' di occorrenza. In altre parole bisogna chiedersi "cosa si perde qualora il rischio si verificasse?".
Il fattore rischio si misura come un prodotto tra probabilita' che il rischio si verifichi e la sua gravita'.

**==Raccolta requisiti: i problemi (scope eccetera)==**
Durante la raccolta dei requisiti i problemi che possono verificarsi e che impattano le fasi successive del processo di ingegneria dei requisiti sono:
- problemi di scope, per esempio limiti del sistema mal definiti e presenza di requisiti non necessari che provocano confusione
- problemi di comprensione, dati dall'incapacita' del cliente e dello sviluppatore di comunicare tra loro le caratteristiche del software; molto spesso il cliente non sa precisamente cosa vuole, pertanto lo sviluppatore per evitare di incorrere in problemi di questo genere deve riuscire a instaurare un dialogo con gli stakeholders e studiare l'ambiente in cui il software verra' rilasciato.
- problemi di volatilita', ossia il fatto che nel tempo i requisiti possono cambiare.

**==Modello architetturale client-server==**
Il modello client-server si basa sulla richiesta di servizi da parte del client nei confronti del server.
Uno dei modelli principali e' il three-tier architecture (di cui fanno parte i framework MVC), il quale si compone di tre livelli:
- Presentation layer: si tratta dell'interfaccia grafica attraverso la quale l'utente interagisce con il sistema
- Application processing layer: si occupa di fornire la funzionalita' del sistema
- Data management layer: si occupa della comunicazione con il DBMS
I vantaggi di questo modello risiedono nella sua semplicita' ed efficacia in fatto di scalabilita' e contenimento dei costi; gli svantaggi sono la necessita' di replicare alcune componenti di gestione dei dati tra il DBMS e il Data management layer, e che la mancanza di un modello unico e condiviso puo' comportare un calo di prestazioni.

**==Diversità tra i problemi HW e SW e come posso risolverli==**
Il software, al contrario dell'hardware, non si erode nel tempo, dunque non si puo' sostituire una  componente guasta senza imbattersi nello stesso problema.
Nel caso di fallimento di una componente hardware, la ridondanza costruttiva permette di superare efficacemente malfunzionamenti; il concetto di ridondanza e' simile anche in ambiente software, ma, anziche' basarsi su repliche della componentistica, sfrutta diverse implementazioni dello stesso modulo e confronta tra loro le soluzioni trovate.

**==Modularizzazione del problema (Mayer)==**
Una strategia di modularizzazione che trovi un buon compromesso tra costi di sviluppo e integrazione e' seguire i 5 criteri di Mayer:
- scomponibilita' $\rightarrow$ implica meno complessita'
- componibilita' $\rightarrow$ implica piu' produttivita'
- comprensibilita' $\rightarrow$ ossia interfacce minime, implica piu' facilita' di costruzione e modificabilita'.
- continuita' $\rightarrow$ poche ripercussioni in caso di modifiche ai requisiti dei sistemi
- protezione $\rightarrow$ gli effetti anomali non si ripercuotano su altre parti del software, piu' manutenibilita'

Altre strategie di modularizzazione del problema sono:
- top-down $\rightarrow$ si partiziona il problema a partire dai sottosistemi maggiori, scendendo verso i sottosistemi atomici e trattabili
- bottom-up (o per composizione)
- Sandwich (soluzione naturale)

**==Quando si parla di validazione del software, cosa si intende coni termini “revisione” e “walkthrough”?==**
La revisione di progetto consiste in riunioni in cui si esamina il codice a seguito dello sviluppo del processo, ricorrendo alla compilazione di checklist di errori comuni.
Il walkthrough, che e' un processo meno rapido rispetto alla revisione, consiste in una lettura critica del codice con l'intento di simularne l'esecuzione; si tratta di un processo collaborativo che si basa sull'esperienza degli sviluppatori.

**==Cos'è la visibilità - modello a cascata==**
Per visibilita' si intende la proprieta' di un progetto di produrre documentazione durante le proprie fasi. Il modello a cascata ha una buona visibilita', infatti ogni fase produce deliverables.

**==Modelli di progettazione architetturale della struttura di sistema==**
- Repository
- Client-Server
- Flusso di dati Pipe and Filter
- Macchina astratta

**==Tipi di modelli di progettazione==**
- Modelli a struttura di sistema
- Modelli di controllo centralizzato
- Modelli bassato su eventi

**==Rapporto mese-uomo==**
Per valutare l'effort di un progetto in funzione del numero di sviluppatori e' possibile ricorrere al "mese-uomo". Questa misura e' efficace solamente in casi in cui il compito da svolgere e' perfettamente partizionabile tra gli sviluppatori e non richiede comunicazione. Per questo motivo difficilmente la si puo' utilizzare per valutare il numero di sviluppatori richiesti per un task, infatti l'aggiunta di programmatore richiede che questi vengano adeguatamente formati ad altri membri del personale, i quali dovranno rinunciare a parte della propria produttivita'.

**==Le 3 regole della UI==**
1. Il controllo deve essere nelle mani dell'utente: l'utente deve avere un'interazione flessibile e quindi avere sempre la possibilita' di interrompere o annullare le operazioni in corso; cio' non significa che all'utente debbano essere fornite informazioni di carattere tecnico
2. L'utente deve ricorrere il meno possibile all'uso della memoria
3. L'interfaccia deve essere uniforme: il sistema deve avere un aspetto grafico consistente in ogni suo elemento; e' inoltre opportuno che si rispettino canoni statici e di usabilita' assodati, cosi' e' come importante che si mantengano modelli interattivi preesistenti

**==Architettura a flusso di dati (pipe-and-filter)==**
Si tratta di un modello architetturale (tipico delle shell nei sistemi operativi tradizionali) che favorisce la manipolazione di input codice attraverso filtri. E' un modello semplice ed efficace per gestire computazioni complesse attraverso la concatenazione di numerosi filtri semplici, tuttavia e' poco adatto quando la struttura non e' facilmente "linearizzabile"

==**White-box**==
Per white box si intende il collaudo del software di cui si conosce il codice e per cui si controllano tutti i vari cammini indipendenti possibili interni ai moduli. Questo tipo di collaudo e' estensivo ed e' ad opera degli sviluppatori. Si studiano gli aspetti procedurali del programma piuttosto che mancanze dei requisiti.

==**Black-box**==
Per collaudo black-box si intende il collaudo del software di cui non si conosce il codice, e dunque si valuta la conformita' delle specifiche indicate nella specifica dei requisiti (disinteressandosi quindi della struttura interna). si cercano comportamenti erronei del programma o mancanze. E' complementare al collaudo white-box

**==Descrivere cosa si intende, rispettivamente, per requisiti funzionali e requisiti non-funzionali. Potete fare qualche esempio per entrambi i tipi?==**
Per requisiti funzionali si intendono i servizi che il programma offre o le sue funzionalita'. Per requisiti non funzionali si intendono gli aspetti di programmazione, come librerie di codici e sistemi di sviluppo, che se non rispettati possono portare il programma ad essere inutilizzabile. Inoltre, per proprieta' non funzionali si intendono caratteristiche come reliability, performance, security.

**==Design pattern (in generale cosa sono)==**
I design pattern sono soluzioni consolidate e accettate per un problema corrente. La progettazione di software che segue design pattern favorisce il "concept-reuse" e permette la scrittura di codice chiuso alle modifiche e aperto alle estensioni.

**==Descrivere cosa si intende per affidabilità di un sistema software, e come conviene procedere per cercare di migliorarla==**
Per affidabilita' si intende la probabilita' che il sistema funzioni senza errori per un dato intervallo di tempo, in un dato ambiente e per un determinato scopo. E' una proprieta' molto importante di ogni sistema, ma e' molto difficile e costoso ottenere un'affidabilita' tale da garantire la correttezza di ogni output per ogni possibile input, pertanto e' piu' importante nella maggior parte dei casi diminuire il piu' possibile la possibilita' che si verifichino fallimenti gravi piuttosto che implementare funzioni di recupero da fallimenti transienti piuttosto che applicare una politica di fault avoidance. Inoltre, molto spesso gli sforzi per l'aumento dell'affidabilita' comportano un costo in prestazioni ed efficienza non indifferenti, senza avere la garanzia di non aggiungere errori non previsti.
Esistono numerosi metodi per misurare l'affidabilita':
- POFOD: misura la probabilita' che si verifichi un fallimento a seguito di un input
- MTTF: misura il tempo medio tra fallimenti
- AVAIL: misura il tempo necessario per ripristinare l'operativita' del sistema a seguito di un fallimento
Alcuni passi per migliorare l'affidabilita' del prodotto finito e' applicare nelle fasi di progettazione e di sviluppo sani principi di buona programmazione, per cui evitare goto e fare un uso ragionato dell'ereditarieta' e dell'allocazione in memoria.

==**A cosa serve la fase di “negoziazione” durante l’analisi dei requisiti?**==
La negoziazione durante l'analisi dei requisiti e' molto importante ai fini della stesura della specifica dei requisiti in quanto permette agli sviluppatori di valutare i requisiti proposti dai clienti e i vincoli indicati dagli stakeholders e imposti dall'ambiente, cosi' da poter trovare compromessi e risolvere conflitti per accontentare tutte le parti in gioco.

**==Descrivere cosa si intende per “piano di progetto” e per “piano esecutivo”==**
Il piano di progetto e' la sequenza potenziale dei compiti da svolgere per completare il processo che tiene conto solamente dei vincoli di precedenza tra i vari tasks, ignorando fattori come la durata di sviluppo e le risolrse economiche.
Il piano esecutivo e' la sequenza effettiva delle attivita' elementari che tiene conto dei fattori economici, delle tempistiche di sviluppo e della gestione del personale.

**==Back-to-back-testing==**
Utilizzato quando sono disponibili piu' versioni diverse dello stesso sistema
- se l'output e' diverso nelle diverse versioni ci sono errori potenziali
- il confronto puo' essere automatizzato per ridurre il costo dei testi
- si puo' usare quando e' disponibile un prototipo, quando un sistema viene sviluppato in piu' versioni (magari su diverse piattaforme), oppure nel caso di upgrade o nuove release del sistema

==**TMR - Triple Modular Redundancy**==
Nell'hardware la tolleranza avviene attraverso la triple-modular redundancy, ossia tre componenti identiche che ricevono gli stessi input e devono generare gli stessi output: e' diverso dagli altri due, viene ignorato e la componente che l'ha prodotto viene dichiarata guasta. Si assume che le componenti hw falliscano causa usura, non perche' progettate male.