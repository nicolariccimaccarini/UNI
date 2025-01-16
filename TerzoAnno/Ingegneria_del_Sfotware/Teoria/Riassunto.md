
## Capitolo 1 - SW ENGINEER

### Che cos'e' il software?
E' un insieme di programmi e la loro documentazione. Esistono due fondamentali tipi di software:
- SW GENERICI $\rightarrow$ sono sistemi prodotti da un'organizzazione di sviluppo e venduti al mercato "aperto" per tutti i clienti che sono in grado di comprarli. (es. sw per databases, word, drawing, ...); ampio spettro di utilizzo
- SW SPECIFICI $\rightarrow$ sw destinati e commissionati da una particolare clientela. Es: sistema di controllo devices tecnologici o sistemi di controllo del traffico aereo.

### Quali sono le caratteristiche di un sw?
Mentre i costi sw aumentano i costi hw diminuiscono, in quanto c'e' una difficolta' della stima deo costi sw derivanti a problemi quali:
- l'incapacita' di dimensionare accuratamente un progetto; 
- l'incapacita' di definire nel suo complesso il processo e l'ambiente operativo del progetto;
- effettuare una valutazione inadeguata del personale sia in quantita' che in qualita';
- sbagliata/inadeguata raccolta dei requisiti per la stima di attivita' specifiche all'interno del progetto.

Mentre il tempo di sviluppo aumenta, i costi per mantenere un software aumentano.
Mentre gli errori sw incrementano, i guasti hw diminuiscono $\rightarrow$ significa che quando l'hw funziona correttamente si possono effettuare delle correzioni a livello software.

### Quali sono gli attributi di un sw?
Le qualita' di un sw possono essere:
- INTERNE $\rightarrow$ riguardano caratteristiche di implementazione non visibili all'utente
- ESTERNE $\rightarrow$ riguardano le caratteristiche di funzionalita' al momento dell'esecuzione visibili all'utente finale.
Ovviamente non e' possibile ottenere le qualita' esterne se non si dispone delle qualita' interne.

Gli attributi essenziali sono riassunti dal triangolo di McCall, tra questi troviamo:
- MAINTAINABILITY $\rightarrow$ sw dev'essere scritto in modo che possa evolvere con il cambiamento delle esigenze dei consumatori.
- DEPENDABILITY E SECURITY $\rightarrow$ non deve creare danni fisici o economici nell'eventualita' di "system failure". Utenti malintenzionati non devono poter accedere e danneggiare il sistema.
- EFFICIENCY $\rightarrow$ sw non deve sprecare risorse.
- ACCEPTABILITY/USABILITY $\rightarrow$ il sw deve essere accettabile dagli utenti per cui e' stato sviluppato: comprendibile, usabile e compatibile con gli altri sistemi usati da loro.

### Che cos'e' l'ingegneria del software
L'ingegneria del software e' una disciplina che riguarda tutti gli aspetti sulla produzione di un sw. Ha il compito di creare e mantenere applicativi sw usando tecnologie e tecniche derivanti sia dall'informatica che da altri campi come l'ingegneria gestionale.

### Differenza tra ingegneria del sw e informatica
L'informatica riguarda la teoria e i fondamenti come gli algoritmi e i linguaggi, mentre l'ingegneria del sw riguarda le pratiche di sviluppo e consegna di un software di qualita'.

### Differenza tra l'ingegneria del sw e l'ingegieria dei sistemi
L'ingegneria dei sistemi riguarda tutti gli aspetti dello sviluppo di un sistema basato su computer, inclusi aspetti hw, sw e di processo, mentre l'ingegneria del sw e' solo una parte di esso.

### Cos'e' il processo di produzione del sw?
Un set di attivita' con lo scopo di sviluppare o/ed evolvere il software.
Le attivita' di tutti i processi di produzione sono:
- SPECIFICA $\rightarrow$ cosa deve fare il sistema e quali sono i vincoli di progettazione
- SVILUPPO $\rightarrow$ produzione del sistema sw
- VALIDAZIONE $\rightarrow$ verifica che il sistema sia coerente alle richieste del cliente (testing)
- EVOLUZIONE $\rightarrow$ modifica del sw in base alle nuove esigenze del cliente

### Quali sono i problemi nel processo di sviluppo di un sw?
- Specifiche incomplete/incoerenti
- Mancanza di distinzione tra specifica, progettazione ed implementazione
- Assenza di un sistema di validazione
- Il sw non si consuma $\rightarrow$ la manutenzione e' la modifica del prodotto rispetto a nuove esigenza da parte del cliente.

### Cos'e' un modello di produzione del sw?
Una rappresentazione semplificata del processo di produzione, presentata da una specifica prospettiva. Esso include le attivita' ce sono parte de processo, prodotti sw e il ruolo delle persone implicate in esso.

### Quali sono i costi legati alla produzione sw?
Il 60% dei costi e' legato ai costi di sviluppo e il 40% alla verifica e validazione del sw; anche se questi costi variano a seconda del tipo di sistema che si deve utilizzare. I costi di sviluppo sono cosi' alti e sono difficili da stimare, perche' comprendono anche i costi di evoluzione sw.
I costi variano a seconda del tipo di sistema che dev'essere sviluppato, alla performance e all'affidabilita'.
L'80% degli errori avviene durante la raccolta requisiti o la fase di design. 
Esistono 3 categorie importanti dei progetti sw: 
- SUCCESS, ossia il progetto terminato con successo nei tempi previsti(16,2%) 
- CHALLENGED, progetto consegnato in ritardo e con un aumento di costi (52,7%)
- IMPAIRED, progetto cancellato per costi troppo alti e per un tempo di consegna non definito-troppo alto(31,1%).

### Quali sono le sfide che l'ingegnere del sw deve affrontare?
- THE HETEROGENEY CHALLENGE $\rightarrow$ la sfida dello sviluppo di tecniche per la costruzione di sw sia affidabili che flessibili abbastanza per far fronte all'eterogeneita' dei linguaggi di programmazione.
- THE DELIVERY CHALLENGE  $\rightarrow$ ridurre i tempi di consegna  per sistemi grandi e complessi senza compromettere la qualita' dei sistemi.
- THE TRUST CHALLENGE $\rightarrow$ sviluppare tecniche che dimostrano la sicurezza del software agli utenti.

Le richieste di tempi di consegna ridotti e lo sviluppo di software affidabili, mantenendo legacy system eterogenei (compatibili ed evoluti sia a livello hw che sw all'era attuale).
Queste sfide non sono indipendenti fra loro, per esempio, per fare dei rapidi cambiamenti ad un sistema legacy, abbiamo bisogno di strumenti e tecniche innovativi che si ottengono usando metodi esistenti dell'ingegneria del sw.

---

## Capitolo 2 - SISTEMI

### Complessita' di un sistema: problema di scalabilita' dei sottosistemi
Un sistema e' un insieme di componenti correlati che lavorano insieme per raggiungere un obiettivo. I sistemi che includono sw rientrano in due categorie:
- TECHNICAL COMPUTER-BASED SYSTEM $\rightarrow$ i sistemi che includono componenti hw e sw ma non le procedure e i processi; per esempio tv, cellulari e molti sw per pc.
- SOCIO-TECHNICAL SYSTEMS $\rightarrow$ includono uno o piu' technical system, ma includono anche la conoscenza di come il sistema potrebbe essere utilizzato per raggiungere degli obiettivi.
	- Questi sistemi hanno definito processi operazionali, che includono persone come parte del sistema e ce sono governati da  politiche organizzative e regole.
	- Questi socio-technical systems hanno 3 essenziali caratteristiche:
		1. Hanno proprieta' emergenti (funzionali e non)
		2. Sono spesso non deterministici, ossia che i comportamenti del sistema non sono sempre gli stessi, ma dipendono dalle persone che effettuano gli input

I sistemi sono spesso gerarchici e includono sottosistemi. I sottosistemi sono indipendenti, per cui possono essere utilizzati in differenti sistemi.
Il numero di sottosistemi e' dato dall'ampiezza dell'alberodi decomposizione del sistema e dalla sua profondita'. Data l'eterogeneita' di tipo dei sottosistemi e' difficile definire le relazioni tra di essi, quindi servono capacita' scientifiche e manageriali per effettuare un socio-technical system (approccio multidisciplinare).

### Modellazione di un sistema
Prima di modellare un sistema si valutano le caratteristiche qualitative o quantitative.
- Si definisce una serie di processi che rappresentano l'entita' della realta' fisica
- Si definisce il comportamento di ciascun proceso
- Si definiscono i dati del sistema, che possono essere esogeni (che provengono dall'esterno) o endogeni (che il sistema scambia dal proprio interno).

Gli strumenti di modellazione e simulazione permettono di costruire sistemi con una diminuzione di errori, perche' gia' modellati.

Esistono modelli architetturali (mostrano in modo astratto la struttura in sottosistemi), gerarchici (organizzazione ad albero) o endogeni (rappresentano i flussi di informazion3e tra i vari sottosistemi).

### Affidabilita' di un sistema
L'affidabilita' compressiva dipende da quella hw,  sw e quella degli operatori. Per verificare l'affidabilita' ci poniamo le seguenti domande:
- Quanto e' probabile un fallimento hw e quanto tempo e' richiesto per prepararlo?
- Quanto e' probabile che una componente sw produca un output sbagliato?
- Quanto e' probabile che un operatore commetta errori?

La maggior parte degli errori dipende dal software e l'interdipendenza delle componenti fa si che gli errori si propaghino per tutto il sistema, per cui molti sistemi complessi sono considerati un fallimento a causa sw.

Il sistema deve, quindi, essere RESILIENTE, ovvero continuare ad operare correttamente in presenza di fallimenti di uno o piu' componenti.

### Quali sono le proprieta' emergenti di un sistema?
Proprieta' del sistema visto globalmente, in quanto derivanti dall'integrazioni delle componenti.
Esistono le proprieta' funzionali, ovvero le proprieta' che appaiono solo quando le varie componenti vengono integrate e cooperano per uno scopo comune e non-funzionali, ovvero le proprieta' che riguardano il comportamento del sistema nel suo ambiente operativo, come reliability, performance, safety, etc.

### Quale relazione incorre tra l'affidabilita' delle componenti di un sistema?
L'ambiente in cui il sistema opera puo' influenzare la sua affidabilita', in quanto l'ambiente puo' condizionare il comportamento di un sistema.
Es. i guasti hw possono causare segnali spuri (non autentici/falsificati) nel sw e causare la produzione di segnali non corretti, mentre gli errori nel sw possono causare stress nell'operatore aumentando la sua propensione a commettere errori.

### Acquisizione di un sistema, progettazione, sviluppo
Un sistema puo' essere costruito o acquisito, nel caso dell'acquisizione, e' necessario sapere la specifica del sistema (cosa e' richiesto al sistema) e l'architettura di progetto.

PROCESSO DI ACQUISIZIONE:
- sistemi "off the shelf" (quali sistemi comprare)
	1. Analisi di mercato (definire le specifiche per la clientela)
	2. Adattare i requisiti
	3. Scegliere il sistema
	4. Richiedere  i preventivi
	5. Scegliere il fornitore
- sistemi dedicati (quali sistemi sviluppare)
	1. Analisi di mercato
	2. Richiesta di offerte
	3. Selezionare le offerte
	4. Negoziare il contratto
	5. Contrattare lo sviluppo

Nella progettazione di un sistema c'e' multidisciplinarieta', perche' coinvolge vari tecnici di aree diverse; di solito segue un modello di sviluppo a cascata per poter sviluppare parallelamente le diverse componenti del sistema; dati gli alti costi di modifica c'e' poco spazio per le iterazioni fra le diverse fasi; il sottosistema sw dev'essere il piu' flessibile (in quanto le modifiche hw sono piu' costose e complesse e dovrebbero esser compensate dal sw).

SVILUPPO DI UN SISTEMA:
1. Definizione dei requisiti
2. Progettazione in sottosistemi (disegno del sistema, come le funzionalita' del sistema devono essere fornite dalle diverse componenti)
	- organizzare  i requisiti, separandoli in gruppi collegati
	- identificare i sottosistemi (ogni sottosistema soddisfa un gruppo di requisiti)
	- assegnare i requisiti ai sottosistemi
	- specificare le funzionalita' dei sottosistemi
	- definire le interfacce dei sottosistemi
3. Sviluppo dei sottosistemi $\rightarrow$ implementare ciascuno dei sottosistemi individuati (puo' richiedere un nuovo processo di sviluppo)
4. Integrazione del sistema $\rightarrow$ mette insieme hw e sw e risorse umane. I sottosistemi vengono integrati uno alla volta per ridurre i costi di individuazione degli errori.
5. Installazione del sistema $\rightarrow$ inserire il sistema completo nel suo sistema operativo. Questo puo' portare vari problemi, quali:
	- coesistenza dei due sistemi (nuovo + precedente)
	- l'ambiente finale puo' essere diverso da quello di sviluppo
	- resistenza al nuovo sistema da parte degli utilizzatori
	- problemi pratici (cablaggio, corrente, ...)
6. Mantenimento del sistema $\rightarrow$ la durata di un sistema e' legata alla sua dimensione, l'evoluzione e' costosa (interdipendenza sottosistemi, affidabilita', ecc.)
7. Decommissioning $\rightarrow$ dimissione

### Come vengono definiti i requisiti
I requisiti globali sono quelli funzionali (cosa il sistema deve fare), non funzionali (proprieta' del sistema + vincoli) e i requisiti che un sistema non deve avere.

---

## Capitolo 3: SW LIFE CYCLE

### Processo di produzione del sw: ciclo di riszoluzione di un problema
Il processo di produzione di un sw e' l'insieme di attivita' per la specifica, il progetto, l'implementazione e la verifica dei sistemi sw.
- Status quo $\rightarrow$ stato della situazione attuale
- Definizione del problema $\rightarrow$  individuo il problema da risolvere
- Sviluppo tecnico $\rightarrow$ risolvo il problema con una opportuna tecnologia
- Integrazione $\rightarrow$ consegno i risultati al committente

Questo ciclo di produzione puo' essere fatto sia a livello dell'intero sistema che ai singoli sottoinsiemi o funzioni.

### Modelli di processo del sw
Modelli di produzione $\rightarrow$ rappresentazione semplificata del software
#### Modello a cascata
Il modello a cascata si compone delle seguenti 5 fasi:
1. Definizione dei requisiti $\rightarrow$ si lavora alla specifica dei requisiti dopo aver avuto un dialogo con il committente
2. Progettazione del sistema $\rightarrow$ si effettua la progettazione dell'architettura del sistema e delle sue componenti
3. Implementazione e testing delle singole componenti
4. Integrazione e testing delle singole componenti
5. Installazione e manutenzione $\rightarrow$ si installa il sistema e se ne prende cura.

E' possibile passare da una certa fase a quella precedente, tuttavia il cambiamento dei requisiti in corso d'opera e' estremamente costoso.
Dal punto di vista della visibilita', il modello di sviluppo a cascata e' buono perche' produce deliverables (risultati) ad ogni fase.
Il rischio di questo modello di sviluppo dipende dalla familiarita' del team nei confronti del problema da affrontare.

#### Modello RAD (Rapid Application Development) (5 - CPMS)
Il modello RAD e' particolarmente indicato nei casi in cui si deve sviluppare un progetto facilmente partizionabile in cui non vi sia la necessita' di utilizzare interfacce appositamente definite. Inoltre non e' il modello indicato qualora si utilizzassero tecnologie innovative con alto rischio di incontrare problemi in corso d'opera.
Le fasi che compongono questo modello sono:
1. Comunicazione
2. Pianificazione
3. Modellazione
4. Costruzione
5. Deployment

In particolare, la modellazione e la costruzione sono ad opera di ciascun team incaricato di lavorare su una partizione del progetto]

#### Modello evolutivo
Lo sviluppo evolutivo da importanza alla produzione di prototipi usa-e-getta a partire da definizione dei requisiti di massima, ossia si da molta importanza alla scrittura del codice, il quale evolve le direttive del cliente. Le fasi sono:
1. Definizione di massima
2. Specifica, che produce la versione iniziale
3. Sviluppo, che produce la versione intermedia
4. Validazione, che produce la versione finale

La visibilita' e' scarsa a causa della produzione di documenti carente, il che puo' elevare il livello di rischio (che pero' allo stesso tempo e' ridotto grazie allo sviluppo di numerosi prototipi). E' fondamentale che il prodotto finale non sia un prototipo: una volta raggiunti gli obiettivi di sviluppo e' opportuno scrivere il software per la release. Questo modello si presenta particolarmente per progetti di piccola dimensione con una vita attesa corta.

#### Modello formale o trasformazionale
Il modello di sviluppo formale e' adeguato nel momento in cui si prefigge, ad esempio, di codificare linguaggio matematico.
1. Definizione dei requisiti
2. Specifica formale
3. Trasformazione formale
4. Integrazione e test

Il rischio e' legato all'esperienza degli sviluppatori e dalla necessita' di tecnologie avanzate. La visibilita' e' buona, ogni fase dovrebbe produrre documentazione affinché il processo di sviluppo possa svolgersi.

#### Modello basato sul riutilizzo (AAPI)
Il modello di sviluppo basato sul riutilizzo si bassa sull'uso di componenti off-the-shelf gia' scritte per la produzione software.
Le fasi di questo modello sono:
1. Analisi dei componenti
2. Adattamento dei requisiti
3. Progettazione del sistema
4. Integrazione

La visibilita' e' buona, pero' puo' essere macchinoso scrivere documentazione per componenti off-the-shelf. Il rischio e' ridotto, dato che si presuppone la correttezza dei componenti riutilizzati.
Questo modello si presta molto allo sviluppo di software ad oggetti. 

#### Modello a spirale (iterativo)
Il modello di sviluppo a spirale prevede i seguenti step, che si reiterano numerose volte fino alla release finale del sw.
1. Comunicazione con il cliente
2. Progettazione del sistema
3. Analisi dei rischi
4. Strutturazione
5. Costruzione e release
6. Valutazione del cliente

Questo modello e' indicato per progetti di grandi dimensioni. La visibilita' e' buona perche' ogni spicchio della spirale produce documentazione. I rischi sono minimizzati dalle numerose iterazioni, pero' la erronea valutazione dei rischi puo' ripercuotersi successivamente.

#### Modello RUP (Rational Unified Process)
Il Rational Unified Process e' un modello che si rifa' a UML per la progettazione di sistemi che prende elementi da vari modelli generici e fornisce buone prassi in fatto di progettazione e specifica. E' particolarmente indicato per sistemi che fanno uso di cicli. Le sue fasi sono:
1. Inception $\rightarrow$ il progetto ha inizio con il dialogo con il cliente, il quale fornisce i requisiti del sistema; si propone un'archiettura di base e un piano per la natura iterativa e incrementale del progetto
2. Elaboration $\rightarrow$ comunicazione con il cliente, modellazione, si ampliano gli use cases, e si espande la rappresentazione dell'archiettura
3. Construction $\rightarrow$ il software e' pronto per essere installato nell'ambiente reale

Queste milestones, se non producono risultati soddisfacenti, devono essere reiterate oppure si deve abortire il progetto.

#### Valutazione dei rischi nei vari modelli
Per ogni rischio bisogna valutare l'impatto, le conseguenze e la probabilita' che si verifichi una situazione problematica che impedisca al programma di funzionare adeguatamente. Non tutti i rischi e i fallimenti che ne derivano sono uguali, per esempio il rischio di un fallimento corruttivo e' molto piu' grave di un fallimento transiente recuperabile a partita' di probabilita' di occorrenza. In altre parole bisogna chiedersi "cosa si perde qualora il rischio si verificasse?".
Il fattore di rischio si misura come prodotto tra probabilita' che il rischio si verifichi e la sua gravita'.

#### Metodologie di sviluppo agile (xp)-1999
Il modello di progettazione Agile, o Extreme Programming, e' un modello molto recente e si basa si quattro punti:
1. Le persone e le interazioni sono piu' importanti di software e processi
2. Il software funzionante e' piu' importante rispetto alla documentazione
3. La documentazione e' fondamentale
4. La capacita' di risposta ai cambiamenti e' piu' importante rispetto ad aderire al progetto

Questo modello ha la particolarita' di prevedere che lo sviluppo sia condotto da due sviluppatori, uno che scrive il codice e l'altro che assiste, in maniera tale da favorire la concentrazione e la buona programmazione. Il testing (casi di prova) e' obbligatorio, se il test non funziona si ferma lo sviluppo e si fissa il problema. Inoltre presenta costi dovuti al cambiamento dei requisiti molto inferiori rispetto a quelli del modello a cascata.

---

## Capitolo 4: SW PROJECT MANAGMENT
### Che cos'e' un progetto?
Un progetto e' un insieme ben definito di attivita' che hanno:
- un inizio 
- una fine
- uno scopo
- viene portato avanti da  un insieme di persone
- utilizza un insieme di risorse
- non e' un lavoro di routine
Il project manager deve conciliare i tempi, scopo e budget

### Quali sono le possibili problematiche?
il sw e' tangibile (intoccabile), per valutare i progressi bisogna basarsi sulla documentazione. Non esiste uno standard per la produzione di un progetto perche' ogni caso e' a se'.
Motivi per cui un progetto puo' essere in ritardo:
- Deadline non realistica (impostata da un componente esterno allo staff tecnico)
- Cambiamenti dei requisiti
- Stima troppo ottimistica
- Rischi non presi in considerazione (difficolta' tecniche e umano imprevedibili)
- Problemi di comunicazione nel gruppo
- Incapacita' di riconoscere u ritardo e mancata attuazione delle contromisure

### Caso peggiore
In caso di riconoscimento ritardo ci sono 3 possibilita':
1. Aumento del budget e aggiungo risorse (possibile solo all'inizio)
2. Elimino le funzionalita' non essenziali $\rightarrow$ sviluppo prototipo
3. Non tengo conto dell'analisi e procedo fino al fallimento

### Come evitare la maggior parte dei problemi?
Seguire i principi fondamentali della pianificazione:
1. Un progetto deve essere ripartito in attivita' e compiti di dimensioni ragionevoli
2. Il numero di persone coinvolte e' definito a priori e non si deve accedere
3. Determinare le dipendenze tra attivita' e compiti
4. Determinare quali compiti svolgere in parallelo e quali svolgere in sequenza
5. Ogni compito deve avere una data di inizio e una di fine e deve essere associato a qualcuno, che deve produrre per ognuno un risultato predefinito
6. Ad ogni compito si deve associare almeno un punto di controllo, ossia verificare la qualita' di uno o piu' compiti.

### Quali sono gli attori sulla scena e di cosa si occupano?
1. SENIOR MANAGERS $\rightarrow$ definiscono gli aspetti economici del progetto
2. PROJECT MANAGERS $\rightarrow$ pianificano, organizzano e controllano lo sviluppo del progetto. Piu' precisamente si occupa della stesura del progetto, della stima dei costi, del planning e scheduling, del monitoraggio e revisione del progetto, della selezione dello staff tecnico e dell'assegnazione dei compiti e della stesura dei rapporti e delle presentazioni. Per questi motivi e' una figura necessaria ed essenziale nel progetto. 
3. PRATICIONERS $\rightarrow$ chi ha competenze tecniche per realizzare parti del progetto (sviluppatori)
4. CUSTOMERS $\rightarrow$ cliente che definisce i requisiti del sw
5. END USERS $\rightarrow$ chi utilizzera' il sw una volta sviluppato

### Come organizzare le attivita' e lo scheduling?
Le attivita' devono produrre documentazione affinché i manager possano valutare l'avanzamento del progetto. Le MILESTONES sono i punti finali di ogni attivita' e le DELIVERABLES sono i risultati intermedi del progetto consegnati al cliente.

Lo scheduling e' la suddivisione in task del progetto e la stima del tempo e risorse necessarie per completare ogni compito.
Buona pratica e' minimizzare le dipendenze tra task per evitare la propagazione dei ritardi a catena.
I problemi dello scheduling derivano da una difficolta' di stima dei costi di sviluppo e del tempo, dato che la produttivita' non e' proporzionale al numero di persone che lavorano in un progetto.

Generalmente viene utilizzata la regola del 40-20-40:
- 40% del tempo per l'analisi e la progettazione
- 20% del tempo per la stesura del codice
- 40% del tempo per il collaudo

### Possiamo regolarizzare?
NO. A parita' di obiettivi si hanno benefici maggiori impiegando un numero di persone inferiore per un periodo piu' lungo.

### Creazione di un progetto:
1. Introduzione $\rightarrow$ definizione di obiettivi e vincoli
2. Organizzazione $\rightarrow$ definizione persone coinvolte e ruoli
3. Analisi dei rischi
4. Stime di costo e temporali per acquisire le risorse hw e sw richieste
5. Scheduling del progetto $\rightarrow$ identificazione dipendenze tra attivita' e stime tempo richiesto per milestones, assegnazione personale alle attivita'.
6. Controllo e rapporto sulle attivita'.

#### Quali sono le tipologie di team?
Esistono 3 tipologie di team:
1. DEMOCRATICO DECENTRALIZZATO $\rightarrow$ team in cui non c'e' un leader e serve un consenso di gruppo sulle soluzione e sull'organizzazione del lavoro, presenta una comunicazione orizzontale, ossia tra le persone dello stesso livello. E' difficile da imporre e non e' scalabile, ma funziona bene per problemi difficili, come la ricerca e l'individuazione degli errori e' piu' veloce.
2. CONTROLLO CENTRALIZZATO $\rightarrow$ il team leader decide sulle soluzioni e sull'organizzazione, la comunicazione avviene in odo verticale tra i team leader e gli altri membri. Ruoli:
	- Project manager $\rightarrow$ pianifica, coordina e supervisiona le attivita' del team
	- Technical staff, $\rightarrow$ conduce le analisi e sviluppo
	- Backup engineer $\rightarrow$ supporta il project manager ed e' responsabile della validazione
	- Software librarian $\rightarrow$ si occupa di controllare la documentazione, i listati di codice e dati
3. CONTROLLO DECENTRALIZZATO $\rightarrow$ presenta un leader che coordina il lavoro e la risoluzione dei problemi e' di gruppo, ma l'implementazione e' gestita dai leader dei sottogruppi. Presenta quindi una comunicazione verticale con il leader e orizzontale tra i membri dei vari sottogruppi.

### Che cos'e' la WBS (Work Breakdown Structure) - dividi e conquista
Il problema viene risolto tramite la soluzione dei sottosistemi e l'integrazione delle soluzioni. Il WBS definisce quali sono le attivita' da seguire e quali sono i rapporti gerarchici tra le attivita'. Grazie alla decomposizione dell'attivita' di pianificazione in attivita' piu' semplici, i tempi, i costi e gli scopi di realizzazione vengono stimati in modo piu' preciso. 

### Rapporto mese-uomo
Per valutare l'effort di un progetto in funzione del numero dio sviluppatori e' possibile ricorrere al "mese-uomo"Questa misura e' efficace solamente in casi in cui il compito da svolgere e' perfettamente partizionabile tra gli sviluppatori e non richiede comunicazione.
Per questo motivo difficilmente la si puo' utilizzare per valutare il numero di sviluppatori richiesti per un task, infatti l'aggiunta di programmatore richiede che questi vengano adeguatamente formati da altri membri del personale, i quali dovranno rinunciare a parte della propria produttivita'.

### Cosa sono i diagrammi di Gantt e a cosa servono?
Sono uno strumento utilizzabile sia in fase di pianificazione che in fase di monitoraggio. Sono dei diagrammi bidimensionali (un'ase per il tempo e un'asse per le attivita' di progetto), l'origine degli assi e' l'inizio del progetto.
Permettono di avere tutte le informazioni di sintesi sul progetto e il suo andamento, infatti integrano info sui tempi, vincoli di precedenza nell'esecuzione delle attivita', info sull'avanzamento e punti di controllo.

### Cosa sono i diagrammi di Pert e a cosa servono?
I diagrammi di Pert contengono le stesse informazioni dei diagrammi di Gantt, ma si focalizzano sulla durata e sui vincoli di precedenza dell'esecuzione delle attivita' (scopo: pianificazione delle attivita').

### Algoritmi di trasformazione e pratiche di ottimizzazione
Per trasformare  il processo in piano di progetto inserisco nella prima colonna le attivita' che non dipendono da nulla e proseguo all'aumentare  delle dipendenze.
Per passare dal piano di progetto al piano esecutivo si ottimizzano le attivita' che eseguono in parallelo per uomini, tempi e costi: si valuta se renderle sequenziali o lasciarle in parallelo.

### Cosa si intende per piano di progetto e piano esecutivo
- Il piano di progetto pianifica la sequenza potenziale dei compiti da svolgere per completare il processo che tiene conto solamente dei vincoli di precedenza tra i vari tasks, ignorando fattori come la durata di sviluppo e le risorse economiche.
- Il piano esecutivo pianifica la sequenza effettiva delle attivita' elementari che tiene conto dei fattori economici, delle tempistiche di sviluppo e della gestione del personale.

### Analisi cammini critici
Il cammino critico (determina la lunghezza del progetto) e' la successione di attivita' legate allo sviluppo del software per cui un ritardo su una qualsiasi delle attivita' interessate risulta in un ritardo nella consegna.

Per analisi dei cammini critici si intende lo studio della disposizione temporale dei compiti all'interno di un progetto volta ad ottimizzarla per mezzo di una ri-schedulazione dei compiti che tenga conto della disposizione del personale, delle dipendenze tra le attivita' (parallelismo) e dei tempi di consegna. Attraverso l'analisi dei cammin critici si puo' ridurre la durata di sviluppo del progetto.
Uno strumento utile per lo studio dei cammini critici e' il diagramma di Pert. 
E' molto oneroso compiere quest'analisi quando il progetto e' gia' in esecuzione.

---
## Capitolo 5: INGEGNERIA DEI REQUISITI
### Cos'e' l'analisi dei requisiti?
L'analisi dei requisiti risponde alla domanda: che cosa deve fare l'applicativo?
L'analisi dei requisiti e' necessaria per comprendere precisamente cosa si richiede dal sistema, per comunicare questa comprensione agli sviluppatori e per controllare che il sistema soddisfi le specifiche.
L'analisi dei requisiti coinvolge sicuramente lo staff tecnico, il committente e coloro che vengono chiamati gli stakeholders. Gli stakeholders sono tutte delle persone che traggono vantaggio dal sistema sviluppato, per esempio il manager, utenti finali, personale marketing, clienti esterni/interni, consulenti, ingegneri sw, etc.

Generalmente vengono coinvolte 5 figure:
- Consumatori $\rightarrow$ spiegano cosa dev'essere consegnato
- Managers $\rightarrow$ controllano lo scheduling e avanzamento del progetto
- Designers  $\rightarrow$ producono le specifiche di design
- Programmatori $\rightarrow$ preparano liste di implementazioni accettabili/output
- Testers $\rightarrow$ pensano ad una serie di test per la veridica, validazione e valutazione del progtto

### Qual'e' la differenza tra definizione e specifica dei requisiti?
- Definizione dei requisiti: descrizione delle funzionalita' del sistema e dei vincoli operativi, orientata al cliente, in linguaggio naturale. Questo porta a problemi di chiarezza, confusione di requisiti (funzionali e non) e la descrizione di requisiti diversi insieme.
  I requisiti possono essere funzionali (descrivono le funzionalita' che il sistema deve fornire) o non funzionali (vincoli sui servizi e funzionalita' fornite o vincoli sull'ambiebnte e sul processo di sviluppo oppure proprieta' che il sistema non deve avere). I requisiti non funzionali sono suddivisi in:
	- Requisiti di prodotto, quelli che riguardano il comportamento del prodotto finale (per esempio, velocita' di esecuzione o affidabilita')
	- Requisiti organizzativi, sono conseguenza delle politiche organizzative del cliente (gli standard del processo per esempio)
	- Requisiti esterni al sistema e all'ambiente di sviluppo (per esempio quelli legati alla legislatura o all'interoperabilita')
- Specifica dei requisiti: documento strutturato e dettagliato dei servizi che il sistema deve fornire; di solito viene utilizzato come base di contratto tra committente e fornitore del sw. Puo' essere scritta anche con linguaggi di descrizione di programmi, che e' un linguaggio simile a quello di programmazione, ma piu' espressivo. Di solito si utilizza quando un'operazione e' specificata come sequenza di azioni e l'ordine e' quindi importante e quando si devono specificare le interfacce hw e sw.
- Specifica del software: una descrizione dettagliata del sw che serve come base per il design dell'architettura e per l'implementazione. E' quindi rivolta agli sviluppatori e agli architetti di sistema.

### Come sono classificati i requisiti e come ottenerli?
Funzionali e non. Si ottengono tramite delle "interviste" agli stakeholders ed analizzandole cercando di trovarne uno scopo. Esistono varie fasi del processo di analisi requisiti.

### Perche' i requisiti possono essere imprecisi?
Perche' gli stakeholders possono non avere le idee chiare su cosa vogliono, esprimono i requisiti usando il loro linguaggio naturale, ci possono essere requisiti contrastanti e fattori (generalmente politiche interne di gestione) che influenzano i requisiti del sistema.

### Quali sono le fasi di processo dell'ingegneria dei requisiti?
L'ingegneria dei requisiti specifica delle caratteristiche operative del sw, indica un'interfaccia sw con altri elementi del sistema e stabilisce i vincoli a cui il sistema deve sottostare.

Le fasi di processo sono:
1. Studio delle fattibilita': quali sono le necessita' che il sistema puo' soddisfare con tecnologia e risorse disponibili.
2. Analisi dei requisiti:
	- Comprensione dl dominio applicativo $\rightarrow$ avvio del processo
	- Raccolta dei requisiti, presenta 3 problemi principali, ossia problemi di scope, di comprensione e di volatilita'. Tecnica di raccolta piu' utilizzata $\rightarrow$ FAST.
	- Classificazione dei requisiti (funzionali e non)
	- Risoluzione dei conflitti fra questi
	- Assegnazione delle priorita'
	- Validazione dei requisiti, ossia la verifica che siano completi, consistenti e coincidano con i desideri del committente $\rightarrow$ controllo validita', consistenza, completezza, realismo, verificabilita', comprensibilita', tracciabilita' e adattabilita' dei requisiti.
	  Inoltre vengono effettuate delle revisioni dei requisiti periodiche con lo scopo di ottenere un'adeguata comunicazione fra committente, sviluppatore e utenti finali per individuare e risolvere i problemi appena si manifestano.
3. Definizione dei requisiti: descrizione con linguaggio naturale dei servizi + vincoli.
4. Specifica dei requisiti
5. Specifica del software

#### Che cos'e' FAST (Facilitated Application Specification Technique)
E' una tecnica che consiste nella formazione di un team misto di sviluppatori e clienti che collabori ad individuare e specificare il problema, proporre una soluzione, negoziare diverse strategie pe valutare le soluzioni proposte e specificare un insieme preliminare di requisiti (elenco consensuale per oggetti, servizi, vincoli e prestazioni + elenco dei criteri di validazione)

### Cosa si fa in caso di evoluzione dei requisiti e perche' evolvono?
I requisiti evolvono perche' si scopre le vere esigenze del cliente o perche' cambiano gli obiettivi dell'azienda. Esistono requisiti durevoli (derivano dalla natura dell'attivita' del committente) e volatili (possono cambiare durante lo sviluppo o durante l'uso del sistema). Bisogna poter, quindi, identificare, tracciare e controllare i requisiti e i cambiamenti man mano che si prosegue allo sviluppo del progetto. Per far questo ad ogni requisito viene assegnato un identificatore  univoco, si sviluppano delle tabelle di tracciabilita' (indicano le relazioni tra requisito e funzionalita', l'origine di ogni requisito e le dipendenze/relazioni con altri requisiti).

---
## Capitolo 8: ANALISI DEI REQUISITI
### Modello di processo VORD (Viewpoint Oriented Requirement Definition)
Viewpoint $\rightarrow$ attori coinvolti nel sistema (produttori/consumatori di dati, utenti/realizzatori di servizi, ...)

Il modello VORD e' formato da 4 fasi:
1. Individuazione dei viewpoints che ricevono servizi dal sistema e identificazione dei servizi che il sistema deve offrirgli.
2. Strutturazione dei viewpoints in modo gerarchico
3. Documentazione, ossia descrizione dei viewpoints e dei servizi identificati
4. Mapping viewpoint-sistema, ossia la definizione degli oggetti che rappresentano i viewpoint (modello ad oggetti)

---
## Capitolo 9: PROGETTAZIONE

### Cos'e' la progettazione?
La progettazione e' il processo che porta alla definizione ingegneristica di quello che dev'essere realizzato. E' formato da due fasi:
- Diversificazione $\rightarrow$ il progettista acquista il materiale grezzo del progetto per individuare le possibilita' realizzative
- Convergenza $\rightarrow$ il progettista sceglie e combina le componenti per arrivare al prodotto finale

### Quali sono i 3 requisiti progettuali?
1. Il progetto deve soddisfare tutti i requisiti espliciti ed impliciti dettati dal cliente
2. Il progetto dev'essere una guida comprensibile per chi si occupa delle fasi di codifica, collaudo e manutenzione.
3. Deve dare un quadro generale completo e coerente dei domini dei dati e funzionale e comportamentale dell'implementazione

### Quali sono le fasi della progettazione?
1. Comprensione del problema
2. Descrivere astrazioni delle soluzioni $\rightarrow$ l'astrazione e' la descrizione  del sistema in un certo livello trascurando i dettagli dei livelli sottostanti
3. Ripetere lo step per ogni astrazione identificata finche' la progettazione non e' espressa in termini primitivi.

### Quali sono le fasi del design?
- Architectural design $\rightarrow$ identifica e documenta i sottosistemi e le loro relazioni
- Abstract specification $\rightarrow$ specifica i servizi forniti e i vincoli
- Interface design $\rightarrow$ descrivono l'interfaccia dei sottosistemi
- Component design $\rightarrow$ alloca i servizi ai diversi componenti e definisce le interfacce di questi componenti
- Data structure design $\rightarrow$ definire le strutture dati
- Algorithm design $\rightarrow$ specifica degli algoritmi utilizzati

### Quali sono le strategie di modularizzazione?
- Progettazione top-down $\rightarrow$ il problema viene partizionato ricorsivamente in sottosistemi e si inizia a risolvere il problema dalla radice al livello piu' basso
- Progettazione bottom-up $\rightarrow$ composizione di soluzioni
- Progettazione sandwich $\rightarrow$ soluzione naturale

### Come definire una dimensione ottimale dei moduli che minimizza i costi di sviluppo e di integrazione 
5 criteri di Mayer (1988):
1. Scomponibilita' $\rightarrow$ riduzione complessita' grazie alla scomposizione di grandi problemi in sottoproblemi
2. Componibilita' $\rightarrow$ assemblamento di componenti preesistenti per migliorare la produttivita'
3. Comprensibilita' $\rightarrow$ minimizza le interfacce di un modulo con altri $\rightarrow$ facile costruzione e modifica
4. Continuita' $\rightarrow$ modifiche a singoli moduli $\rightarrow$  facile controllo
5. Protezione $\rightarrow$ effetti anomali non si propagano da un modulo ad un altro $\rightarrow$ migliora la manutenibilita'

### Cos'e' l'architttura del sw, quali sono le sue proprieta' e quali modelli vengono utilizzati per presentarla?
L'architettura del sw descrive la struttura gerarchica dei moduli di un programma, come interagiscono e la struttura dei dati che manipolano.
Ha proprieta' strutturali (descrive componenti del sistema e come sono assemblate), extrafunzionali (deve esplicitare il modo in cui vengono soddisfatti i vari requisiti) ed di affinita' (permette il riuso strutture e schemi per progetti simili).

L'architettura puo' essere presentata da modelli:
- strutturali $\rightarrow$ mostra l'architettura come collezione organizzata in componenti
- dinamici $\rightarrow$ mostrano gli aspetti comportamentali dell'architettura
- Schematici $\rightarrow$ vengono utilizzati per il riuso di schemi ricorrenti
- Di process $\rightarrow$ descrivono il processo tecnico o aziendale che il progetto deve supportare
- Funzionali $\rightarrow$ descrivono le funzionalita' di un sistema in modo gerarchico

### Qual'e' la struttura dei dati
La struttura dati definisce l'organizzazione, metodi di accesso, grado di associativita' e alternative di elaborazione delle informazioni. Esistono varie strutture base con il vettore sequenziale o lo spazio n-dimensionale o, ancora, la lista.

### Cos'e' l'information hiding e perche'/quando viene utilizzato?
L'information hiding serve per identificare il minimo costo per la modularita' del sistema. Richiede che ogni modulo sia definito in modo ce le sue procedure ed informazioni non siano accessibili ad altri moduli. L'unico modo di interazione tra moduli deve avvenire tramite interfaccia.

### Quali sono le strategie di progettazione?
Le strategie di progettazione sono quella funzionale, in cui lo stato del sistema e' centralizzato e condiviso tra le funzioni che operano su quello stato e quella orientata agli oggetti, in cui il sistema e' visto come un insieme di oggetti che interagiscono (il sistema e' decentralizzato e ogni oggetto ha il proprio stato; gli oggetti comunicato attraverso i metodi di classe)

### Come si stabilisce la qualita' della progettazione?
Dipende da specifiche priorita' di tipo organizzativo.
Attributi di mantenibilita' di un oggetto:
- Coesione: un modulo esegue un numero di compiti limitato e coerente. Se ci sono cambiamenti al sistema, permette di mantenere il cambiamento locale ad una singola componente.
- Accoppiamento: misura la forza di connessione tra i componenti di un sistema
- Comprensibilita': legata a coesione, naming (nomi significativi), documentazione e complessita'. Elevata complessita' = scarsa comprensibilita'.
- Adattabilita': un progetto e' adattabile quando le sue componenti sono debolmente accoppiate, e' ben documentato, c'e' una corrispondenza stretta tra i livelli di progettazione e per ogni componente c'e' una forte coesione. L'eredita' migliora molto l'adattabilita' perche' le componenti possono essere adattate senza cambiamenti e modifiche.

### Quali sono le tipologie e i livelli di coesione?
- Coesione incidentale (debole) $\rightarrow$ diverse parti di un componente raggruppate insieme ma non correlate
- Coesione temporale (debole) $\rightarrow$ raggruppamento componenti attivate nello stesso istante
- Coesione logica (debole) $\rightarrow$ raggruppamento componenti che svolgono azioni simili
- Coesione procedurale (debole) $\rightarrow$ raggruppate componenti che vengono attivati in sequenza uno dopo l'altro
- Coesione di comunicazione (media) $\rightarrow$ raggruppamento componenti che sullo stesso input effettuano lo stesso output
- Coesione sequenziale (media) $\rightarrow$ l'output di un componente e' l'input di un altro
- Coesione funzionale (forte) $\rightarrow$ ogni parte di componente e'  necessaria solo per una singola funzione
- Coesione d'oggetto (forte)  $\rightarrow$ ogni operazione fornisce funzionalita' per osservare o modificare gli attributi di un oggetto.

### Quali tipi di coupling esistono
- Accoppiamento lasco: quando i cambiamenti di una componente non hanno effetto sulle altre; puo' essere ottenuto decentralizzando gli stati e realizzando la comunicazione tramite passaggio di parametri o di messaggi
- Accoppiamento stretto: avviene quando le variabili sono condivise e c'e' scambio di informazioni di controllo
- Accoppiamento basso: moduli diversi si scambiano parametri semplici, non strutture dati
- Accoppiamento di controllo: un segnale di controllo viene scambiato tra due moduli
- Accoppiamento comune: moduli diversi hanno dati in comune.

### Come si puo' migliorare la modularita' del progetto?
1. Studia la prima versione del progetto e cerca di ridurre accoppiamento e aumentare la coesione
2. Mantenere il campo di azione (insieme dei moduli che dipendono da una decisione presa localmente) di un modulo entro il suo campo di controllo (insieme dei moduli subordinati)
3. Studiare le interfacce per ridurre l'accoppiamento
4. Definire i moduli con funzioni prevedibili
5. Evitare collegamenti patologici (interni) tra moduli.

### Che cos'e' la specifica di progetto?
E' il documento che descrive il progetto finale:
1. Descrizione della portata globale del progetto ricavata dalla specifica dei requisiti
2. Descrizione di stati di progetto, es struttura del db o file esterni.
3. Descrizione dell'architettura con riferimento ai metodi utilizzati per ricavarla e rappresentazione grafica dei moduli.
4. Progetto delle interfacce esterne ed interne e descrizione dettagliata delle interazioni fra utente e sistema con eventuale prototipo
5. Descrizione procedurale dei singoli componenti in linguaggio naturale

---

## Capitolo 10: PROGETTAZIONE ARCHITETTURALE
### Perche' la progettazione architetturale e' importante?
Per 3 motivi:
1. La rappresentazione dell'architettura serve per far comunicare le parti interessate allo sviluppo di sistema
2. L'architettura del sistema mette in evidenza le decisioni progettuali preliminari
3. L'architettura e' un modello conciso e facilmente comprensibile di come il modello e i vari componenti interagiranno fra loro.

### Quali sono i principi di progettazione dei dati?
- Analisi di funzionalita' e comportamenti
- Individuazione delle strutture dati e delle operazioni che ognuna deve svolgere
- Inserire il class-diagram: definisce gli oggetti e le operazioni applicate
- Raffinazione analisi dei requisiti (gerarchia delle decisioni e immissione nelle fasi di progettazione)
- Coupling e information hiding per i moduli
- Individuare possibili modularizzazioni e riutilizzi
- Implementare un adt in un linguaggio di programmazione

### Che cos'e' uno stile architetturale?
E' composto da un insieme di componenti che implementano le funzionalita' richieste, da un insieme di connettori per la comunicazione dei componenti, da un insieme di vincoli per l'interazione fra di essi e da modelli semantici che permettono al progettista di comprendere il funzionamento del sistema sulla base delle sue componenti.

### Quali sono i modelli di struttura di un sistema?
- Repository: il sistema e' centrato su un archivio di dati che puo' essere attivo (quando notifica al client ogni variazione dei dati) o passivo (quando tutte le operazioni sono effettuate dal client). Le componenti accedono al client operando indipendentemente tra loro, quindi si puo' intervenire su un componente senza che gli altri ne risentano. E' molto efficiente per grandi quantita' di dati, ma presenta una bassa scalabilita' e non c'e' spazio per politiche di accesso dati e modificare il sistema nel suo complesso diventa dispendioso.
  
- Pipe and Filter: si tratta di un modello architetturale (tipico delle shell nei sistemi operativi) che favorisce la manipolazione di input codice attraverso filtri. E' un modello semplice ed efficace per gestire computazioni complesse attraverso la concatenazione di numerosi filtri semplici, tuttavia e' poco adatto quando la struttura non e' facilmente "linearizzabile".
  
- Client-Server: il modello client-server si basa sulla richiesta di servizi da parte del client nei confronti del server.
  Uno dei modelli principali e' il three-tier architecture (di cui fanno parte i framework MVC), il quale si compone di tre livelli:
	- Presentation layer: si tratta dell'interfaccia grafica attraverso la quale l'utente interagisce con il sistema.
	- Application processing layer: si occupa dell'interfaccia grafica attraverso la quale l'utente interagisce con il sistema
	- Data management layer: si occupa della comunicazione con il DBMS
	I vantaggi di questo modello risiedono nella sua semplicita' ed efficacia in fatto di scalabilita' e contenimento dei costi; gli svantaggi sono la necessita' di replicare alcune componenti di gestione dei dati tra DBMS e il Data management layer e la mancanza di un modello unico e condiviso puo' comportare un calo di prestazioni

- Macchina astratta: organizza il sistema in un insieme di strati, ognuno dei quali svolge un insieme di servizi; e' usato per modellare l'interfaccia tra sottosistemi e se sei cambia l'interfaccia di un livello, solo quello adiacente ne risente, ma si possono incontrare restrizioni per l'interazione tra livelli interni ed esterni.

### Qual'e' la differenza tra sottosistema e modulo?
- Il sottosistema e' un sistema di diritto formato da moduli e interfacce ben definite per comunicare con altri sottosistemi
- Il modulo e' una componente del sottosistema e fornisce uno o piu' servizi ad altri moduli. Non e' indipendente.

### Cosa si intende con scomposizione modulare, all'interno della fase di progettazione?
Per scomposizione modulare si intende un raffinamento a livello strutturale per cui i sottosistemi sono scomposti in moduli: cio' che comporta una riduzione del tempo di sviluppo, ma richiede che il sistema sia effettivamente partizionabile.

### Quali sono i modelli di scomposizione modulare?
- Il modello ad oggetti, dove il sistema e' scomposto in oggetti che interagiscono 
- modelli data-flow, dove il sistema e' scomposto in modelli funzionali che trasformano input in output (pipeline)

### Quali sono i modelli di controllo?
Descrivono il modo in cui fluisce il controllo tra i sottosistemi:
- Modello centralizzato $\rightarrow$ esiste un sottosistema che ha il controllo globale e che avvia e disattiva i sottosistemi
- Modello call-return $\rightarrow$ gerarchia di procedure, ossia il controllo da parte della routine iniziale e in base alla gerarchia si sposta verso il basso, utilizzabile su sistemi sequenziali.
- Modello manager $\rightarrow$ un componente del sistema controlla l'avvio, l'interruzione e il coordinamento degli altri processi.
- Modello basato su eventi $\rightarrow$ dove ogni sistema puo' intervenire ad eventi generati da altri sottosistemi o dall'ambiente esterno
- Modello broadcast $\rightarrow$ un evento viene trasmesso a tutti i sottosistemi e ogni sistema in grado di gestire l'evento puo' farlo 
- Modello interrupt-driven $\rightarrow$ le interruzioni sono raccolte da un gestore che le pasa al componente in grado di processarle

---
## Capitolo 11: PROGETTAZIONE OO

### Che cos'e' la progettazione orientata agli oggetti?
E' rappresentata come un insieme di oggetti che interagiscono.
- Classe $\rightarrow$ caratteristiche comuni di entita' che hanno uno stato e un insieme di operazioni che operano sullo stato. Un oggetto e' un istanza di una classe.
- Attributi $\rightarrow$ rappresentano lo stato di un oggetto
- Metodi $\rightarrow$ operazioni definite sull'oggetto che offrono servizi ad altri oggetti
- Incapsulamento
- Ereditarieta'
- Specializzazione
- Polimorfismo

### Metodo OOD e caratteristiche
Esistono 3 fasi di sviluppo  orientato agli oggetti:
- Fase dell'analisi, che riguarda lo sviluppo del dominio di applicazione
- Fase di progettazione, che riguarda la progettazione del modello orientato agli oggetti che deve soddisfare i requisiti richiesti
- Fase di programmazione, che riguarda la realizzazione del progetto attraverso uno specifico linguaggio ad oggetti

Noi ci troviamo nella fase di progettazione, che indica gli oggetti che sono derivati da ogni classe, come interagiscono fra loro (gerarchia di ereditarieta' e aggregazioni), come si implementa il comportamento e come viene implementata la comunicazione tra oggetti.

Gli oggetti sono astrazioni di entita' del mondo reale, indipendenti, che contengono informazioni sul proprio stato e sulla rappresentazione dei propri dati. La comunicazione non avviene con memoria condivisa, ma gli oggetti comunicano tra loro tramite i metodi e passando parametri. Essi possono essere distribuiti ed eseguiti sia in sequenza che in parallelo.

### Livelli di progettazione
- Progettare i sottosistemi
- Progettare le classi
- Progettare le comunicazioni tra oggetti
- Progettare le strutture dati e l'implementazione per gli attributi e i metodi di ciascun oggetto 

### Identificazione degli oggetti
Esistono vari approcci per l'identificazione degli oggetti come quello grammaticale:
- I nomi corrispondono ad oggetti del sistema
- I verbi corrispondono ad operazioni associate ad oggetti
- I predicati corrispondono ad operazioni che restituiscono valori di tipo booleano

Esistono anche altri approcci come quello comportamentale (identificazione degli oggetti per il modo in cui si comportano) oppure utilizzare il dominio di applicazione ed individuarne le entita'.

### Tipi di oggetti
- Entita' interne $\rightarrow$ producono o consumano informazioni usate dal sistema
- Entita' esterne $\rightarrow$ parte del dominio informativo del sistema
- Eventi $\rightarrow$ occorrenti nelle operazioni del sistema
- Ruoli $\rightarrow$ persone che interagiscono con il sistema
- Luoghi $\rightarrow$ stabiliscono il contesto del sistema
- Strutture $\rightarrow$ costruite dai sistemi di entita' correlate

---
## Capitolo 12: PROGETTAZIONE INTERFACCIA UTENTE
### Principi della progettazione UI
Le tre regole sono:
1. Il controllo deve essere nelle mani dell'utente $\rightarrow$ l'utente deve avere un'interazione flessibile e quindi avere sempre la possibilita' di interrompere o annullare le operazioni in corso; cio' non significa che all'utente debbano essere fornite informazioni di carattere tecnico 
2. L'utente deve ricorrere il meno possibile all'uso della memoria
3. L'interfaccia deve essere uniforme: il sistema deve avere un aspetto grafico consistente in ogni suo elemento; e' inoltre opportuno che si rispettino i canoni stilistici e di usabilita' assodati, cosi' come e' importante che si mantengano modelli interattivi preesistenti

#### Limiti del ricorso alla memoria
Affiche' si possa raggiungere questo scopo, e' importante che l'interfaccia grafica a cui e' sottoposto l'utente sia di facile lettura e conforme a standard assodati in termini di estetica e usabilita', in maniera tale che non si tratti di un processo di apprendimento, bensi' di riconoscimento. Inoltre l'UI dovrebbe sempre fornire all'utente informazioni circa le operazioni svolte in precedenza e in maniera progressiva. Un'altra ragione per ridurre la mode di informazioni che l'utente deve ricordare e' che ad una maggiore memorizzazione corrisponde una maggiore propensione a commettere errori nelle interazioni di sistema.

### Modelli di utente
Esistono 3 tipi di utente:
1. Principiante $\rightarrow$ nessuna conoscenza sintattica (uso dell'interfaccia) e scarse conoscenze semantiche dell'uso del sistema (comprensione delle funzionalita')
2. Casuale $\rightarrow$ ragionevole conoscenza semantica dell'applicazione, mentre la conoscenza sintattica e' limitata
3. Costante $\rightarrow$ buona conoscenza semantica e sintattica

### Immagine e percezione
L'immagine del sistema comprende  sia l'aspetto e il comportamento dell'interfaccia utente sia tutte le informazioni di supporto che descrivono sintassi e semantica del sistema. La percezione del sistema, invece, e' l'immagine mentale che si forma nella mente dell'utente finale.
Generalmente si cerca di fare in modo che coincidano per soddisfare

### Processo della progettazione UI
Il processo di progettazione e analisi e' iterativo e segue un modello di sviluppo a spirale in cui troviamo 4 fasi:
1. Analisi e modellazione degli utenti, operazione e dell'ambiente: dobbiamo comprendere quali sono il tipo di utenti coinvolti, la loro disponibilita' nell'accettare il sistema, per ogni utente bisognera', poi, individuare i requisiti dell'interfaccia e l'ambiente in cui verra' usata l'interfaccia, sia a livello fisico, sia a livello comportamentale.
   Varie fasi:
	- Utilizzo di use case nell'analisi per mostrare come l'utente svolge operazioni specifiche; da questi use case sono estratte le operazioni, gli oggetti e il flusso generale delle interazioni.
	- Elaborazione delle operazioni tramite traduzioni di attivita' manuali oppure tramite lo studio delle specifiche di una soluzione computerizzata; generalmente per fare cio' si parte dallo use case e si estraggono gli oggetti per catalogarli in classi e definire ogni attributo di classe dalle azioni che si fanno sugli oggetti.
	- Analisi del workflow (sequenza di operazioni di lavoro), che viene effettuata se puo' utenti hanno ruoli differenti, per chiarirli tramite gli activity diagram.
	- Rappresentazione  gerarchica, per associare le operazioni ad ogni ruolo.
2. Progettazione dell'interfaccia
3. Costruzione o implementazione  dell'interfaccia
4. Validazione dell'interfaccia

### Design UI e problemi relativi
1. Il tempo di risposta dev'essere ne' troppo lungo ne' troppo corto e dovrebbe esserci una bassa variabilita'
2. Il sistema di help dell'utente non e' disponibile per ogni funzione e in ogni fase dell'interazione
3. I messaggi di errore dovrebbero descrivere il problema in un linguaggio comprensibile all'utente, fornire info per risolvere il problema e sottolineare quali effetti negativi ha portato/puo' portare l'errore
4. Poche interfacce inseriscono l'uso della riga di comando, che viene solitamente richiesta da un utente esperto
5. L'accessibilita' e' sempre piu' richiesta
6. Spesso le interfacce sono pensate e progettate in una specifica lingua e poi adattate a tutte le altre, questo provoca un problema di internazionalizzazione. 

### Strumenti di implementazione
Sono strumenti che permettono la creazione di un prototipo di interfaccia

### Valutazione del prototipo
Esistono vari metodi per valutare un prototipo, quali il metodo informale, in cui l'utente usa il sistema e offre le proprie considerazioni e quello formale, in cui gruppi di utenti usano il sistema e vengono utilizzati sistemi statistici per valutare le loro opinioni.
Abbiamo vari indici di valutazione:
- Difficolta' di apprendimento degli utenti $\rightarrow$ specifiche dei requisiti molto lunghe
- Tempo di interazione e efficienza  $\rightarrow$ il numero di operazioni degli utenti e il numero di azioni per ogni operazione fatta dall'utente
- Quanto l'utente ricorre alla memoria e' proporzionale al numero di azioni e operazioni effettuato
- La complessita' dell'UI e del livello di accettazione da parte degli utenti dal modo di gestione degli errori e dall'utilizo dell'help

---
## Capitolo 13: VERIFICA E VALIDAZIONE
- Per validazione si intende il processo che risponde alla domanda "si sta costruendo il progetto giusto?", ossia si chiede se si sta implementando il prodotto in modo da soddisfare le richieste dei clienti. La validazione puo' essere solo dinamica, in quanto non c'e' la necessita' di un prototipo per eseguire dei test di prova e osservare il comportamento.

- La verifica, invece, risponde alla domanda "si sta costruendo il progetto nel modo giusto?", ossia si chiede se il prodotto e' conforme alle specifiche imposte. Essa puo' essere statica, si analizza la rappresentazione statica del sistema per individuare problemi o dinamica, ossia si testa il prodotto con varie prove per vedere come risponde.

### Principi di testing
1. Ogni singola prova dev'essere ricondivibile ai requisiti del cliente
2. I collaudi vanno pianificati con largo anticipo: possono iniziare dopo la specifica dei requisiti (statici) e dopo la prototipazione (dinamici)
3. Principio di pareto: 80% degli errori e' riconducibile al 20% dei componenti del programma, quindi serve isolare i componenti  sospetti
4. I collaudi vengono effettuati prima sulle componenti e poi sull'intero sistema
5. E' impossibile effettuare collaudi esaurienti perche' sono troppe tutte le possibili esecuzioni del programma
6. Il collaudo andrebbe effettuato da una persona esterna a chi scrive il codice.

### Cos'e' la collaudabilita'?
La facilita' con cui un test puo' essere provato. Dipende
-  dall'operabilita', ossia da pochi errori che non impediscono l'esecuzione del collaudo
- dall'osservabilita', ossia da input e output correlati e chiari, con visualizzazione degli stati e delle variabili durante l'esecuzione del collaudo e quando gli errori vengono subito identificati
- controllabilita', tutto l'output puo' essere generalizzato ad un opportuno input
- scomponibilita', ossia i moduli, essendo indipendenti, possono essere collaudati singolarmente
- semplicita' funzionale (funzionalita' minime ed essenziali), strutturale (architettura scomponibile in moduli) e del codice (standard unico di codifica)
- stabilita', meno modifiche si effettuano, minor volte devono essere eseguiti i collaudi
- comprensibilita' (chiarezza del progetto, dipendenze ben descritte e documenti e modifiche dettagliate e pubbliche)

Esistono due tipi di collaudo:
- WHITE-BOX $\rightarrow$ si intende il collaudo del software di cui si conosce il codice e per cui si controllano tutti i vari cammini indipendenti possibili interni ai moduli. Questo tipo di collaudo e' estensivo ed e' ad opera degli sviluppatori. Si studiano gli aspetti procedurali del programma piuttosto che mancanze di requisiti.
- BACL-BOX $\rightarrow$ si intende il collaudo del software di cui non si conosce il codice, e dunque si valuta la conformita' delle specifiche indicate nella specifica dei requisiti (disinteressandosi quindi della struttura interna). Si ricercano funzioni errate o mancanti, errori di interfaccia, nelle strutture dati o comportamenti erronei o problemi prestazionali, oppure errori nelle fasi di inizializzazione e terminazione. E' complementare al collaudo white-box.

### Che cos'e' la complessita' ciclomatica?
E' la misura quantitativa della complessita' logica di un sistema, ossia il numero di cammini indipendenti in un grafo.

### Qual'e' la differenza fra testing e debugging?
Il testing viene utilizzato per confermare la presenza di errori, mentre il debugging viene utilizzato per localizzare e risolvere gli errori identificati.

Le diverse strategie di testing sono:
- TOP-DOWN $\rightarrow$ si testa il modulo radice sostituendo quelli di livello inferiore con stub (versione semplificata dei moduli dei livelli inferiori) e man mano si includono i moduli superiori testati.
- BOTTOM-UP $\rightarrow$  si parte  dal livello piu' basso sviluppando dei driver che usano e verificano il comportamento del sw fino alla radice, aggiungendo livelli (appropriata per il linguaggio ad oggetti)
- COLLAUDO PER REGRESSIONE (incrementale) $\rightarrow$ ad ogni introduzione di modulo vengono rieffettuate tutte le prove di testing per verificare che le modifiche/introduzioni di nuovi moduli/correzioni di bug non abbiano portato effetti collaterali
- STRESS TESTING $\rightarrow$ testa le prestazioni con carichi molto alti
- BACK-TOBACK TESTING $\rightarrow$ testa le varie versioni del programma con lo stesso input e confronta vari output, sono diversi ci sono errori potenziali. Permette di ridurre i costi di esaminare il risultato per l'automazione dei testi di confronto. Si usa quando c'e' una nuova versione di un sistema oppure quando un sistema viene sviluppato in piu' versioni.

### Perche' e' difficile trovare le cause di un errore?
- Per la distanza tra la causa e il punto in cui si presenta l'errore
- Non c'e' una causa specifica
- Puo' dipendere da un errore umano
- Puo' dipendere da problemi di tempo e non di elaborazione
- L'intermittenza di errore
- La dipendenza da cause distribuite su piu' processi

### Revisione vs walkthrough
- La revisione del progetto consiste in riunioni in cui sis riesamina il codice a seguito dello sviluppo del processo, ricorrendo alla compilazione di checklist di errori comuni
- Il walkthrough, che e' un processo meno rapido rispetto alla revisione, consiste in una lettura critica del codice con l'intento di simularne l'esecuzione; si tratta di un processo collaborativo che si basa sull'esperienza degli sviluppatori.
---
## Capitolo 14: AFFIDABILITA'
Per affidabilità si intende la probabilità che il sistema funzioni senza errori per un dato intervallo di tempo, in un dato ambiente e per un determinato scopo. È una proprietà molto importante di ogni sistema, ma è molto difficile e costoso ottenere un’affidabilità tale da garantire la correttezza di ogni output per ogni possibile input, pertanto è più importante nella maggior parte dei casi diminuire il più possibile la possibilità che si verifichino fallimenti gravi piuttosto e implementare funzioni di recupero da fallimenti transienti(gli errori si verificano solo per determinati input) piuttosto che applicare una politica di fault avoidance(evitare guasti). Inoltre, molto spesso gli sforzi per l’aumento dell’affidabilità comportano un costo in prestazioni ed efficienza non indifferenti, senza avere la garanzia di non aggiungere errori non previsti.

Esistono numerosi metodi per misurare l'affidabilita':
- POFOD $\rightarrow$ misura la probabilita' che si verifichi un fallimento a seguito di un input
- ROCOF (Tasso di occorrenza dei fallimenti) e MTTF $\rightarrow$ misura il tempo medio tra fallimenti
- AVAIL $\rightarrow$ misura il tempo necessario per ripristinare l'operativita' del sistema a seguito di un fallimento

Alcuni passi per migliorare l'affidabilita' del prodotto finito e' applicabile  nelle fasi di progettazione e di sviluppo sani principi di buona programmazione strutturata, per cui evitare goto e fare un uso ragionato dell'ereditarieta' e dell'allocazione di memoria 

---
## DESIGN PATTERN
Sono linee guida utilizzate per l'implementazione di framework e di librerie di codice

### Che cosa sono e quali sono i vantaggi e svantaggi?
I design pattern sono soluzioni consolidate (convalidata dal suo utilizzo con successo in piu' progetti) e accettate per un problema ricorrente in un determinato contesto.

Possono essere strutturali (si concentrano come classi e oggetti e si combinano per formare strutture piu' grandi), comportamentali (identificano metodi di comunicazione tra oggetti e li realizzano) o creazionali (astraggono il processo di creazione di oggetti, riducono l'accoppiamento e aumentano la flessibilita' e nascondono la conoscenza dei dettagli di creazione e composizione degli oggetti)

La progettazione di software che segue design pattern favorisce il "concept reuse"  e permette la scrittura di codice chiuso alle modifiche e aperto alle estensioni

Vantaggi:
- Permettono il riuso di architetture sw su larga scala
- Aiutano a documentare e comprendere il sistema

Svantaggi:
- I pattern non portano al riuso diretto del codice
- I pattern possono essere ingannevolmente semplici
- C'e' la possibilita' di utilizzarne troppi
- Non sono validati da un testing esaustivo, ma dall'esperienza e dal confronto
- L'integrazione dei pattern in un processo di sviluppo dio un sw richiede un'intensa attivita' umana.