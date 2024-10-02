## Perche' modellare
Modello == semplificazione della realta'

I modelli:
- visualizzano com'e' fatto un sistema, o come vorremmo che fosse
- specificano la struttura o il comportamento di un sistema
- sono una guida nello sviluppo di un sistema
- documentano le decisioni prese

Possiamo modellare sia un sistema esistente che un nuovo sistema
- i **modelli di un sistema esistente** si usano per chiarire cosa fa il sistema (documentazione)
- i **modelli di un nuovo sistema** si usano per descrivere il sistema proposto agli altri stakeholder, come documentazione

Tramite vari modelli si puo' rappresentare il sistema da diverse prospettive:
- **esterna** $\rightarrow$ contesto o ambiente in cui opera il sistema
- **interazione tra i componenti**
- **strutturale** $\rightarrow$ del sistema o dei dati
- **comportamentale**

Ogni modello puo' essere sviluppato da diversi livelli di precisione

### Che modello mi serve?
- Se modello per facilitare la discussione su un sistema proposto o esistente
	- i modelli possono essere incompleti
- Se sto documentando un sistema proposto o esistente
	- i modelli devono essere corretti e descrivere accuratamente il sistema, ma non e' necessario che siano completi
- Se voglio fornire una descrizione dettagliata del sistema da utilizzarsi per sviluppare l'implementazione
	- i modelli devono essere sia corretti che completi

## UML
- e' un linguaggio $\rightarrow$ serve a specificare, costruire, visualizzare e documentare i componenti di un sistema
- e' universale e percio' puo' applicarsi a sistemi molto diversi (dal Cobol a OO al web) 

### UML: non e' un metodo
- e' un linguaggio di modellazione $\rightarrow$ non e' un metodo ne una metodologia
- definisce una notazione standard $\rightarrow$ introduce un meta-modello integrato dei componenti che formano un sistema software
- non prescrive cosa bisogna fare

### UML e processo software 
UML assume un processo
- basato su "casi d'uso" (use case driven)
- incentrato sull'architettura
- iterativo e incrementale

I dettagli di questa procedura di tipo generale vanno adattati al dominio applicativo di ciascuna organizzazione

### Struttura di UML
- **Costituenti fondamentali** $\rightarrow$ gli elementi, le relazione e i diagrammi di base
- **Meccanismi comuni** $\rightarrow$ tecniche comuni per raggiungere specifici obiettivi con UML
- **Architettura** $\rightarrow$ il modo con cui UML esprime l'architettura del sistema

#### Costituenti fondamentali
- **Entita' strutturali** $\rightarrow$ strutturali (classe, interfaccia), comportamentali  (interazione, stati), di raggruppamento (package) e informative (annotazione)
- **Relazioni** $\rightarrow$ collegano tra di loro le entita', rappresentano associazione, dipendenza, generalizzazione, realizzazione
- **Diagrammi** $\rightarrow$ rappresentano grafiche parziali di un modello UML

#### Meccanismi comuni
- **Specifiche** $\rightarrow$ descrizione testuale della semantica di un elemento (classe, relazione, ...)
- **Ornamenti** $\rightarrow$ informazioni aggiunte di un diagramma ad un elemento per illustrare un concetto supplementare
- **Distinzioni comuni** $\rightarrow$ per distinguere tra astrazioni e le loro istanze
- **Meccanismi di estendibilita'** $\rightarrow$ permettono di estendere il linguaggio per esigenze specifiche. Tre tipi:
	1. vincoli
	2. stereotipi
	3. valori etichettati

#### Architettura
Viene definita da piu' viste. "Vista" non e' definita in modo formale, l'approccio piu' famoso e' il "modello 4+1"
- **Visita logica** $\rightarrow$ mostra le astrazioni chiave nel sistema come oggetti o classi di oggetti (diagrammi di classe, di stato, di oggetti)
- **Vista dei processi** $\rightarrow$ descrive il comportamento dinamico dei diagrammi (diagrammi di stato, di oggetti)
- **Visita di implementazione** $\rightarrow$ descrive la struttura concreta del software che compone il sistema
- **Visita di deployment** $\rightarrow$ mostra come il software viene suddiviso per lo sviluppo
- **Vista dei casi d'uso (+1)** $\rightarrow$ descrive i requisiti in termini di servizi offerti

## Diagramma - casi d'uso
Mostra:
- modalita' utilizzo del sistema (casi d'uso)
- gli utilizzatori e chi interagisce col sistema (attori)
- relazioni tra attori e casi d'uso

Caso d'uso:
- rappresenta un possibile modo di utilizzare il sistema
- descrive l'interazione, non la logica interna del sistema

### Perche' fare dei casi d'uso
E' la prima fase nel ciclo di vita del software in cui sviluppiamo una rappresentazione del software
1. **Esplicitare e comunicare a tutti gli stakeholder i requisiti funzionali del sistema**
2. **Validazione requisiti utente**

## Use Case
- Uno "use case" rappresenta chi (attore) fa cosa (interazione) con il sistema, e con quale scopo (goal), senza considerare l'internod del sistema
- Un use case:
	- rappresenta un singolo, discreto, completo, significativo e ben definito task di interesse per un attore
	- e' un pattern di comportamento tra alcuni attori e il sistema
	- e' scritto usando il vocabolario del dominio
	- definisce scopo e intento (non le azioni concrete)
	- e' generale e indipendente dalla tecnologia

### Use Case - Requisiti
- un caso d'uso puo' soddisfare piu' requisiti
- un requisito puo' generare piu' casi d'uso
- ad un caso d'uso possono essere associati piu' requisiti
- a cosa serve?
	- chiarire i requisiti del committente in modo comprensibile
	- trovare aspetti comuni (riuso)
	- individuare gli attori del sistema
	- individuare gli eventi a cui il sistema deve rispondere

### Use Case - descrizioni 
Quattro concetti fondamentali:
1. **Caso d'uso** $\rightarrow$ rappresenta una specifica interazione tra un attore e il sistema (in UML viene rappresentato con un ellissi etichettata con il nome del caso d'uso)
2. **Attore** $\rightarrow$ rappresenta un ruolo che caratterizza le interazioni tra utente e sistema (in UML viene rappresentato da un omino stilizzato)
3. **Scenario** $\rightarrow$ sequenza di azioni che definisce una particolare interazione tra attore e sistema
4. **Descrizione** $\rightarrow$ testo che descrive lo scenario, l'ordine temporale delle azioni, l'eventuale gestione degli errori, gli attori coinvolti

### Attore
- Un attore e' rappresentato mediante il ruolo che ricopre nel caso d'uso
- E' possibile definire delle gerarchie tra attori
- Associare uno o piu' attori a un attore specializzato (specializzazione)

### Use Case diagram
![[UseCaseDiagram.png]]

### Esempio di documentazione di un Use Case 
- **Nome del caso d'uso** $\rightarrow$ ogni use case deve avere un nome; il nome esprime il goal dell'utente
- **Goal** $\rightarrow$ descrizione sommaria della funzionalita' fornita
- **Attori** $\rightarrow$ persona, dispositivo o altro, esterno al sistema che interagisce col sistema. In ogni use case ci deve essere un attore primario, ci inizia il caso d'uso stesso 
- **Pre-condizioni** $\rightarrow$ condizioni che devono essere soddisfatte all'inizio del caso d'uso. "Garanzie minime" che devono essere soddisfatte per poter arrivare lo scenario in questione 
- **Trigger** $\rightarrow$ evento che attiva il caso d'uso
- **Descrizione (scenario principale)** $\rightarrow$ descrizione della sequenza di interazioni piu' comune tra gli attori e il sistema. In particolare la sequenza che porta alla conclusione con successo. Il sistema viene trattato come una black-box, importa la risposta all'imput, non come viene generata la risposta
- **Alternative (estensioni)** $\rightarrow$ variazione della sequenza dello scenario principale. Un'alternativa non e' necessariamente un fallimento dello use case
- **Post-condizioni** $\rightarrow$ condizioni sempre soddisfatte alla fine del caso d'uso

### Relazione tra Use e Case
- Relazione di inclusione `include`
- Relazione di generalizzazione
- Relazione di estensione `extend`
- poche relazioni: modello semplice

#### Relazioni tra Use e Case: inclusione
- rappresenta l'invocazione di un caso d'uso da parte di un altro
- simile alla chiamata a funzione
- serve per scomporre un caso complesso in comportamenti piu' semplici
- facilita il riuso dei singoli casi d'uso
- puo' anche mostrare un comportamento comune a piu' casi d'uso

![[include.png]]

#### Relazioni tra Use Case: generalizzazione
- Simile all'ereditarieta' tra classi
- Relazione che lega un caso generico a casi d'uso che sono particolari specializzazioni (realizzazioni) del primo
- Logica del caso derivato simile a (ma diversa da) quella del caso d'uso generico
- Viene riscritta la sequenza delle azioni di base oppure viene elaborata una sequenza alternativa
"Effettuare un ordine" e' un caso generale e gli altri effettua sono specializzazioni

![[generalizzazione.png]]

#### Relazione tra Use Case: estensione
- Rappresenta l'estensione della logica di base di un caso d'uso
- Il caso d'uso estensione continua il comportamento del caso d'uso di base inserendo delle azioni alternative da un certo punto in poi (punto di estensione)
- Il caso d'uso di base dichiara tuti i possibili punti di estensione
- Simile alla gestione degli interrupt (gestione delle eccezioni)

![[estensione.png]]

### Diagramma delle classi
- e' il caposaldo dell'object oriented 
- rappresenta le classi di oggetti del sistema, con i loro attributi e operazioni
- mostra le relazioni tra le classi (associazioni, aggregazioni e gerarchie di specializzazione/generalizzazione)
- puo' essere utilizzato in diversi livelli di dettaglio (in analisi e in progettazione)

#### Class diagram: le classi
- Una classe e' una tipologia di oggetti, con propri attributi e operazioni
- Rappresentazione di una classe in UML
![[ClasseUML.png]]

- **Nome** $\rightarrow$ inizia con la lettera maiuscola, non e; sottolineato e non contiene underscore
- **Attributi** $\rightarrow$ proprieta' i cui valori identificano un oggetto istanza della classe e ne costituiscono lo stato; iniziano con una lettera maiuscola
- **Operazioni** $\rightarrow$ insieme di funzionalita'; che esprimono il comportamento di n oggetto, cioe' che ogni oggetto di questa classe puo' fare
  
- **Visibilità** $\rightarrow$ riguarda attributi e operazioni, puo' essere private (-), protected (#), public (+), package (~)
- **Molteplicità** $\rightarrow$ si indica con la notazione degli array `[n]` 
- **Sottolineatura** $\rightarrow$ indica che l'attributo o l'operazione e' static
- **Parametri delle operazioni** $\rightarrow$ possono essere preceduti da modificatore che indica "direzione";"in" parametro in ingresso (passaggio per valore), "out" parametro in uscita (passaggio per riferimento), "inout" parametro di ingresso e uscita (passaggio per riferimento)
- **Corsivo** $\rightarrow$ indica che la classe o l'operazione e' astratta
- **Interfacce** $\rightarrow$ stereotipo `<<interfacce>>`

#### Class diagram: relazioni
- in UML una relazione e' *una connessione semanticamente significativa tra elementi di modellazione*
- Nei diagrammi delle classi abbiamo tre tipi di relazioni:
	1. **Dipendenze** $\rightarrow$ relazioni d'uso
	2. **Associazioni** $\rightarrow$ associazione semplice, aggregazione, composizione
	3. **Generalizzazioni e realizzazioni** $\rightarrow$ ereditarieta' tra classi e implementazione di interfacce

##### Class diagram: dipendenze
- una dipendenza e' una relazione tra due elementi dove un cambiamento in uno di essi (fornitore) puo' influenzare o fornire informazioni necessarie all'altro (cliente)
- Es: rapporto client-server tra due class o fra una classe e un'interfaccia

##### Class diagram: associazioni
- un'associazione e' una correlazione tra classi; nel diagramma e' una linea continua fra due classi caratterizzata da un nome, nomi dei ruoli, molteplicita' e navigabilita'
- nomi e ruoli sono opzionali

##### Class diagram: aggregazione
- E' un tipo particolare di associazione; esprime il concetto "e' parte di" (part of), che si ha quando un insieme e' relazionato con le sue parti.
- Si rappresenta con un diamante dalla parte della classe che e' il contenitore

##### Class diagram: composizione
- E' un caso particolare dell'aggregazione
- La parte *componente* non puo' esistere da sola senza la classe *composto*
- una componente appartiene ad un solo composto

![[EsAggregazioneComposizione.png]]

#### Class diagram: ereditarieta'
![[CDEreditarieta.png]]

### Diagramma di sequenza
- Si usa per definire la logica di uno scenario (specifica sequenza di eventi) di un caso d'uso
- E' uno dei principali input per l'implementazione dello scenario
- Mostra gli oggetti coinvolti specificando la sequenza temporale dei messaggi che gli oggetti si scambiano
- E' un diagramma di interazione $\rightarrow$ evidenzia come un caso d'uso e' realizzato tramite la collaborazione di un insieme di oggetti

- E un diagramma che descrive interazioni tra oggetti che collaborano per svolgere un compito
- gli oggetti collaborano scambiandosi messaggi
- lo scambio di un messaggio in programmazione ad oggetti equivale all'invocazione di un metodo

### Scambio di messaggi sincroni
- Si disegna con una freccia chiusa $\rightarrow$ da chiamante a chiamato. La freccia e' etichettata col nome del metodo invocato e opzionalmente con i parametri e il valore di ritorno
- il chiamante attende la terminazione del metodo chiamato prima di proseguire (chiamata bloccante)
- il life-time di un metodo e' rappresentato da un rettangolino che collega la freccia di invocazione e freccia di ritorno
- il messaggio di ritorno e' rappresentato da una freccia tratteggiata
- il ritorno e sempre opzionale: se si omette, la fine del metodo e' decretata dalla fine del life-time

### Esecuzione condizionale di un messaggio
- L'esecuzione di un metodo puo' dipendere da una condizione $\rightarrow$ il metodo viene invocato solo se la condizione risulta vera a run-time
- Si disegna aggiungendo la condizione, racchiusa tra parentesi quadre, che definisce quando viene eseguito il metodo
- Es $\rightarrow$ `[cond]:nomeMetodo()`

### Interazione di un messaggio
- Rappresenta l'esecuzione ciclica di messaggi
- Si rappresenta aggiungendo un * (asterisco) prima del metodo su cui si vuole iterare
- Si puo' aggiungere la condizione che definisce l'interazione
- La condizione si rappresenta tra parentesi quadre
- Es $\rightarrow$ `[cond]:*nomeMetodo()`

### Iterazione di un blocco di messaggi
- Rappresenta l'esecuzione ciclica di piu' messaggi
- Si disegna raggruppando con un riquadro (blocco, box) i messaggi (metodi) su cui si vuole iterare
- Si puo' aggiungere la condizione che definisce l'iterazione sull'angolo in alto a sinistra del blocco
- La condizione si rappresenta al solito tra parentesi quadre

### Auto-Chiamata (Self-Call)
- Descrive un oggetto che invoca un suo metodo (chiamante e chiamato coincidono)
- Si rappresenta con una "freccia circolare" che rimane all'interno del life time di uno stesso metodo

![[SelfCall.png]]

### Costruzione di un oggetto
- Rappresenta la creazione di un nuovo oggetto non presente nel sistema fino a quel momento
- Messaggio etichettato new, create, ...
- L'oggetto viene collocato nell'asse temporale in corrispondenza dell'invocazione nel metodo new (o create...)
![[costruzioneOggetto.png]]

### Eliminazione di un oggetto
- Rappresenta la distruzione di un oggetto presente nel sistema fino a quel momento
- Si rappresenta con un X posta in corrispondenza della life-line dell'oggetto
- Da quel momento in avanti non e' legale invocare alcun metodo dell'oggetto distrutto
![[EliminazioneOggetto.png]]