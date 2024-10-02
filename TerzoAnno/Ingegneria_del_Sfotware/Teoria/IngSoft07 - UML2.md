## Diagramma di collaborazione
- e' un diagramma di interazione $\rightarrow$ rappresenta un insieme di oggetti che collaborano per realizzare il comportamento di uno scenario di un caso d'uso
- a differenza del diagramma di sequenza, mostra legami (link) tra gli oggetti che si scambiano messaggi, mentre la sequenza di tali messaggi e' meno evidente
- puo' essere utilizzato in fasi diverse (analisi, disegno di dettaglio)

![[DiagrammaCollaborazione.png]]

## Diagramma transizioni di stato
- serve a modellare il ciclo di vita degli oggetti di una singola classe
- mostra gli eventi che causano la transazione da uno stato all'altro, le azioni eseguite in seguito a un determinato evento
- quando un oggetto si trova in un certo stato puo' essere interessato ad alcuni eventi e non ad altri
- va utilizzato solo per le classi che presentano un ciclo di vita complesso e segnato da una successione ben definita di eventi

## Diagramma di stato
- rappresenta il ciclo di vita degli oggetti di una classe
- il ciclo di vita e' descritto in termini di
	- **eventi**
	- **stati**
	- **transizioni di stato**
- gli eventi possono attivare delle transizioni di stato
- un evento in uno statechart corrisponde a un messaggio in un sequence diagram
- uno stato e' costituito da un insieme di "valori significativi" assunti dagli attributi dell'oggetto che ne influenzano il comportamento

- Due stati "speciali" , detti **pseudostati**:
	- lo **stato iniziale** (pallino pieno)
	- lo **stato finale** (pallino semi vuoto)
- Un oggetto puo' non avere uno stato finale (non viene mai distrutto)
![[DiagrammaStato.png]]

- Un'evento puo' essere:
	- **invocazione sincrona** di un metodo (detta "call")
	- la ricezione di una **chiamata asincrona** ("signal")
	- una **condizione predefinita** che diventa vera ("change event")
	- la fine di un **periodo di tempo** come quello impostato da un timer ("elapsed-time event")
- Un evento si puo' rappresentare graficamente con una freccia (transizione) etichettata con il nome del metodo o della condizione associata all'evento stesso
- Un evento puo' essere rappresentato anche mediante una espressione testuale con la seguente sintassi:
	- event-name `'('[comma-separated-parameter-list]')' ['['guard-condition']'] / [action-expression]`
- dove:
	- **event-name** identifica l'evento
	- **parameter-list** definisce i valori dei dati che possono essere passati come parametro con l'evento
	- **guard-condition** determina se l'oggetto che riceve l'evento deve rispondere ad esso
	- **action-expression** definisce come l'oggetto ricevente deve rispondere all'evento

### Tipi di evento
- Event + state = response
	- Lo stesso evento causa diversi comportamenti in base allo stato in cui l'oggetto che riceve l'evento si trova
- Change event
- Guarded event

### Tipi di azioni
- **entry action** $\rightarrow$ azione che viene eseguita in una transizione entrante nello stato
- **exit action** $\rightarrow$ azione che viene eseguita in una transizione uscente dallo stato

### Modellare le attivita'
- All'interno degli stati possono essere eseguite delle azioni
- Negli statechart distinguiamo ta:
	- **Azioni** $\rightarrow$ operazioni atomiche
		- provocano un cambiamento di stato e quindi non possono essere interrotte
	- **Attività** $\rightarrow$ operazioni generalmente non atomiche
		- non alterano lo stato dell'oggetto

- Quando si verifica un evento associato ad una transizione, l'ordine di esecuzione e' il seguente:
	1. Se e' in esecuzione un'attivita', questa viene interrotta ("gracefully" se possibile)
	2. Si esegue l'exit action
	3. Si esegue l'azione associata all'evento
	4. Si esegue l'entry action del nuovo stato
	5. Si inizia l'esecuzione delle eventuali attivita' del nuovo stato

## Diagrammi di stato e di sequenza
- Due scenari (sequence diagram) $\rightarrow$ successo e fallimento di una transizione
	- Scenario di successo e relativo (parziale) diagramma a stati
	  ![[SequenceDiagramSuccess.png]]
	  
	- Scenario di fallimento e relativo (parziale) diagramma a stati
	  ![[SequenceDiagramFailure.png]]

## Diagramma a stati composti
- Uno stato puo' contenere al suo interno piu' **sottostati mutuamente esclusivi**
- Uno stato puo' contenere al suo interno **sottostati concorrenti**
![[StatoComposito.png]]

## Diagrammi di attivita'
- rappresenta sistemi di workflow, oppure la logica interna di un processo, di un caso d'uso o di una specifica operazione di una classe
- permette di modellare processi paralleli e la loro sincronizzazione
- e' un caso particolare di diagrammi di stato, in cui ogni stato e' uno stato di attivita'

- Un diagramma di attivita' mostra il **flusso di azioni** relativo ad un'attivita'
- Un'attivita' e' una **esecuzione non atomica** di operazioni all'interno di una macchina a stati
- L'esecuzione di un'attiovita' viene decomposta in azioni atomiche
- Ogni azione puo' o meno cambiare lo stato del sistema
- I diagrammi di attivita' sono spesso usati per descrivere la **logica di un algoritmo**

Diagramma di attivita':
- Azioni (atomiche)
	- Valutazione di espressioni
	- Assegnamenti/Ritorno di un valore
	- Invocazione di un'operazione su un oggetto
	- Creazione/distruzione di un oggetto
- Nodi e attivita'
- Raggruppamento di azioni atomiche o di altri nodi attivita'
- Un'azione puo' essere vista come un'attivita' che non puo' essere ulteriormente decomposta
- A parte questa differenza, i due concetti sono rappresentati mediante lo stesso simbolo grafico

- Quando un'azione o un'attivita' viene completata, il flusso di controlla passa al nodo azione immediatamente successivo
- Il flusso di controllo viene specificato mediante frecce che collegano due nodi (attivita' o azione)
- **flusso sequenziale** $\rightarrow$ flusso piu' semplice

- Un altro tipo di flusso possibile e' il **branch**
- Un branch e' rappresentato da un diamante
- Ogni branch ha:
	- Un flusso entrante
	- Due o piu' prefissi uscenti
	- Una condizione logica (talvolta implicita) che determina quale dei flussi uscenti verra' eseguito da una particolare esecuzione
- Quando due flussi si riuniscono, e' possibile usare ancora il simbolo del diamante: in questo caso viene detto **merge**
- Ogni merge ha almeno due flussi entranti e un flusso uscente

- Alcuni flussi possono essere concorrenti
- In UML vengono usate delle barre di sincronizzazione per specificare fork e join di flussi di controllo paralleli
- Una join rappresenta la sincronizzazione di due o piu' flussi di controllo concorrenti
- Una join ha due o piu' flussi entranti e un flusso uscente
- La sincronizzazione sul join attende che tutte le attivita' nei flussi entranti abbiano terminato la loro esecuzione prima di procedere
- Join e fork si devono bilanciare
- Le attivita' in un flusso di controllo parallelo comunicano tra loro spedendosi segnali (stile di comunicazione detto co-routine)

- A volte conviene separare le attivita' in base alle entita' che le devono svolgere
- In UML si usano le cosiddette swimlane
- E' un raggruppamento (verticale o orizzontale) di attivita' eseguite da una stessa entita' (per esempio una classe)
- Ogni swimlane deve avere un nome univoco nel diagramma
- Rappresentano responsabilita' specifiche nel contesto di un'attivita' generale
- Le attivita' sono associate univocamente ad un unica swimlane
- Solo le transizioni (flussi) possono attraversare due o piu' swimlane

- A volte e' utile evidenziare non solo il flusso di controllo, ma anche gli oggetti coinvolti
- Un'attivita' puo' creare un oggetto
- Un'altra attivita' puo' contenere azioni che modificano lo stato interno di un oggetto
- Il flusso del valore (stato) di un oggetto tra due azioni e' detto flusso dell'oggetto
- Lo stato viene rappresentato tra parentesi quadre all'interno dell'oggetto, oppure come constraint in una nota associata all'oggetto stesso

### Diagramma di attivita' e casi d'uso 
- Un caso d'uso puo' essere il punto di partenza per la costruzione di un diagramma di attivita'
- Entrambi sono rappresentazioni tipiche dell'analisi di un problema (o di un dominio)
- Il diagramma di attivita' fornisce una prospettiva algoritmica, mentre i casi d'uso forniscono una prospettiva funzionale
- Le due visite sono correlate, ma non totalmente equivalenti
- Il punto di partenza per costruire un diagramma di attivita' da un caso d'uso sono le descrizioni testuali, i flussi alternativi, le eccezioni, i singoli passi, le post-condizioni, le condizioni di terminazione
- Esempio: la spedizione di un ordine

## Diagramma dei componenti
- evidenzia l'organizzazione e le dipendenza tra i componenti software
- i componenti (come i casi d'uso o le classi) possono essere raggruppati in package
	- un componente e' una qualunque porzione fisica riutilizzabile con un'identita' e un'interfaccia ben definite
	- un componente puo' essere costituito dall'aggregazione di altri componenti
![[ComponentDiagram.png]]

## Diagramma di distribuzione
- Serve per mostrare come sono configurate e allocate le unita' hardware e software per un'applicazione
- Evidenzia la configurazione dei nodi elaborativi in ambiente di esecuzione (run-time) e dei componenti, processi ed oggetti allocati su questi nodi

## Package
- Consente di partizionare il sistema in sottoinsiemi costituiti da elementi omogenei di
	- natura logica (classi, casi d'uso, ...)
	- natura fisica (moduli, tabelle, ...)
	- altra natura (processori, risorse di rete, ...)
- ogni elemento appartiene ad un solo package
- un package puo' referenziare elementi appartenenti ad altri package

## Riassumendo
- UML e' una evoluzione di modelli preesistenti
- e' adatto a esprimere modelli di vario tipo, creati per obiettivi diversi
- puo' descrivere un sistema software a diversi livelli di astrazione, dal piano piu' svincolato dalle caratteristiche tecnologiche fino alla collocazione dei componenti software nei diversi processori di un'architettura distribuita
- puo' rispondere a tutte le necessita' di modellazione, ma e' opportuno "adattarlo" alle specifiche esigenze dei progettisti e dei progetti, utilizzando solo cio' che serve nello specifico contesto