## Processo di produzione del software
- Insieme coerente di attivita' per la specifica, il progetto, l'implementazione, la verifica di sistemi software: e' il percorso per sviluppare il prodotto sw
- Un modello di processo e' una rappresentazione astratta di un processo|
	- descrive il processo da una particolare prospettiva
- Modelli di processo generici:
	- a cascata (waterfall)
	- sviluppo evolutivo
	- sviluppo formale
	- meccanismi basati sul riutilizzo
	- modelli agile (extreme programming)
	- modello a spirale

## Ciclo di risoluzione di un problema
- Status quo $\rightarrow$ stato della situazione attuale
- Definizione del problema $\rightarrow$ individuare lo specifico problema da risolvere
- Sviluppo tecnico $\rightarrow$ risoluzione di un problema mediante una opportuna tecnologia
- Integrazione $\rightarrow$ consegna dei risultati al committente
- Il ciclo si puo' applicare a diversi livelli di dettaglio:
	- a livello piu' alto (intero sistema)
	- ai singoli sottoinsiemi
	- alle singole funzioni

## Modello
- Modellare il processo significa strutturarlo
	- dividerlo in attivita'
	- per ogni attivita' specificare
		- cosa
		- quali prodotti
		- quando

## Modello di ciclo di vita
- Come organizzo le attivita'
	- ordinamento delle attivita'
	- criteri per terminare un'attivita' e passare alla successiva

### Come sono evoluti i modelli del ciclo di vita?
- Build-and-fix $\rightarrow$ non ho un modello
	- attivita' non identificate ne organizzate
	- il progetto non e' gestito
- Modelli prescrittivi
	- cascata
	- incrementale
	- rapid prototyping
	- spirale
- Unified Process
- Modelli agili
	- extreme programming
	- scrum

#### Build-and-Fix
- Il prodotto viene sviluppato senza specifica, nessun tentativo di progettazione $\rightarrow$ "stile libero"
	- Chi sviluppa comincia a scriver il programma
	- il programma viene modificato piu' volte finche' non va bene al committente
![[BuildAndFix.png]]

- Puo' andare bene per un progetto molto piccolo
- Non e' proponibile per prodotti di ragionevoli dimensioni
- Non esiste specifica ne documentazione $\rightarrow$ la manutenzione e' molto difficile

#### Modello a cascata
![[modelloCascata.png]]

Fasi del modello a cascata:
- Analisi e definizione dei requisiti
- Progettazione del sistema e/o del software
- Implementazione e test delle singole unita'
- Installazione e mantenimento del sistema

Problemi del modello a cascata:
- Il grosso limite del modello a cascata e' la difficolta' a effettuare cambiamenti nel corso del processo
	- e' molto difficile soddisfare cambiamenti nei requisiti da parte del committente
- Manca interazione con il cliente $\rightarrow$ c'e' solo il prodotto finito
- La suddivisione in fasi puo' sembrare arbitraria o artificiosa
- **Il modello a cascata e' adeguato quando i requisiti sono ben compresi dall'inizio e non sono soggetti a modifiche**

#### Prototipazione
- **Spesso i requisiti non sono sufficientemente chiari**
	- Il committente stabilisce gli scopi generali del sistema software, ma non chiarisce subito i requisiti in modo completo
	- oppure gli sviluppatori sono incerti sul significato di alcuni requisiti o su come strutturare l'interfaccia o gli algoritmi
- Possibilita' di procedere per prototipi
- Possibilita' di combinare queto modello con il modello a cascata

#### RAD (Rapid Application Development)
- Modello sequenziale lineare per avere un ciclo di sviluppo molto breve
- Lo sviluppo rapido e' ottenuto mediante il riuso dei componenti
- Quando applicarlo:
	- requisiti chiari
	- processo di sviluppo ben vincolato
- Nei casi in cui si puo' applicare, il modello RAD puo' portare allo sviluppo di software in tempi brevi rispetto al modello a cascata classico
- Adatto alle situazioni in cui il sistema da implementare puo' essere facilmente partizionato fin dall'inizio
- Ogni parte deve essere sviluppata indipendentemente e in tempi brevi (< 3 mesi)

![[RAD.png]]

Fasi del modello RAD:
- Comunicazioni
	- servono per comprendere il problema e i dati che devono essere considerati dal software
- Pianificazione
	- come suddividere il lavoro tra vari team
- Modellazione
	- modellazione business $\rightarrow$ quali dati guidano il processo? quali dati vengono generati? da chi? chi li utilizza?
	- modellazione dati $\rightarrow$ definirli in termini di oggetti o di ADT
	- modellazione del processo $\rightarrow$ definire le operazioni sui dati
- Costruzione
	- generare l'applicazione con linguaggi di alto livello, riuso di componenti, ...
	- collaudo singoli componenti
- Deployment
	- integrazione dei vari componenti e collaudo finale

Limiti del modello RAD:
- Progetti estesi ma partizionabili
	- bisogna avere risorse umani sufficienti a creare il numero corretto di team
- Sviluppatori e clienti devono essere disponibili a completare il sistema in tempi rapidi
- RAD non e' il modello adatto quando:
	- il sistema non puo' essere modularizzato correttamente
	- si richiedono alte prestazioni che dipendono da interfacce ben definite
	- si usano tecnologie innovative con alto rischio di implementare problemi strada facendo

#### Modello evolutivo
- Prototipazione di tipo evolutivo
	- L'obiettivo e' lavorare con il committente ed evolvere verso il sistema finale a partire da una specifica di massima. Lo sviluppo inizia con le parti del sistema che sono gia' ben specificate, aggiungendo via via nuove caratteristiche
- Prototipazione di tipo usa e getta
	- L'obiettivo e' capire i requisiti del sistema e quindi sviluppare una definizione migliore dei requisiti. Il prototipo sperimenta le parti del sistema che non sono ancora ben comprese
![[modelloEvolutivo.png]]

- Problemi:
	- mancanza di visibilita' del processo
	- sistemi spesso poco strutturati
	- possono essere richieste capacita' particolari
- Attenzione a non trasformare il prototipo in un prodotto da porre in produzione:
	- al committente di prototipo potrebbe sembrare funzionante, ma puo' avere tanti e tali problemi da essere inutilizzabile in condizioni reali
	- il sistema va rifatto
- Applicabilita':
	- sistemi interattivi di piccola o media dimensione
	- per parti di sistemi piu' grandi
	- per sistemi con vita di attesa corta
	- per sviluppare prototipi

#### Modello trasformazionale (o modello formale)
- Basato sulla trasformazione di una specifica matematica in una programma eseguibile, attraverso trasformazioni che permettono di passare da una rappresentazione formale ad un altra
- Le trasformazioni devono preservare la correttezza
	- in questo modo e' banale verificare che il programma soddisfa la specifica

Fasi del modello trasformazionale:
- Definizione dei requisiti
- Specifica formale
- Trasformazione e formale
- Integrazione e test

Problemi:
- richiede conoscenze specializzate e addestramento per essere applicato
	- certe parti del sistema sono difficili da specificare formalmente

Quando applicarlo:
- sistemi critici, in cui sicurezza e/o affidabilita' sono essenziale e devono non solo essere raggiunti, ma bisogna dimostrare che il sistema li puo' mantenere

#### Modello basato sul riutilizzo (sviluppo a componenti)
- Si basa sul riutilizzo sistematico di componenti *off-the-shelf* opportunamente integrate
- Fasi del modello:
	- analisi dei componenti
	- adattamento dei requisiti
	- progettazione del sistema
	- integrazione
- E' per natura evolutivo $\rightarrow$ richiede un approccio iterativo allo sviluppo del software
- Classico esempio: tecnologia a oggetti
- Vantaggi:
	- netta riduzione del ciclo di sviluppo
	- netta riduzione dei costi di progetto
	- maggiore produttivita'
- Svantaggi:
	- dipende tutto dal trovare componenti che possano servirci

#### Modello a spirale
- Obiettivo principale $\rightarrow$ minimizzare i rischi
- Rischio $\rightarrow$ misura di incertezza del risultato di un'attivita'
- Meno informazioni si ha, piu' alti sono i rischi
- Bisogna acquisire maggiori informazioni per ridurre l'incertezza
- Il processo di sviluppo e' rappresentato come una spirale piuttosto che come una sequenza
- Ogni ciclo nella spirale e' una fase del processo
- A termine di ogni "giro" il risultato puo' essere un *progetto*, un *prototipo*, un *sistema funzionante* oppure un *software* completo
- Non ci sono fasi predefinite $\rightarrow$ sta al management del progetto decidere come strutturarlo in fasi
![[modelloSpirale.png]]

La spirale e' divisa in spicchi:
- **Comunicazione con il cliente** $\rightarrow$ specificare gli obiettivi e i vincoli di quella fase, indentificare i rischi e eventualmente proporre strategie alternative
- **Pianificazione** $\rightarrow$ definizione delle scadenze e delle risorse
- **Analisi dei rischi** $\rightarrow$ sistemare i rischi tecnici e di gestione
- **Strutturazione** $\rightarrow$ costruire una o piu' rappresentazioni del sistema
- **Costruzione e rilascio** $\rightarrow$ sviluppo effettuato secondo un modello generico
- **Valutazione da parte del committente** $\rightarrow$ ricevere le reazioni del cliente su quanto prodotto. Nei primi giri si creano idee o proposte, nei successivi prodotti o prototipi

Vantaggi:
- concentra l'attenzione
- concentra l'attenzione sull'eliminaizone degli errori
- valutazione esplicita dei rischi
- adatto allo sviluppo di progetti di grandi dimensioni
- interazione con il cliente a ogni giro della spirale

Svantaggi:
- richiede esperienza nella valutazione dei rischi
- errori nella valutazione dei rischi si ripercuotono nelle fasi successive
- richiede raffinamenti per uso generale

#### Relational Unified Process (RUP)
- Derivato da UML e dal relativo **Unified Software Development Process**
- Modello ibrido $\rightarrow$ prendere elementi dai vari modelli generici, aiuta i cicli e illustra buone prassi nella specifica e nella progettazione
- Individua due prospettive
	1. **Prospettiva tecnica** $\rightarrow$ tratta gli aspetti qualitativi, ingegneristici e di metodo di progettazione
	2. **Prospettiva gestionale** $\rightarrow$ tratta gli aspetti finanziari, strategici, commerciali e cani
- Consiste di 4 fasi, separate da milestone che possono essere superate solo se lo stato di avanzamento e' sufficiente e l'analisi dei rischi e' soddisfacente
- Se non si supera un milestone, o si abortisce il progetto, oppure la fase viene iterata per raffinare il risultato e risolvere il problema

Fasi:
- **Inception** (avvio) $\rightarrow$ comprende le attivita' di comunicazione con il cliente e di pianificazione. Collaborando con il cliente e utenti finali si identificano i requisiti business, si propone una architettura base per il sistema e un piano per la natura iterativa e incrementale del progetto. Si ottiene indicazione dai principali sottoinsiemi e delle relative funzioni, da raffinare e ampliare in seguito. La pianificazione identifica risorse, rischi principali e stabilisce un piano dei tempi per le fasi successive
- **Elaboration** $\rightarrow$ comprende comunicazione con il cliente e modellazione. Si ampliano gli use case e si espande la rappresentazione dell'architettura con 5 visite: modello use case, modello analitico, modello di progettazione, modello implementativo, modello deployment. Alla fine di questa fase si ricontrolla la pianificazione.
- **Construction** $\rightarrow$ sostanzialmente progettare, programmare e testare il sistema. Varie parti vengono sviluppate parallelamente e integrate. A seconda della dimensione del progetto, questa fase e' divisa in varie iterazioni: inizialmente prototipi e poi raffinamenti e perfezionamento. Alla fine di questa fase si dovrebbe avere qualcosa di funzionale e relativa documentazione
- **Transition** $\rightarrow$ spostare il sistema dalla comunita' di sviluppo al cliente e farlo funzionare nell'ambiente reale. Alla fine di questa fase, l'ultima versione sviluppata dell'incremento software diventa una release software utilizzabile

La ciclicita' e l'incremento sono supportati sia in ogni fase, sia come intero insieme di fasi.
Visione statica $\rightarrow$ suddivisione in attivita' di produzione del software (workflow); sei workflow principali e tre di supporto.

### Valutazione dei rischi nei modelli di processo del software
- Modello a cascata
	- alto rischio per sistemi nuovi, per problemi di specifica e di progettazione
	- basso rischio per sviluppo di problemi se familiarita' e' gia' acquisita
- Modello evolutivo, prototipazione
	- basso rischio per nuovi problemi
	- alto rischio a causa della scarsa visibilita' del processo (antieconomico produrre documentazione a ogni iterazione)
- Modello trasformazionale
	- alto rischio legato alla necessita' di tecnologia avanzata e capacita' da parte degli sviluppatori

### Come identificare i rischi
- Comincia con una lista di errori tipici
- Guarda il piano di sviluppo
	- cammini critici
	- membri dello staff critici
	- consegne critiche
	- milestones critiche
- Guarda i requisiti
- Guarda la progettazione tecnica
- Guarda i progetti precedenti
- Conduci delle sessioni di brainstorming con lo staff, gli utenti finali, i venditori e il management dedicate all'identificazione dei rischi

### Rischi: analisi, conseguenze e priorita'
- Per ciascun rischio
	- determina la probabilita' che esso si verifichi
	- determinare l'impatto
	- determinare le conseguenze $\rightarrow$ cosa perdiamo se il rischio si verifica?
- Per l'insieme dei rischi
	- mettili in ordine di priorita'

### Esempio di classificazione dei rischi
![[classificazioneRischi.png]]

### Visibilita' del processo software
- C'e' bisogno di documentazione per poter valutare i progressi nel processo di sviluppo software
- Problema
	- la programmazione dei tempi di consegna dei "deliverables" puo' non combaciare con i tempi necessari per completare un'attivita'
	- la necessita' di produrre documentazione vincola l'iterazione del processo
	- il tempo necessario per approvare i documenti e' significativo