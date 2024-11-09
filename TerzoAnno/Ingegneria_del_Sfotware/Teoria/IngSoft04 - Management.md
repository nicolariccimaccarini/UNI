## Project Management
- **Software Product Management**
	- intendiamo tutte le attivita' necessarie ad assicurare che un progetto software sia sviluppato rispettando le scadenze prefissate e risponda a determinati standard
	- aspetti sia tecnici che economici
- Un progetto diretto bene puo' anche fallire, ma un progetto diretto male fallisce quasi sicuramente

## Cosa intendiamo per progetto
- In generale per progetto intendiamo un insieme ben definito di attivita'
	- ha un inizio
	- ha una fine
	- ha uno scopo
	- viene portato avanti da un insieme di persone
	- utilizza un insieme di risorse
	- non e' un lavoro di routine
- Il project management deve conciliare tempi, scopo e budget

## Problemi
- Il software e' intangibile. Per valutare i progressi di un progetto bisogna basarsi su qualcosa: la documentazione
- L'ingegneria del software non e' ancora riconosciuta come disciplina 
- Non esiste un vero standard di processo per la produzione del software

## Motivi per cui un progetto puo' essere in ritardo
- Deadline non realistica, fissata e/o imposta da qualcuno all'esterno allo staff tecnico
- Cambiamenti dei requisiti imposti dal cliente
- Stima ottimistica del lavoro e delle risorse necessarie a compierlo
- Rischi che non sono stati persi in considerazione all'inizio del progetto
	- difficolta' tecniche prevedibili
	- difficolta' umane imprevedibili
- Problemi di comunicazione nel gruppo del progetto di riconoscere che c'e' un ritardo, e mancata attuazione di contromisure

## Principi fondamentali per la pianificazione
- Un progetto deve essere ripartito in attivita' e compiti di dimensioni ragionevoli
- Bisogna determinare le dipendenza tra attivita' e compiti
- Determinare quali compiti si possono svolgere in parallelo e quali in sequenza
- Alcune attivita' dipendono da altre per poter iniziare
- A ogni compito bisogna assegnare delle "unita' di lavoro"
- Ogni computo deve avere una data di inizio e una di fine
- A ogni progetto e' assegnato un numero definito di persone
- Non bisogna assegnare piu' persone del necessario
- Ogni compito deve essere assegnato a qualcuno
- Ogni compito deve avere un risultato predefinito
- A ogni compito si deve associare almeno un *punto di controllo*
- Un punto di controllo e' passato quando la qualita' di uno o piu' compiti e' approvata

## Gli attori sulla scena
- Senior manager
	- di solito definiscono gli aspetti economici del progetto
- Project managers
	- pianificano, organizzano e controllano lo sviluppo del progetto
- Practitioners
	- chi ha le competenze tecniche per realizzare parti del progetto
- Customers
	- il cliente che stabilisce i requisiti del software
- End users
	- chi usera' il sistema una volta sviluppato

### Il project manager
Di cosa si occupa?
- Stesura della proposta del progetto
- Stima del costo del progetto
- Planning e scheduling
	- partiziona il progetto, individua le milestones e i deliverables
- Monitoraggio e revisioni
- Selezione dello staff e assegnazione ai singoli compiti
- Stesura dei rapporti e delle prestazioni

## Abbiamo un piano?
1. Introduzione $\rightarrow$ definizione degli obiettivi del progetto e dei vincoli prefissati
2. Organizzazione $\rightarrow$ definisce l'organizzazione del team di sviluppo (quali sono le persone e quali sono i loro ruoli)
3. Analisi dei rischi $\rightarrow$ elenco dei rischi, della probabilita' che accadano, delle strategie per ridurli o affrontarli
4. Risorse HW e SW richieste $\rightarrow$ stime di costo per acquistare le risorse
5. Suddivisione del lavoro $\rightarrow$ suddivisione in attivita', vengono identificati i *deliverables* e le *milestones*
6. Scheduling del progetto $\rightarrow$ identificare le dipendenze tra le attivita', stima del tempo richiesto per le milestones, assegnazione personale alle attivita'
7. Controllo e rapporto sulle attivita' $\rightarrow$ elenca i rapporti che devono essere prodotti per i manager, quando devono essere prodotti e che meccanismi di controllo sullo stato di avanzamento delle attivita' sono previste

> milestones $\rightarrow$ sono i punti finali di ogni attivita'
> deliverables $\rightarrow$ sono i risultati del progetto consegnato ai clienti


## Scheduling
- Occorre suddividere il progetto in tasks e stimare il tempo e le risorse necessarie per completare ogni compito
- Organizzare i compiti concorrentemente permette un uso ottimale della forza lavoro
- Minimizzare le dipendenze tra task permette di evitare la propagazione a catena dei ritardi

**Problemi**:
- Stimare la difficolta' di un problema e i costi di sviluppo di una soluzione e' difficile
- La produzione non e' proporzionale al numero di persone che lavorano ad un task
- Regola del 40-20-40
	- 40% del tempo per l'analisi e la progettazione
	- 20% del tempo per la scrittura del codice
	- 40% per il collaudo

## Relazione tra persone e lavoro: curva PNR
![[curvaPNR.png]]

$L = P \cdot E^{\frac{1}{3}} \cdot t^{\frac{4}{3}} \leftarrow t \space \text{tempo}$ 
$L \rightarrow$ numero di righe di codice
$P \rightarrow$ parametro di produttivita' (valori tipici da $2000$ a $28000$)
$E \rightarrow$ impegno espresso in mesi/uomo o anni/uomo

## Tipologie di team
- **Democratico Decentralizzato**
	- assenza di un leader permanente
	- consegna di gruppo sulle soluzioni e sulla organizzazione del lavoro
	- comunicazione orizzontale
	- Vantaggi:
		- attitudine positiva e ricercare presto gli errori
		- funziona bene per problemi "difficili"
	- Svantaggi:
		- difficile da imporre
		- non e' scalabile
- **Controllato Decentralizzato**
	- un leader riconosciuto che coordina il lavoro
	- la risoluzione dei problemi e' di gruppo, ma l'implementazione delle soluzioni e' assegnata dal leader ai vari sottogruppi
	- comunicazione orizzontale tra i sottogruppi e verticale con il leader
- **Controllato Centralizzato**
	- il team leader decide sulle soluzioni e sull'organizzazione
	- comunicazione verbale tra il team leader e gli altri membri

### Ruoli in un team Controllato Decentralizzato
- Project manager
	- pianifica, coordina e supervisiona le attivita' del team
- Technical staff
	- conduce l'analisi e lo sviluppo
- Backup Engineer
	- supporta il project manager ed e' responsabile della validazione
- Software librarian
	- mantiene e controlla la documentazione, i listati del codice, i dati... 

## Mese-Uomo
- Come misurare l'effort richiesto per un lavoro? La misura tipica e' il mese-uomo
- In effetti facilita il calcolo dei costi del personale: basta fare $\text{numeroMesi} \cdot \text{nuemroUomini}$  
- Peccato che i progressi compiuti non siano proporzionabili a questa unita'
- Quand'e' che mesi e uomini sono effettivamente interscambiabili? Solo per compiti che possono essere perfettamente partizionati tra i lavoratori, *e che non richiedono comunicazione tra essi* 

## Work Breakdown Structure (WBS)
- Applicare alla pianificazione il principio del "dividi e conquista": il problema va risolto mediante la soluzione di sotto-problemi e la seguente integrazione delle soluzioni
- Definisce:
	- quali sono le attivita' da seguire
	- quali sono i rapporti gerarchici tra le attivita'
- Facilita l'attivita' di pianificazione in quante, grazie alla decomposizione in attivita' piu' semplici, migliora la capacita' di stimare:
	- tempi di realizzazione
	- costi di realizzazione
	- scopi realizzabili

## Diagrammi Gantt
- Vogliamo aggiungere al WBS le seguenti informazioni
	- informazioni sui tempi
	- vincoli di precedenza nell'esecuzione delle attivita'
	- informazioni sull'avanzamento delle attivita'
	- punti di controllo (checkpoint, milestones)
- Il diagramma di Gantt sono uno strumento sia in fase di pianificazione che in fase di monitoraggio
- Sono diagrammi bi-dimensionali
	- Sull'asse X vi e' il tempo (giorni, settimane, mesi)
	- Sull'asse Y vi sono le attivita' di progetto, prese da WBS
	- L'origine degli assi e' l'inizio del progetto

![[GnattDiagram.png]]

- Esistono molte varianti del diagramma di Gantt: tendono a sovrapporre in vario modo diversi modelli rappresentativi
- p.e.: sul diagramma si possono aggiungere i nomi degli esecutori delle attivita', gli attrezzi necessari o vincoli di altro tipo
- Il vantaggio di questi diagrammi consiste nell'avere in un colpo d'occhio le informazioni di sintesi circa il progetto e il suo andamento
- Esistono molti tool di supporto alla definizione, verifica e modifica di diagramma Gantt

## Algoritmi di trasformazione e pratiche di ottimizzazione
- **Processo** $\rightarrow$ e' la descrizione concettuale delle attivita' da eseguire e dei manufatti di input e output necessari a ognuna di queste. Consideriamolo come una n-tupla $P=(A, I, O)$ dove
	- $A = \{A_i\}$ e' l'insieme della attivita' elementari $A_i$ da eseguire
	- $I \cup I_i$ e' l'unione degli $I_i$, con $I_i$ che e' l'insieme dei manufatti in input necessari all'esecuzione dell'attivita' $A_i$
	- $O=\cup O_i$ e' l'unione degli $O_i$, con $O_i$ che e' l'insieme dei manufatti in output risultanti dall'esecuzione dell'attivita' $A_i$
- **Piano di progetto** $\rightarrow$ indica la sequenza temporale potenziale con cui sono eseguite le attivita' $A$ dichiarate nel modello di processo
- **Piano esecutivo** $\rightarrow$ indica la sequenza temporale reale delle attivita' $A$, considerati i vincoli di progetto e le decisioni manageriali circa tempi, costi, uomini (risorse)

### Piano di progetto ed esecutivo
- Lo scopo e' essere flessibili durante la gestione di un progetto
	- Il **piano di progetto** rappresenta l'ottimizzazione ottimale del progetto: pianifica la sequenza di esecuzione tenendo conto dei soli vincoli di precedenza imposti dal fabbisogno dei manufatti delle attivita' e non considerando vincoli di tempi, costi e personale
	- Il **piano esecutivo** parte dalla pianificazione ottimale e tenendo conto delle precedenze la rimodula sulla base dei vincoli di tempi, costi e uomini
- Questi piani si rappresentano utilizzando il diagramma Gantt e Pert.


FARE DA 49 A FINE