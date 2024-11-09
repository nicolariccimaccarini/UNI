- La progettazione e' il processo che porta alla definizione ingegneristica di cio' che deve essere realizzato
- Si compone di due fasi:
	- **diversificazione** $\rightarrow$ il progettista acquisisce il materiale grezzo del progetto per individuare le possibilita' realizzative
	- **convergenza** $\rightarrow$ il progettista sceglie e combina gli elementi disponibili per arrivare ad un prodotto finale

## I 3 requisiti progettuali
- Il progetto deve soddisfare tutti i requisiti espliciti contenuti nel modello concettuale e tutti i requisiti impliciti voluti dal cliente
- Il progetto deve essere una guida leggibile e comprensibile per chi si occupera' delle fasi di codifica, collaudo e manutenzione
- Il progetto deve dare un quadro completo e coerente del software, considerando i domini dei dati, funzionale e comportamentale dal punto di vista dell'implementazione

## Indicazioni generali
- L'architettura del progetto deve
	- essere creata con modelli di progettazione riconoscibili
	- essere costituita da componenti ben progettate
	- poter essere implementata in modo evolutivo
- Il progetto deve essere modulare e contenere una rappresentazione distinta di dati, architetture, interfacce e componenti
	- le strutture dei dati devono essere tratte da modelli di dati riconoscibili e devono essere appropriate per i dati da implementare
	- le componenti devono avere caratteristiche funzionali indipendenti
	- le interfacce devono tendere a ridurre la complessita' delle comunicazioni tra moduli e verso l'esterno
- Il metodo di progetto deve essere ripetibile e pilotato dai requisiti

## Tools per la progettazione
- I tools usati di solito sono catalogabili come
	- meccanismi per tradurre il modello concettuale in progetyto
	- notazioni per rappresentare i componenti funzionali e le loro interfacce
	- regole euristiche per il raffinamento e la suddivisione dei moduli
	- metodi per la valutazione della qualita'

## Regole empiriche
- Non procedete col paraocchi
	- siate aperti all'utilizzo di soluzioni alternative
- Il progetto deve sempre essere riconducibile al modello concettuale 
	- poiche' un singolo elemento del progetto e' relativo a piu' requisiti, e' necessario poter risalire al modo in cui i requisiti sono soddisfatti nel progetto
- Non re-inventare la ruota
	- dove e' possibile, riutilizzare schemi o strutture gia' sviluppati in altri progetti
- Il progetto finale deve apparire uniforme ed integrato
	- se si lavora in team definire da subito le regole di formato e di stile
- Il progetto deve poter accogliere modifiche
- Il software deve reagire in maniera controllata alle situazioni di errore
	- se e' ben fatto, dovrebbe poter reagire a condizioni non standard e se necessario arrestarsi in maniera regolata
- Progetto $\ne$ stesura del codice
	- il livello di astrazione del progetto e come mantenerla vanno decisi all'inizio dello sviluppo
- La qualita' del progetto e come mantenerla vanno decisi all'inizio dello sviluppo
	- sia per la qualita' esterna sia per quella interna
- Al termine del progetto va sempre prevista una revisione formale che lo riesamini

## Fasi della progettazione
- **Comprensione** del problema
	- guardare al problema da angolature differenti
- Identificare una o piu' **soluzioni**
	- valutare le soluzioni possibili e scegliere la piu' appropriata rispetto all'esperienza del progettista e alle risorse disponibili
- Descrivere **astrazioni** delle soluzioni
	- usare notazioni grafiche, formali o altro per descrivere le componenti del progetto
- Ripetere lo step per ogni astrazione identificata, finche' la progettazione non e' espressa in termini primitivi

### Astrazione
- L'astrazione e' l'atto di dare una descrizione del sistema ad un certo livello, trascurando i dettagli inerenti i livelli sottostanti
	- a livelli di astrazione elevati si utilizza un linguaggio vicino al contesto del problema che il sistema dovra' risolvere
	- a livelli piu' bassi di astrazione del linguaggio si formalizza sempre di piu' fino ad arrivare, al livello piu' basso, al codice sorgente

### Raffinamento
- Per raffinare utilizziamo tecniche di scomposizione per passare da astrazioni funzionali ad alto livello alle linee del codice
- Raffinamento e astrazione possono essere considerate attivita' complementari
	- mediante l'astrazione il progettista specifica procedure e dati eliminando i dettagli di basso livello
	- mediante raffinamento i dettagli emergono via via

![[fasiProgettazione.png]]

## Fasi del design
- Architectural Design
	- identificare e documentare i sottosistemi e le loro relazioni
- Abstract Specification
	- specifichiamo i servizi forniti da ciascun sottosistema e i vincoli a cui deve sottostare
- Interface Design
	- descriviamo l'interfaccia dei sottosistemi verso altri sottosistemi; la specifica delle interfacce deve essere non ambigua (deve consentire la definizione dei sottosistemi ignorando come sono fatti all'interno)
- Component Design
	- allocare i servizi ai diversi componenti e definire le interfacce di questi componenti
- Data Structure Design
	- definire come sono fatte le strutture dati
- Algorithm Design
	- specificare gli algoritmi utilizzati

## Progettazione top-down
- Il problema viene partizionato ricorsivamente in sottosistemi fino a che non si identificano dei sottoproblemi trattabili
- In teoria, si inizia con il componente radice della gerarchia e si procede verso il basso livello dopo livello
- In pratica per sistemi di grosse dimensioni la progettazione non e' mai completamente top-down:: alcuni rami vengono sviluppati prima e i progettisti riutilizzano l'esperienza e le componenti

## Strategie di decomposizione
- Top-down $\rightarrow$ decomposizione del problema
- Bottom-up $\rightarrow$ composizione di soluzioni
- Sandwich $\rightarrow$ soluzione naturale

## Modularita' e integrazione
- Un sistema composto da un unico blocco monolitico di software e' sempre difficile da comprendere, implementare e mantenere
- L'unico modo per permettere di gestire intellettualmente il programma e' suddividerlo in moduli con funzionalita' definite e limitate con interfacce ben definite
- Warning $\rightarrow$ una eccessiva modularita' richiede sforzi per l'integrazione

## Costi di sviluppo e integrazione
- $C(p) =$ complessita' percepita per la soluzione del problema $p$
- $E(p) =$ impegno impiegato nella risoluzione del problema $p$
- empiricamente si ha che:
	- $C(p_1) > C(p_2)$ implica $E(p_1) > E(p_2)$ 
	- $C(p_1 + p_2) > C(p_1) + C(p_2)$
	- $E(p_1 + p_2) > E(p_1) + E(p_2)$

## Criteri di Mayer
- Scomponibilità $\rightarrow$ un metodo che permette la scomposizione del problema in sottoproblemi (riduce la complessita')
- Componibilità $\rightarrow$ un metodo che permette l'assemblemanto di componenti preesistenti (migliora la produttività)
- Comprensibilita' $\rightarrow$ un modulo le cui interfacce con altri moduli siano minimi e' di piu' facile costruzione e modificabilita'
- Continuita' $\rightarrow$ modifiche ai requisiti di sistema che comportano solo modifiche a singoli moduli sono di facile controllo
- Protezione $\rightarrow$ se effetti anomali in un modulo non si propagano la mantenibilita' migliora

## L'architettura del software
- Intendiamo la "struttura complessiva del software e il modo in cui tale struttura sorregge l'integrita' concettuale di un sistema"
	- cioe' descrive la struttura gerarchica dei moduli di un programma, come interagiscono e la struttura dei dati che manipolano
- Uno degli obiettivi della progettazione e' la definizione di una architettura appropriata per il sistema che si sviluppa 

- *Proprieta' strutturali* $\rightarrow$ l'architettura deve descrivere le componenti del sistema e come sono assemblate (interazioni tra componenti)
- *Proprieta' extrafunzionali* $\rightarrow$ l'architettura deve esplicitare in che modo vengono soddisfatti i requisiti di prestazioni, affidabilita', sicurezza, modificabilita', ...
- *Affinità* $\rightarrow$ il progetto architettura deve permettere di usare strutture e schemi simili per progetti simili

- Specificate le proprieta' precedenti, l'architettura puo' essere presentata usando uno o piu' modelli
	- Modelli strutturali $\rightarrow$ mostrano l'architettura come una collezione organizzata di componenti
	- Modelli schematici $\rightarrow$ usati per individuare schemi progettuali ricorrenti in applicazioni dello stesso tipo
	- Modelli dinamici $\rightarrow$ mostrano gli aspetti comportamentali dell'architettura, indicano in che modo il sistema muta a seguito di eventi esterni
	- Modelli di processo $\rightarrow$ descrivono il processo aziendale o tecnico che il sistema deve sviluppare
	- Modelli funzionali $\rightarrow$ descrivono la gerarchia funzionale di un sistema (quali funzioni usano quali altre)

## Ripartizione strutturale
- Dato un sistema organizzato (gerarchia ad albero), la struttura del programma puo' essere partizionata in senso orizzontale o verticale

### Ripartizione orizzontale
- La versione piu' semplice, definisce tre partizioni
	1. input
	2. trasformazioni
	3. output
- Vantaggi
	- piu' facile da collaudare
	- manutenzione piu' semplice
	- la propagazione di effetti collaterali e' ridotta
	- il software risulta piu' facile da estendere

### Ripartizione verticale
![[ripartizioneVerticale.png]]

## La struttura dei dati
- La struttura dei dati definisce l'organizzazione, i metodi di accesso, il grado di associativita' e le alternative di elaborazione per le informazioni
- Organizzazione e complessita' dipendono dall'inventiva del progettista e dalla natura del problema
- Esistono diverse strutture di base che possono essere combinate
	- elemento scalare $\rightarrow$ entita' elementare (bit, intero, reale, stringa)
	- vettore sequenziale $\rightarrow$ gruppo contiguo di elementi scalari omogenei
	- spazio n-dimensionale $\rightarrow$ vettore a 2 o piu' dimensioni
	- lista $\rightarrow$ gruppo di elementi connessi

## La procedura software
- La gerarchia di controllo riassume le relazioni gerarchiche tra i moduli ma non descrive la logica interna dei moduli
- La procedura software si concentra sui dettagli dell'elaborazione specificando la sequenza degli eventi, i punti di decisione e i punti di chiamata ai moduli subordinati
- Un flow-chart e' una rappresentazione grafica della procedura software

## Information hiding
- Il principio dell'information hiding richiede che ciascun modulo sia definito in modo che le sue procedure e le informazioni locali su cui agisce non siano accessibili ad altri moduli
- l'interazione con gli altri moduli deve avvenire solo tramite la sua interfaccia
- Vantaggi $\rightarrow$ facilita' di modifica di un modulo, perche' non ci si deve preoccupare di effetti collaterali delle modifiche
- La definizione di moduli tramite la tecnica dell'information hiding puo' essere d'aiuto nell'identificare il punto di minimo costo per la modularita' del sistema

## Strategie di progettazione
- Progettazione funzionale $\rightarrow$ lo stato del sistema e' centralizzato e condiviso tra funzioni che operano su quello stato
- Progettazione object-oriented $\rightarrow$ il sistema e' diviso come un insieme di oggetti che interagiscono. Il sistema e' de-centralizzato e ogni oggetto ha un proprio stato. Gli oggetti possono essere istanze di una classe e comunicano scambiando attraverso i propri metodi

### Progettazione mista
- C'e' una complementarieta' tra approccio funzionale e approccio object-oriented
- Di volta in volta un buon ingegnere del software dovrebbe scegliere l'approccio piu' appropriato per il sottosistema che sta progettando

### Qualita' della progettazione
- La qualita' di un progetto e' difficile da stabilire. Dipende da specifiche priorita' di tipo organizzativo
	- un "buon" progetto potrebbe essere il piu' efficiente, il meno costoso, il piu' manutenibile, il piu' affidabile, ...
- Noi sottolineeremo gli attributi legati alla mantenibilita' del progetto: coesione, accoppiamento, comprensibilita', adattabilita'.
	- un progetto mantenibile puo' essere adatto modificando funzionalita' esistenti o aggiungendone di nuove; il progetto dovrebbe rimanere comprensibile; i cambiamenti dovrebbero avere effetto locale.
- Le stesse caratteristiche di qualita' si applicano sia alla progettazione funzionale che a quella orientata agli oggetti

#### Indipendenza modulare
- Per ottenere una modularita' effettiva, i moduli devono essere indipendenti, cioe' devono occuparsi di una funzione be determinata nelle specifiche dei requisiti ed interagire con gli altri moduli solo tramite interfacce semplici
- L'indipendenza di un modulo puo' essere misurata in termini della sua coesione e dal suo accoppiamento con altri moduli

#### Coesione
- Un modulo e' coeso quando esegue un numero di compiti limitato e coerente: nel caso ideale implementa una singola entita' logica o una singola funzione
- Ogni modulo avra' un grado piu' o meno alto di coesione: occorre tenere alta la coesione media ed eliminare i moduli a bassa coesione
- La coesione e' un attributo importante in quanto, qualora si dovesse effettuare un cambiamento al sistema, permette di mantenere il cambiamento locale ad una singola componente.
- Si possono individuare livelli diversi di coesione

- Proprieta' interna al singolo componente
	- funzionalita' vicine devono stare nello stesso componente
	- vicinanza per tipologia, algoritmi, dati in ingresso e in uscita
- Vantaggi di un alto grado di coesione
	- vantaggi rispetto al riuso e alla manutenibilita'
	- riduce l'interazione fra i componenti
	- migliore comprensione dell'architettura del sistema

##### Tipologie e livelli di coesione
- Coesione incidentale (debole)
	- le diverse parti di un componente sono semplicemente raggruppate insieme, ma non sono affatto correlate
- Associazione logica (debole)
	- vengono raggruppate le componenti che svolgono azioni simili (p.e. tutte le routine matematiche)
- Coesione temporale (debole)
	- vengono raggruppate le componenti che sono attivate nello stesso istante di tempo
- Coesione procedurale (debole)
	- vengono raggruppati tutti gli elementi di una componente che costituiscono una singola sequenza di controllo, cioe' che vengono attivati in sequenza uno dopo l'altro
- Coesione di comunicazione (media)
	- tutti gli elementi di una componente operano su di uno stesso input o producono lo stesso output
- Coesione sequenziale (media)
	- l'output di una parte della componente e' l'input di un'altra parte
- Coesione funzionale (forte)
	- ogni parte di una componente e' necessaria solo per l'esecuzione di una singola funzione di quella componente
- Coesione d'oggetto (forte)
	- ogni operazione fornisce delle funzionalita' per osservare o modificare gli attributi di un oggetto

##### Coesione come attributo di progetto in progettazione OO
- Se si ereditano attributi da una superclasse si diminuisce la coesione
	- per comprendere una classe bisogna esaminare sia tutte le sue superclassi che le componenti della classe

#### Coupling (accoppiamento)
- Misura la "forza" di connessione tra le componenti di un sistema: quanto le componenti "si usano" tra di loro
- Loose coupling (accoppiamento lasco) implica che i cambiamenti di una componente non anno forti effetti sul comportamento delle altre
- Variabili condivise o lo scambio di controllo porta ad accoppiamento stretto (tight coupling)
- L'accoppiamento lasco puo' essere ottenuto decentralizzando gli stati e realizzando la comunicazione con passaggio di parametri o di messaggi

##### Tipi di accoppiamento
- Basso accoppiamento
	- moduli diversi si scambiano parametri "semplici"
	- una alternativa si ha quando attraverso l'interfaccia di un modulo passa una struttura dati anziche' dati semplici o atomici (accoppiamento a stampo)
- Accoppiamento di controllo
	- un "segnale di controllo" viene scambiato tra due moduli
- Accoppiamento comune
	- moduli diversi hanno dati in comune; questo tipo di accoppiamento e' insidioso e puo' causare problemi difficili da diagnosticare

#### Accoppiamento
- In un sistema software gli elementi collaborano tra loro quindi e' normale che ci siano dipendenze
	- per cui l'accoppiamento e' inevitabile ma va mantenuto il piu' basso possibile
- Nella programmazione OO le forme più comuni di accorpamento tra due tipi `TypeX` e `TypeY` sono
	- `TypeX` ha un attributo `TypeY` o referenzia una istanza `TypeY`
	- un oggetto `TypeX` richiama servizi di un oggetto `TypeY`
	- `TypeX` ha un metodo che referenzia oggetti di tipo `TypeY` (variabili locali, parametri o tipi ritornati)
	- `TypeY` è una interfaccia e `TypeX` implement direttamente o indirettamente questa interfaccia

##### Basso accoppiamento: come?
- Problema $\rightarrow$ come ridurre l'impatto dei cambiamenti, aumentare manutenibilita' e riusabilita'?
	- assegnare la responsabilita' in modo che l'accoppiamento rimanga basso
	- usare questo principio per valutare le alternative progettuali. A parita' di altre condizioni si consideri il progetto con il minore accoppiamento

#### Accoppiamento ed ereditarieta'
- I sistemi orientati ad oggetti sono sistemi "loosely coupled"
	- non condividono uno "stato"
	- gli oggetti comunicano passando messaggi attraverso metodi
- Tuttavia una classe e' accoppiata con la sua superclasse
	- i cambiamenti effettuati su una classe si propagano a tute le sottoclassi

#### Comprensibilita'
- Legata a diverse caratteristiche delle componenti
	- coesione $\rightarrow$ la componente puo' essere considerata da sola?
	- naming $\rightarrow$ i nomi usati sono significativi
	- documentazione $\rightarrow$ il progetto e' ben documentato
	- complessita' $\rightarrow$ si usano algoritmi complicati
- Di solito un'elevata complessita' significa molte relazioni tra diverse parti del sistema e quindi scarsa comprensibilita'
- Metriche di qualita' di progettazione: misurano la complessita'

#### Adattabilita'
- Un progetto e' adattabile quando
	- le sue componenti sono debolmente accoppiate
	- e' ben documentato e la documentazione e' aggiornata
	- c'e' una corrispondenza stretta tra livelli della progettazione
	- ogni componente e' auto-contenuta (coesione forte)
- Per adattare un disegno si devono poter individuare tutti i collegamenti tra componenti diverse, cosi' che le conseguenze di una modifica a un componente possano essere analizzate

##### Adattabilita' ed ereditarieta'
- L'ereditarieta' migliora molto l'adattabilita'
	- le componenti possono essere adattate senza cambiamenti attraverso la derivazione di una sotto-componente e la modifica solo di quest'ultima
- Tuttavia man mano che la profondita' dell'ereditarieta' cresce adattare il progetto diviene sempre piu' complesso
	- deve essere periodicamente rivisto e ristrutturato

### Come migliorare la modularita' del progetto
1. Studia la prima versione del progetto e individuando la possibilita' di aumentare la coesione e diminuire l'accoppiamento
	- esplosione dei moduli $\rightarrow$ trasformazione di una funzionalita' comune a piu' moduli in un modulo separato (aumento di coesione)
	- implosione dei moduli $\rightarrow$ piu' moduli con interfacce reciproche complesse possono collassare in un unico modulo per diminuire l'accoppiamento generale del progetto
2. Diminuire i fan-out ad alto livello gerarchico e aumentare il fan-in a basso livello gerarchico
	- una struttura "ovale" migliora la ripartizione verticale del progetto (compiti sempre piu' elementari scendendo nella gerarchia)
3. Mantenere il campo di azione in un modulo entro il suo campo di controllo
	- **campo di azione** $\rightarrow$ insieme dei moduli che dipendono da una decisione presa localmente
	- **campo di controllo** $\rightarrow$ insieme dei moduli subordinati (anche indirettamente)
4. Studiare le interfacce per ridurre l'accoppiamento
5. Definire i moduli con funzioni prevedibili
	- modulo come scatola nera $\rightarrow$ si sa cosa fa anche se non si sa la struttura interna
	- evitare moduli con memoria, cioe' il comportamento dipende dalla sequenza di azioni che hanno fatto
6. Evitare collegamenti patologici tra moduli
	- salti interni al modulo o simili

## La specifica del progetto
- La specifica del progetto e' il documento che descrive il progetto finale con il seguente formato
	- Descrizione della portata globale del progetto ricavata dalla specifica dei requisiti
	- Descrizione del progetto dei dati $\rightarrow$ struttura del DB, file esterni, dati interni, riferimenti fra i dati
	- Descrizione dell'architettura con riferimento ai metodi utilizzati per ricavarla, rappresentazione gerarchica dei moduli
	- Progetto delle interfacce interne ed esterne, descrizione dettagliata dell'interazione utente/sistema con eventuale prototipo
	- Descrizione procedurale dei singoli componenti di linguaggio 
- In ogni punto vanno inseriti riferimenti alla specifica dei requisiti da cui quella particolare parte del progetto e' ricavata  va indicato quando le specifiche progettuali dipendono da un vincolo del sistema
- Nella descrizione delle interfacce va inclusa una prima versione della documentazione di collaudo con i test utili per stabilire la corretta implementazione dell'interfaccia
- In coda alla documentazione vanno aggiunti dati supplementari quali descrizione dettagliata degli algoritmi, eventuali alternative di realizzazione e una bibliografia che punti a tutti i documenti sviluppati nella fase di progetto 