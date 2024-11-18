## Di cosa si tratta
- Vogliamo garantire che il sistema software sia privo di errori e difetti
- Nelle prime fasi del processo di sviluppo software si passa da una visione astratta ad una implementazione concreta
- A questo punto bisogna iniziare una serie di casi di prova destinati a "demolire" il software realizzato
- Il collaudo e' l'unico passo del processo di produzione che si puo' considerare distruttivo invece che costruttivo

Esistono varie strategie di testing, in generale hanno tutte queste caratteristiche:
- Fate delle technical review $\rightarrow$ in questo modo molti errori sono eliminati prima ancora di arrivare a farei test
- I test cominciano a "component level" e si vanno ingrandendo fino a riguardare l'integrazione dell'interno del sistema
- Tecniche di testing diverse sono appropriate a modi diversi di fare il testing a tempi diversi nel ciclo del progetto
- Il testing e' fatto dagli sviluppatori e da un gruppo indipendente di testing
- Testing $\ne$ debugging $\rightarrow$ ma una strategia di testing prevede il debugging

## Verifica & Validazione
- **Verifica: stiamo costruendo il prodotto nel modo giusto?**
	- il sw deve essere conforme alle specifiche, cioe' deve comportarsi esattamente come era previsto
- **Validazione: stiamo costruendo nel modo giusto?**
	- il sw deve essere implementato in modo da soddisfare quello che l'utente realmente richiede

- Sono domande che devono percorre tutto il ciclo di vita del software, per correggere errori e per valutare se il prodotto e' usabile dal punto di vista operativo
- V & V deve stabilire con un buon grado di confidenza che il software non e' adeguato allo scopo del progetto
- Questo NON significa completamente privo di difetti
- Piuttosto, che e' sufficientemente buono per lo scopo previsto, e il tipo di confidenza richiesto dipende dalla funzione, dalle aspettative degli utenti, dall'attuale ambiente di mercato del sistema

### Verifica statica/dinamica
- **Verifica statica** riguarda l'analisi di una rappresentazione statica del sistema per individuare i problemi
	- si puo' fare in ogni fase del processo di sviluppo, anche prima che il sistema sia implementato
- **Verifica e validazione dinamici** consente nel testare il prodotto facendo prove e osservandone il comportamento
	- richiede l'esistenza di un prototipo eseguibile del sistema su cui effettuare i test
	- la validazione puo' essere solo dinamica $\rightarrow$ non si puo' essere sicuri che un sistema soddisfi i requisiti del cliente semplicemente guardando la struttura, senza eseguirlo

## Testing
- Il collaudo consiste nell'eseguire un programma al fine di scoprire un errore
- Un caso di prova e' valido se ha un'alta probabilita' di scoprire un errore ancora ingoto
- Un test (collaudo) e' riuscito se ha scoperto un errore prima ignoto
- Requisiti non funzionali non possono essere verificati, solo validati
- Il test dei programmi va usato assieme alla verifica statica

### Principi
- Ogni singola prova deve essere riconducibile ai requisiti del cliente
	- nell'ottica del cliente, i difetti piu' gravi sono quelle che impediscono al programma di soddisfare i requisiti
- I collaudi vanno pianificati con largo anticipo
	- la pianificazione dei collaudi puo' iniziare appena e' completata la definizione e la specifica dei requisiti; la definizione dei casi di prova puo' cominciare non appena il modello progettuale e' stabile
- Il principio di Pareto si applica anche al collaudo del software
	- Principio di Pareto $\rightarrow$ l'80% degli errori scoperti e' riconducibile al 20% dei componenti del programma; il problema e' identificare e isolare i componenti sospetti
- I collaudi devono cominciare in piccolo, e proseguire verso collaudi in grande
	- inizialmente si testano le singole componenti, poi si passa a blocchi di componenti e si risale fino all'interno del sistema
- Un collaudo esauriente spesso e' impossibile
	- impossibile provare tutte le possibili esecuzioni del programma $\rightarrow$ gia' sistemi piccoli hanno un numero elevato di percorsi di esecuzione
- Per essere efficace il collaudo andrebbe fatto da terze parti, non da chi ha scritto il codice

### Collaudabilità
Con questo termine intendiamo la facilita' con cui un sistema puo' essere provato. Collaudare un sistema e' difficile: cosa possiamo fare per facilitare il compito?

- **Operabilità** $\rightarrow$ meglio funziona, meglio puo' essere collaudato
	- il sistema contiene pochi errori
	- gli errori non impediscono l'esecuzione dei collaudi
	- il prodotto si evolve per stadi
- **Osservabilità** $\rightarrow$ cio' che vedi e' cio' che collaudi
	- input e output sono chiaramente correlati
	- stati e variabili sono osservabili durante l'esecuzione
	- l'output dipende da fattori ben individuabili
	- l'output errato e' di facile individuazione
	- errori interni vengono identificati automaticamente e riferiti
- **Controllabilità** $\rightarrow$ quanto piu' possiamo controllare il sw, piu' il collaudo puo' essere automatizzato e ottimizzato
	- tutti i dati di output sono generabili mediante opportuno input
	- tutto il codice e' eseguibile mediante opportuno input
- **Scomponibilità** $\rightarrow$ i moduli devono essere collaudati separatamente
	- il sw e' composto da moduli indipendenti
	- ogni modulo puo' essere collaudato da solo
- **Semplicità** $\rightarrow$ meno cose ci sono da collaudare, piu' velocemente si svolgono i collaudi
	- semplicita' funzionale $\rightarrow$ funzionalita' ridotta al minimo necessario
	- semplicita' strutturale $\rightarrow$ es. architettura modulare
	- semplicita' del codice $\rightarrow$ standard unico di codifica
- **Stabilita'** $\rightarrow$ meno modifiche si apportano, meno situazioni devono essere collaudate
	- le modifiche al sw sono rare e controllate
	- le modifiche non inficiano i collaudi gia' svolti
- **Comprensibilità** $\rightarrow$ piu' informazioni abbiamo, piu' adeguati sono i collaudi che possiamo svolgere
	- il progetto e' chiaro
	- le dipendenze tra componenti sono chiare e ben descritte
	- le modifiche al progetto sono rese pubbliche
	- la documentazione tecnica e':
		- facilmente accessibile
		- ben organizzata
		- specifica e dettagliata
		- accurata

#### Tipi di collaudo
- **Collaudo white-box**
	- si parte dalla conoscenza della struttura interna del prodotto: il collaudo vuole verificare che le strutture interne funzionino secondo le specifiche progettuali testando al contempo tutti i componenti
	- controllo meticoloso degli aspetti procedurali
- **Collaudo black-box**
	- si parte dalle specifiche del progetto e si va a verificare che tutte le funzionalita' richieste nei requisiti siano presenti, cercando allo stesso tempo eventuali errori
	- non mi interessa la logica interna, svolge le sue funzioni?


##### Collaudo white-box
- I casi di prova si ricavano dalla struttura di controllo del progetto procedurale in modo da
	- garantire che tutti i cammini indipendenti dentro ciascun modulo siano eseguiti almeno una volta
	- eseguire sia il ramo vero che quello falso di ciascuna condizione
	- eseguire tutti i cicli nei casi limite ed entro i confini operativi
	- esaminare la validita' di tutte le strutture dati interne

### I grafi di flusso
Sono una rappresentazione strutturale del programma che si focalizza sui possibili flussi di esecuzione

![[grafiDiFlusso.png]]

#### Condizioni logiche complesse nei grafi di flusso
Una condizione logica puo' includere piu' di un operatore booleano: questo complica un po' la situazione ma e' comunque rappresentabile

![[condLogCompGrafFlusso.png]]

### Complessita' ciclomatica
- **Complessità ciclomatica** $\rightarrow$ misura quantitativa della complessita' logica di un programma
- Misura il numero di cammini indipendenti in un grafo
	- indipendente $\rightarrow$ che introduce almeno un lato non ancora esplorato o una nuova condizione
- Esistono 3 modi equivalenti per calcolare la complessita' ciclomatica di un grafo $V(G)$
	- $V(G) = R$ $\rightarrow$ dove $R$ e' il numero delle regioni
	- $V(G) = E - N + 2$ $\rightarrow$ dove $E$ e' il numero dei lati e $N$ il numero dei nodi
	- $V(G) = P + 1$ $\rightarrow$dove $P$ e' il numero di diramazioni

### Testing della struttura di controllo
- Lo studio dei cammini di base e della complessita' ciclomatica fa parte di una famiglia di tecniche per il testing della struttura di controllo
- Vi sono altre tecniche che aumentano la portata del test
- Fanno sempre parte dell'approccio white-box

#### Collaudo per condizioni
- Il collaudo per condizioni si focalizza sui test di correttezza delle condizioni logiche del programma
	- **Condizione logica semplice**: $X_1 \text{<operatore relazionale>} X_2$ dove l'operatore relazionale puo' essere $<, \space \le, \space =, \space \ge, \space >, \space \ne$ e $X_1 \space X_2$ sono espressioni aritmetiche 
	- **Condizione logica composta**: combinazione di condizioni logiche semplici usando operatori booleani OR, AND, NOT e strutturate mediante parentesi
	- Se una condizione non comprende espressioni relazionali viene chiamata *espressione booleana*
- Errori in una condizione possono essere causati da
	- errori negli operatori booleani
	- errori in una variabile booleana
	- errori nella costruzione della condizione
	- errori in un operatore relazionale
	- errori in una espressione aritmetica
- Il collaudo per condizioni si propone di testare tutte le condizioni di un programma: si assume che questo sia utile per individuare errori anche nelle altre parti

#### Collaudo per cicli
- Con collaudo per cicli ci concentriamo su test che verifichino la validita' dei loop del programma
- Cicli semplici (n = numero di esecuzioni)
	- saltare un ciclo (1 prova)
	- percorrere il ciclo 0, 1, 2 volte (3 prove)
	- percorrere il ciclo $m$ volte con $m<n$ (1 prova)
	- percorrere il ciclo $n-1, n, n+1$ (3 prove)
- Cicli annidati
	- fissare i cicli esterni ai valori minimi e testare il ciclo interno come se fosse un ciclo semplice
	- procedere verso l'esterno mantenendo quelli piu' esterni al minimo e quelli piu' interni a un valore prefissato
- Cicli concatenati
	- in generale si possono considerare come semplici
	- se pero' il contatore di ciclo del primo viene usato come valore inziale del secondo, non sono indipendenti
- Cicli non strutturati
	- e' impossibile testare come si deve dei cicli non strutturati: e' meglio riprogettarli

#### Collaudo black-box
- Questo tipo di collaudo complementa il white-box, occupandosi di errori dovuti a 
	- funzioni errate o mancanti
	- errori di interfaccia
	- errori nelle strutture dati o nell'accesso a DB esterne
	- comportamento erroneo o problemi prestazionali
	- errori nelle fasi di inizializzazione o terminazione
- Contrariamente alle tecniche white-box questo collaudo viene effettuato nelle fasi finali dell'implementazione
- Bisogna individuare casi di prova che 
	- siano significativi in termini di collaudo (riducano ulteriori test)
	- individuino classi di errori piuttosto che singoli errori
- Prevede di selezionare l'insieme dei dati di input che costituira' il test a partire dalle funzionalita' di un programma
- Vari criteri, a seconda delle regole con cui sono individuati i casi rilevanti che costruiranno la materia del test
	- **Statistico** $\rightarrow$ si provano gli input piu' probabili (profilo operazionale)
	- **Partizione dati di input** $\rightarrow$ dominio dati input e' ripartito in classi di equivalenza. Valido se il numero di comportamenti e' ragionevole
	- **Valori di frontiera** $\rightarrow$ classi equivalenza o causa uguaglianza comportamento o in base a considerazioni inerenti il tipo di valori. Prendiamo gli estremi di queste classi.
	- **Grafo causa-effetto** $\rightarrow$ se possiamo costruire dai requisiti o dalle specifiche un grafo che lega input (cause) e output (effetto) in una rete combinatoria che definisce delle relazioni di cause-effetto

### Black-box vs White-box

| Black box                                                                                        | White box                                                                                       |     |
| ------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------- | --- |
| Considera solo il comportamento esterno del sistema; non si considera come e' fatto internamente | Si considera il funzionamento interno del software nel fare il test                             |     |
| E' svolto da testers                                                                             | E' svolto dagli sviluppatori stessi                                                             |     |
| Si usa nel system testing o nel acceptance testing (si usa nelle fasi finali fi testing)         | Si usa molto nel unit testing e nel integration testing (si comincia a fare testing con questo) |     |
| E' piu' rapido del white box                                                                     | Richiede piu' tempo del black box                                                               |     |
| E' il test del comportamento                                                                     | E' il test della logica del funzionamento                                                       |     |
| Non va bene per testare un algoritmo                                                             | E' adatto per testare algoritmi                                                                 |     |
### Testing & Debugging
- Testing $\rightarrow$ conferma la presenza di errori
- Debugging $\rightarrow$ localizzazione e correzione degli errori

### Strategie di collaudo
- La strategia di collaudo del software utilizza le stesse tecniche di collaudo sviluppate per la procedura di collaudo pianificata di un generico processo di produzione
- Bisogna
	- definire il piano generale di collaudo
	- allocare le risorse necessarie al collaudo
	- progettare i casi di prova
	- definire le verifiche sui risultati
	- raccogliere e valutare i risultati dei collaudi

### Organizzazione dei collaudi
- Analisi e progettazione sono compiti intrinsecamente costruttivi mentre il collaudo viene visto come distruttivo
- Lo sviluppo potrebbe esser psicologicamente condizionato nell'esecuzione del collaudo
- Affiancare un gruppo indipendente allo sviluppatore nei collaudi globali sul sistema
- Lo sviluppatore deve collaborare col team di collaudo sia in fase di test, sia per correggere gli errori individuati durante i collaudi

### Valutazione dei test
- Per valutare quanto efficace e' il testing che stiamo facendo usiamo il concetto di "copertura", e distinguiamo tra:
	- Copertura funzionale $\rightarrow$ percentuale di funzionalita' testate rispetto al numero totale di funzionalita' del software
	- Copertura strutturale $\rightarrow$ percentuale di elementi testati rispetto al numero totale di elementi che compongono il codice. Possiamo pensare in termini di flusso di controllo o flusso di dati
		- Flusso di controllo:
			- copertura dei comandi
			- copertura dei cammini
		- Flusso dei dati:
			- copertura delle definizioni
			- copertura degli usi

### Quando finisce un collaudo?
- Risposta filosofica
	- Mai $\rightarrow$ semplicemente da un certo punto in poi sara' il cliente a fare il collaudatore
- Risposta pragmatica
	- Il collaudo finisce quando finiscono le risorse allocate per farlo (tempo, persone, soldi)
- Risposta quantitativa
	- Il collaudo finisce quando la probabilita' di verificarsi di un bug per unita' di tempo d'uso scende sotto una soglia prefissata

Esistono metodi statistici per calcolare questa probabilita' in funziona dei numeri di guasti durante i collaudi.

### Criteri di collaudo
- Specificare quantitativamente i requisiti del prodotto prima di iniziare i collaudi
- Enunciare in maniera esplicita e quantitativa gli obiettivi del collaudo (MTBF finale, costi, ...)
- Sviluppare i profili di tutte le categorie di utenti
	- porta alla limitazione dei casi d'uso da controllare
- Sviluppare un piano basato su cicli rapidi
	- cominciare dalle funzionalita' principali
- Costruire sw dotato di sistemi di autodiagnosi
- Definire metriche strategiche di collaudo

### Strategie di testing
- Testing Top-Down
	- i test cominciano dalle componenti piu' astratte e si spostano via via verso le componenti di basso livello
- Testing Bottom-Up
	- i test cominciano dalle componenti di basso livello e si spostano via via verso le componenti di alto livello
- Collaudo per regressione
	- si usa per testare gli incrementi e le modifiche
- Stress e testing
	- utilizzato per testare come il sistema reagisce ai sovraccarichi
- Back-to-back testing
	- utilizzato quando sono disponibili piu' versioni diverse dello stesso sistema

#### Test Top-Down
- Primo passo $\rightarrow$ si testa il modulo radice sostituendo quelli di livello inferiore con degli stub
- Secondo passo $\rightarrow$ si includono i moduli del primo livello e si sostituiscono quelli di livello inferiore degli stub
- Inizia dai livelli piu' elevati del sistema e procede verso il basso
- Questa strategia di test puo' essere facilmente usata in parallelo all sviluppo top-down
- Puo' essere difficile individuare errori che dipendono dal livello piu' basso perche' sono sviluppati per ultimi e gli stub passano dati non significativi al livello superiore
- Puo' individuare problemi architetturali fin dall'inizio
- Sviluppare stub puo' essere complicato

##### Casi possibili di "stub"
- Lo stub non fa nulla
	- eventualmente stampa una traccia della chiamata e dei parametri
- Lo stub colloquia con il programmatore per restituire un valore particolare
- Lo stub e' una versione semplificata (un prototipo) del modulo che verra' chiamato

#### Test Bottom-Up
- Si parte dai moduli di livello piu' basso sviluppando dei driver che li usano e ne verificano il comportamento
- Successivamente si continua a salire verso la radice della gerarchia, aggiungendo livello e testandoli con nuovi driver
- Puo' esser utile per testare le componenti critiche di basso livello di un sistema
	- i vantaggi del bottom-up sono gli svantaggi del top-down e viceversa
- Si inizia dai livelli piu' bassi e ci si muove verso la radice dei componenti
- E necessario implementare dei test driver
- Eventuali problemi di design sono scoperti relativamente tardi
- E' una tecnica che risulta appropriata per sistemi object-oriented

#### Back-to-back testing
- Testa diverse versioni del programma con lo stesso input e confronta gli output
	- se l'output e' diverso ci sono errori potenziali
- Riduce il costo di esaminare il risultato dei test: il confronto degli output puo' essere automatizzato
- Si puo' usare quando e' disponibile un prototipo, quando un sistema viene sviluppato in piu' versioni (magari su diverse piattaforme), oppure nel caso di upgrade o nuove release del sistema

#### Collaudo incrementale / Continuous Integration
- Ad ogni introduzione di un nuovo modulo il software cambia
	- sono possibili nuovi cammini di flusso, nuove interfacce, ...
- Questo puo' introdurre problemi anche nelle parti gia' collaudate prima dell'aggiunta del nuovo metodo
- Occorre ripetere le prove gia' fatte per verificare che una modifica non abbia portato effetti collaterali
	- va fatto ad ogni modifica: nuovo modulo, correzione di un bug, aggiornamento
- Si possono usare strumenti automatici per registrare alcune prove chiave e poi riprodurle in automatico in fase di collaudo

#### Smoke-testing
- quando i tempi di un nuovo incremento sono brevi, si puo' inserire il collaudo nel corso dello sviluppo del codice
- I moduli via via prodotti vengono integrati in build che implementano una o piu' delle funzioni del programma
- Per ogni build vengono identificati dei test chiave che ne verificano il funzionamento 
- Tutti i build vengono integrati e il programma risultante viene testato quotidianamente
- Se si identificano degli errori prima inesistenti, il codice aggiunto per ultimo e' maggiore indiziato come causa
- Con questo metodo si ha una misura continuamente aggiornata dei progressi nello sviluppo del programma

##### Vantaggi dello smoke-test
- Minimizza i rischi di integrazione
	- un rischio classico nello sviluppo di grossi progetti consiste nel trovare difficolta' inaspettate all'atto di integrazione di varie componenti: con questo metodo l'integrazione viene fatta quotidianamente e monitorata
- Riduce i rischi di produrre codice di bassa qualita'
	- la build viene controllata ogni giorno
- E' facile diagnosticare i problemi
	- di solito un controllo su quello che e' stato cambiato o aggiunto qual giorno e' sufficiente per trovare l'errore
- Migliora il morale del team di sviluppo
	- si vede che il prodotto che si sta costruendo funziona effettivamente

#### Recovery testing
- Molti sistemi informatici devono recuperare la situazione dopo un malfunzionamento e riprendere l'elaborazione in un tempo determinato
- In alcuni casi il sistema deve esser tollerante ai guasti; in altri deve correggere il malfunzionamento in un tempo predefinito
- Si forza il software a incorrere in errore in molti modi diversi e si verifica che il recupero sia seguito correttamente
	- se il recupero richiede l'intervento umano, si valuta il tempo medio necessario

#### Security testing
- Molti sistemi devono essere protetti contro un uso non autorizzato oppure contro un accesso non autorizzato delle risorse
- Si cerca di verificare i meccanismi di protezione costruiti all'interno del sistema
- Avendo tempo e risorse, un buon security testing riuscira' a violare il sistema: l'obiettivo e' avere un sistema che richieda un tempo o risorse tali per cui il costo per violare il sistema e' superiore al guadagno

#### Stress testing
- Gli stress testing sono progettati per provare i programmi in situazioni di carico eccessivo
	- generare 10 richieste al secondo a fronte di una media stimata di 2
	- progettare casi di utilizzo che richiedono il massimo della memoria e il massimo di altre risorse possibili
- Sostanzialmente si cerca di far crollare il programma
- Se si sta testando degli algoritmi matematici si usa i sensitivity test: valori dei parametri molto vicini ai limiti di validita' dell'algoritmo
	- lo scopo e' cercare combinazioni di dati che, anche se validi come input, possono causare instabilita' o elaborazioni non corrette

### Prove di validazione
- I criteri di validazione sono contenuti nelle specifiche dei requisiti: descrivono le ragionevoli aspettative del cliente
	- Un criterio di validazione spesso trascurato: verificare che la configurazione sw sia opportunamente catalogata e documentata per garantire il supporto futuro
- Il modo migliore di effettuare la validazione consiste nel far usare il sistema ad un insieme ridotto di utenti per verificarne il comportamento in situazioni reali
	- alpha version $\rightarrow$ gli utenti usano il sistema sotto il diretto controllo degli svilupparori
	- beta version $\rightarrow$ gli utenti usano il sistema da soli e redigono un report con errori e problemi individuati

### Il debugging
Lo scopo e' eliminare gli errori individuati durante il collaudo del sistema 

![[debugging.png]]

- Metodi principali di debugging:
	- forza bruta $\rightarrow$ dump della memoria, messaggi di debug, ... di solito si tengono come ultima ratio
	- cammino a ritroso $\rightarrow$ si parte dal punto in cui si e' manifestato l'errore e si procede a ritroso; per programmi piccoli 
	- eliminazione delle cause $\rightarrow$ metodo scientifico in cui si formulano delle ipotesi e si inventano test per verificarle

### La revisione di progetto
- Si basa su riunioni di revisione
- Perche' funzionino vanno pianificate e gestite
	- numero ridotto di partecipanti
	- documentazione scritta dal progettista disponibile prima della riunione
	- durata prestabilita e limitata dal meeting
	- discussione focalizzata sulla scoperta dei problemi, non sulla correzione
	- persone coinvolte: progettista, moderatore, verbalizzatore
	- atmosfera cooperativa $\Rightarrow$ no managers

### Walkthrough
- Obiettivo
	- rilevare la presenza di difetti
	- eseguire una lettura critica del codice
- Agenti
	- gruppi misti ispettori/sviluppatori
- Strategia
	- percorrere il codice simulandone l'esecuzione

#### Fasi del walkthrough
1. Pianificazione
2. Lettura del codice
3. Discussione
4. Correzione dei difetti
5. Documentazione

### Revisione vs. walkthrough
- Similitudini
	- controllo statistici basati su desk-test
	- contrapposizione fra programmatori e verificatori
	- documentazione formale
- Differenze
	- revisione si basa su errori presupposti
	- walkthrough si basa sull'esperienza dei verificatori
	- walkthrough e' piu' collaborativo
	- revisione e' piu' rapido