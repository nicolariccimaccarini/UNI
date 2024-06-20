
## Database AZIENDA
- Richieste sintetiche dell'Azienda (Analisi Requisiti)
	- L'azienda e' organizzata in DIPARTIMENTI. Ogni dipartimento ha un nome, un numero ed un dipendente che lo dirige (direttore). Occorre gestire la data di inizio incarico del direttore del dipartimento.
	- Ogni dipartimento controlla un certo numero di PROGETTI. Ciascun progetto ha un nome, un numero e viene svolto in un singolo luogo (finale?).
	- Occorre memorizzare il codice fiscale, l'indirizzo, lo stipendio, il sesso e la data di nascita per ciascun DIPENDENTE. Ciascun dipendente lavora per un solo dipartimento ma puo' essere impiegato in piu' di un progetto. Occorre gestire il numero di ore settimanali che ciascun dipendente impiega in ciascun progetto assegnatoli. Occorre gestire anche il supervisore di ciascun dipendente.

## Concetti del Modello ER

### Entita' ed Attributi
- Le **entità** sono semplici oggetti o cose del mini-mondo che sono rappresentate dal database. Per esempio i DIPENDENTI, i DIPARTIMENTI, i PROGETTI.
- Uno specifico dipendenti (e.g. Mario Rossi) viene definito un'**istanza** dell'entita' DIPENDENTI.
- Gli **attributi** sono proprieta' usate per descrivere un'entita'. Per esempio i DIPENDENTI possono avere un Nome, un Codice Fiscale, un Indirizzo, etc...
- Un *istanza* di un'entita' avra' un **valore** per ciascuno degli attributi. Ad esempio uno specifico dipendente puo' avere Nome=‘Mario Rossi’, CF=‘MRRSSU73D24D548V’, Indirizzo=‘Via Paradiso 12’, ...
- Ciascun attributo ha un insieme di valori (o **tipo di dati**) associato ad esso. Esempio: intero, stringa, data, ...

#### Tipi di attributi
- **Semplici**:
	- Ciascuna istanza ha un valore singolo, atomico per attributo. Ad esempio CF, Sesso, ...
- **Composti ( )**:
	- L'attributo puo' essere composto da varie componenti. Per esempio, Indirizzo (Via/Viale/Piazza, nome, numero, cap, paese) oppure Nome (Nome proprio, Cognome). La composizione può anche formare gerarchie in cui alcune componenti sono loro stesse composte.
- **Multivalore { }**: Un'istanza puo' avere multipli per attributo. Ad esempio TitoloDiStudio. Denotati come {TitoloDiStudio}.

### Entità, Istanze e Attributi chiave
- Le **Entità** Vengono anche chiamate Tipi di Entità ed ogni elemento in questa entità viene chiamato **Istanza dell'Entità**. In questo contesto si ha la corrispondenza `[Entità, Istanza]` $\approx$ `[Tipo di Entità, Entità]`.
- In ogni Entità esiste un Attributo che il cui valore e' distinto (cioe' unico) per ogni singola istanza (elemento) dell'Entità.
- Un Attributo con questa caratteristica e' detto **Attributo chiave**. Ad esempio, CF e DIPENDENTI.
- Il valore dell'Attributo chiave e' utile per identificare univocamente una singola istanza dell'Entità.
- Ad esempio, Targa puo' essere chiave dell'entita' AUTO ed avere le componenti (Provincia, Numero). 
- Un'entita' puo' avere piu' di una chiave. Ad esempio, l'entita' AUTO puo' avere due chiavi: Targa e NumeroTelaio.

## Notazione ER
![[NotazioneER.png]]

### Diagramma ER AZIENDA
- Entità:
	- DIPENDENTE 
	- DIPARTIMENTO
	- PROGETTO

![[DiagrammaER_AZIENDA.png]]

### Associazioni ed Istanze di Associazioni
- Un'associazione mette in riferimento due o piu' entita' con uno specifico significato.
- Ogni istanza di associazione mette in riferimento due o piu' istanze di entita'
- Ad esempio, DIPENDENTE Mario Rossi *lavora* sul PROGETTO Database, oppure il DIPENDENTE Mario Rossi *dirige* il DIPARTIMENTO Database.
- Le associazioni si indicano generalmente con un verbo e le entita' che vengono messe in riferimento si dice **partecipano** all'associazione. Ad esempio, DIPENDENTE e PROGETTO partecipano all'associazione LAVORA SU.
- Il **grado** di un'associazione e' il numero di entita' partecipanti. LAVORA SU e' un associazione binaria (grado 2)
- Le associazioni di grado due (binarie) sono le piu' comuni, meno comuni sono quelle di grado 3, mentre quelle che vengono usate di meno sono quelle di grado 4/5.
- Più di un’associazione può avere le stesse entità come partecipanti. Ad esempio DIRIGE e LAVORA PER sono due associazioni distinte tra DIPENDENTI e DIPARTIMENTI, ma con significati diversi e istanze di associazione differenti.

### Entita' deboli ed Associazioni Identificanti
- Un'entita' e' definita **debole** se non possiede un attributo chiave.
- Un'entita' debole *deve* partecipare ad un'**associazione identificante** con un'altra entita' (non debole) identificante `[proprietario]`.
- Le sue istanze sono identificate da una combinazione di:
	- Valore di una chiave parziale dell'entita' debole;
	- Istanza dell'entita' identificante a cui e' associata (tramite associazione identificante).

Esempio:
	Le istanze dell'entita' PARENTE possono essere identificate dal Nome e dalla DataDiNascita, e dall'istanze dell'entita' DIPENDENTE a cui sono associate. PARENTE e' quindi un'entita' debole con DIPENDENTE come entita' identificante attraverso l'associazione identificante PARENTE DI.

### Vincoli sulle Associazioni
Vincoli sui rapporti di cardinalita':
- **Cardinalita' Massima**
	- Uno a uno (1:1)
	- Uno a molti (1:N) or Molti a uno (N:1)
	- Molti a molti (N:M)
- **Cardinalita' Minima** 
  (chiamato anche vincolo di partecipazione o vincolo di dipendenza di esistenza)
	- zero  (partecipazione opzionale, non dipendenza di esistenza)
	- uno o piu' (partecipazione obbligatoria, dipendenza di esistenza)

### Associazioni ed Istanze di Associazioni
- Si possono esprimere anche associazioni **ricorsive**.
- Ambedue i partecipanti sono la stessa entita', ma con ruoli diversi.
- Ad esempio, COMANDATO DA e' un'associazione ricorsiva con DIPENDENTE; un'istanza di DIPENDENTE (nel ruolo di subordinato, lavoratore) e' associata ad un'altra istanza di DIPENDENTE (nel ruolo di supervisore, boss, capo).
- Nei diagrammi ER e' necessario specificare i nomi dei ruoli che riveste l'entita' partecipante in modo da permettere le distinzioni del caso
### Istanze dell'associazione LAVORA PER
![[LavoraPer.png]]

### Istanze dell'associazione LAVORA SU
![[LavoraSu.png]]

### Associazione Ricorsiva COMANDATO DA
![[ComandatoDa.png]]

### Attributi delle Associazioni
- Un'associazione puo' avere degli **attributi**;
  ad esempio, OreSettimanali in LAVORA SU; il valore assunto in ciascuna istanza dell'associazione descrive il numero di ore settimanali che un dipendente ha lavorato su un progetto.
- L'attributo puo' essere spostato ad una qualsiasi delle entita' partecipanti nel caso l'associazione abbia rapporto di cardinalita' 1:1.
- L'attributo puo' essere spostato all'istanza partecipante di cardinalita' maggiore nel caso l'associazione abbia rapporto di cardinalita' 1:N.
- L'attributo deve essere proprio dell'associazioone nel caso si abbia rapporto di cardinalita' N:M.

### Vincoli strutturali
- **Rapporto di cardinalità** (di associazioni binarie)
		  *1:1*, *1:N*, *N:1*, oppure *M:N* 
	- MOSTRATO INDICANDO IL NUMERO APPROPRIATO SUL SEGMENTO TRA ENTITA' E ASSOCIAZIONE
- **Vincolo di partecipazione** (su ciascuna entita' partecipante)
		  *Totale* (dipendenza di esistenza) oppure *parziale*
	- MOSTRATO UTILIZZANDO UN SEGMENTO DOPPIO

## Notazione alternativa (min, max)
- Specificata su tutte le partecipazioni di un'entita' E da un'associazione R.
- Specifica che ciascuna istanza *e* dell'entita' E partecipa in almeno *min* ed al massimo *max* istanze di associazione dell'associazione R.
- Default (no vincoli): min = 0, max = n.
- Deve essere $min ≤ max, min ≥ 0, max ≥ 1$
- Derivata dalla conoscenza dei vincoli del mini-mondo. 

Esempio
- Un dipartimento ha esattamente un direttore e un dipendente puo' dirigere (al massimo) u solo dipartimento.
	- Specificare (0,1) per la partecipazione di DIPENDENTE in DIRIGE
	- Specificare (1,1) per la partecipazione di DIPARTIMENTO in DIRIGE
![[NotazioneMinMax1.png]]

- Un dipendente puo' lavorare per uno e un solo dipartimento ma un dipartimento puo' avere un numero qualsiasi di dipendenti.
	- Specificare (1,1) per la partecipazione di DIPENDENTE in LAVORA PER
	- Specificare (0,n) per la partecipazione di DIPARTIMENTO in LAVORA PER
![[NotazioneMinMax2.png]]

## Associazioni di grado superiore
- Associazioni di grado 2 sono dette binarie
- Associazioni di grado 3 sono dette ternarie ed associazioni di grado *n* sono dette *n-arie*.
- In generale, un'associazione *n-aria* **NON** equivale a *n* associazioni binarie!
- Associazioni di ordine superiore a due saranno discusse con il modello EER (o UML)

## Data Modelling Tools
- Esistono numerosi programmi per modellare concettualmente il database e fare un mapping allo schema relazionale: ERWin, S-Designer, ER-Studio, ...
- VANTAGGI: utili per documentare le richieste dell'applicazione; forniscono interfacce grafiche.

## Problemi principali
- DISEGNO
	- Notazione concettuale poco significativa
	- Per evitare problemi di estetica e di algoritmi per la disposizione, sono disponibili essenzialmente solo rettangoli e linee e non rappresentano niente di piu' che associazioni e vincoli su chiave primaria-esterna nelle tabelle risultanti.
- METODOLOGIA
	- Scarso supporto interno
	- Scarsi strumenti per la verifica dei diagrammi o per suggerimenti.

## Problemi con il modello ER
- Il modello Entita'-Associazione nella sua forma originale non supporta astrazioni molto utili quali:
	- Sottoclassi
	- Specializzazione/generalizzazione

## Modello EER (Extended Entity-Relationship)
- Incorpora associazioni di sottoclasse
- Incorpora gerarchie di Specializzazione/Generalizzazione.