## Concetti del modello EER
- Include tutti i concetti di modellazione del modello ER
- Concetti addizionali: sottoclassi/superclassi, specializzazione, categorie, propagazione (ereditarieta') degli attributi
- Il modello risultante e; chiamato Modello Enhanced-ER o Extended ER (E2R or EER)
- E' utilizzato per modellare applicazioni in maniera piu' accurata e piu' specifica
- Include alcuni concetti derivati dalla programmazione ad oggetti come l'ereditarieta'

## Sottoclassi e Superclassi
- Un'entita' potrebbe avere alcuni sottogruppi addizionali di istanze, aventi un significato particolare per il mini-mondo di interesse.
- Esempio: DIPENDENTE potrebbe essere ulteriormente suddiviso in SEGRETARIO, INGEGNERE, DIRETTORE, TECNICO, TEMPORANEO, TEMPO_INDETERMINATO, ...
	- Ciascuno di questi gruppi e' costituito da un *sottoinsieme* delle *istanze* dell'entita' DIPENDENTE
	- Ciascuno di questi gruppi viene chiamato una **sottoclasse** di DIPENDENTE
	- DIPENDENTE e' chiamata **superclasse** di ciascuna di queste sottoclassi
- Queste sono dette associazioni di superclasse/sottoclasse
- Esempio: DIPENDENTE/SEGRETARIO, DIPENDENTE/TECNICO

- Sono anche chiamate associazioni E'-UN (IS-A): SEGRETARIO E'-UN DIPENDENTE, TECNICO E'-UN DIPENDENTE, ...
- Nota: Un'istanza membro di una sottoclasse rappresenta la *stessa istanza del mini-mondo* di un qualche membro della superclasse.
	- Il membro della sottoclasse e' la stessa istanza in n (distinto) ruolo specifico
	- Un'istanza non puo' esistere nel database solamente in qualita' di membro di una sottoclasse; deve essere membro della superclasse
	- Un membro della superclasse puo' essere eventualmente incluso nei membri di un qualsiasi numero delle sue sottoclassi
- Esempio: Un dipendente temporaneo che sia anche un ingegnere appartiene sia alla sottoclasse INGEGNERE che a TEMPORANEO
	- Non e' necessario che tutte le istanze di una superclasse siano membri di qualche sottoclasse

## Ereditarieta' degli Attributi
- Un entita' che sia membro di una sottoclasse eredita tutti gli attributi dell'entita' considerata come membro della superclasse.
- Eredita inoltre anche tutte le associazioni.

## Specializzazione
- **Specializzazione**: il processo di definizione di un *insieme* di *sottoclassi* di una *superclasse*.
- L'insieme di sottoclassi e' basato su alcune caratteristiche peculiari delle istanze della superclasse.
- Esempio: {SEGRETARIO, INGEGNERE, TECNICO} e' una specializzazione di DIPENDENTE basato sul tipo di lavoro.
	- Ci possono essere diverse specializzazioni della stessa superclasse.
- Esempio: Un'altra specializzazione di DIPENDENTE basato sul tipo di contratto di lavoro e' {TEMPORANEO, TEMPO INDETERMINATO}.
	- Le associazioni superclasse/sottoclasse e la specializzazione puo' essere rappresentata graficamente nei diagrammi EER
	- Gli attributi di una sottoclasse sono detti **attributi specifici**.
	- Ad esempio, VelocitàBattitura di SEGRETARIO
	- Una sottoclasse puo' partecipare a specifiche associazioni (a cui non partecipa la superclasse)

### Esempio di specializzazione
![[EsemioSpecializzazione.png]]

## Generalizzazione
- Il processo inverso rispetto alla specializzazione
- Alcune classi con caratteristiche comuni vengono generalizzate in una superclasse. Le classi originali diventano sottoclassi della superclasse creata.
- Esempio: AUTO, CAMION generalizzazione in VEICOLO; sia AUTO che CAMION diventano sottoclassi della superclasse VEICOLO.
	- Si puo' vedere {AUTO, CAMION} come una specializzazione di VEICOLO
	- Di converso si puo' vedere VEICOLO come una generalizzazione di AUTO e CAMION.

## Generalizzazione e Specializzazione
- A volte vengono usati nei diagrammi alcune notazioni per distinguere tra generalizzazione e specializzazione
	- Frecce dirette verso la superclasse rappresentano una generalizzazione
	- Frecce dirette verso le sottoclassi rappresentano una specializzazione
	- Notazione ambigua: spesso e' molto soggettiva, e soggettivo e' decidere quale processo adottare nella situazione particolare
	- A volte e' meglio non indicare alcuna freccia
- Modello dei Dati con Specializzazione e Generalizzazioni
	- Una superclasse o sottoclasse rappresenta una insieme delle istanze
	- Rappresentazione nei diagrammi EER come rettangoli (come le entita')
	- A volte tutti gli insiemi di istanze vengono chiamati semplicemente classi indipendentemente dal fatto che siano entita', superclassi o sottoclassi.

## Vincoli (sulle Specializzazioni e sulle Generalizzazioni)
- Se e' possibile determinare esattamente le istanze che diventeranno membri di una sottoclasse mediante una *condizione*, le sottoclassi vengono chiamate **definite da un predicato** (o **definite da una condizione**)
	- La condizione e' un vincolo che determina i membri di una sottoclasse 
	- Questo tipo di sottoclassi si indicano scrivendo il *predicato di condizione* vicino alla linea che unisce la sottoclasse alla specializzazione
- Se tutte le sottoclassi di una specializzazione hanno la condizione di appartenenza sullo stesso attributo, la specializzazione e' detta **definita da un attributo**
	- L'attributo e' chiamato **attributo di specializzazione**
	- Esempio: TipoLavoro e' l'attributo che definisce la specializzazione {SEGRETARIO, TECNICO, INGEGNERE} di DIPENDENTE
- Se nessuna condizione determina l'appartenenza, la sottoclasse e' chiamata **definita dall'utente**

- **Vincoli di disgiunzione**:
	- Specifica che le sottoclassi di una specializzazione devono essere disgiunte (un'istanza puo' essere membro di solo una sottoclasse della specializzazione)
	- Denota con una **d** nei diagrammi EER
	- Se non sono disgiunte: sovrapposte. La stessa istanza puo' appartenere a piu' di una sottoclasse della specializzazione
	- Denota con una **o** (overlap) nei diagrammi EER
- **Vincolo di completezza**:
	- **Completezza Totale**: specifica che ogni istanza della superclasse deve appartenere ad almeno una sottoclasse della specializzazione/generalizzazione
		- Denota nei diagrammi EER con una linea doppia
	- **Completezza parziale**: consente alle istanze di non appartenere a nessuna sottoclasse
		- Denota nei diagrammi EER con una linea singola

- Si hanno quindi quattro tipi di generalizzazioni/specializzazioni:
	- Disgiunta, totale
	- Disgiunta, parziale
	- Sovrapposta, totale
	- Sovrapposta, parziale
- Nota: una generalizzazione e' solitamente totale in quanto la superclasse e' derivata da sottoclassi

### Esempio di Specializzazione Disgiunta, Parziale
![[DisgiuntaParziale.png]]

## Gerarchie e Reticoli (di Specializzazione/Generalizzazione)
- Una sottoclasse puo' avere in essa ulteriori sottoclassi specificate su di essa (ed esser quindi superclasse per queste)
- Si puo' formare quindi una **gerarchia** o un **reticolo** (di sottoclassi/superclassi)
- Una **gerarchia** impone il *vincolo* che ciascuna sottoclasse abbia *una sola* superclasse (detta anche *ereditarieta' singola* )
- In un **reticolo** invece una sottoclasse puo' essere sottoclasse *di piu' di una* superclasse (detta anche *ereditarieta' multipla*)
- In un reticolo o in una gerarchia, una sottoclasse eredita gli attributi non solo della superclasse diretta, ma anche delle superclassi precedenti
- Una sottoclasse con piu' di una superclasse (possibile solo in un reticolo) viene chiamata **sottoclasse condivisa**
- Si possono avere *gerarchie* o *reticolo di specializzazione* oppure *gerarchie* o *reticoli di generalizzazione*
- Nelle specializzazioni, si inizia con un'entita' e si definiscono sottoclassi dell'entita' mediante specializzazioni successive(processo di *raffinamento concettuale* **top down**)
- Nelle generalizzazioni, si inizia con alcune entita' e si generalizzano (in successione) quelle che hanno proprieta' comuni (processo di *sintesi concettuale* **bottom-up**)
- In pratica, viene utilizzata molto spesso una combinazione dei due processi


FINIRE LEZIONE 4