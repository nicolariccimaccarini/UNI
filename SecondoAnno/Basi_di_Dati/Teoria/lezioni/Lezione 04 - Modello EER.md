## Concetti del modello EER
- Include tutti i concetti di modellazione del modello ER
- Concetti addizionali: sottoclassi/superclassi, specializzazione, categorie, propagazione (ereditarieta') degli attributi
- Il modello risultante e' chiamato Modello Enhanced-ER o Extended ER (E2R or EER)
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
	- Il membro della sottoclasse e' la stessa istanza in $n$ (distinto) ruolo specifico
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
- Nelle specializzazioni, si inizia con un'entita' e si definiscono sottoclassi dell'entita' mediante specializzazioni successive (processo di *raffinamento concettuale* **top down**)
- Nelle generalizzazioni, si inizia con alcune entita' e si generalizzano (in successione) quelle che hanno proprieta' comuni (processo di *sintesi concettuale* **bottom-up**)
- In pratica, viene utilizzata molto spesso una combinazione dei due processi

![[GerarchieEReticoli.png]]

## Categorie (tipi di UNIONE)
- Tutte le associazioni superclasse/sottoclasse finora viste hanno una sola superclasse
- Anche una sottoclasse condivisa ha in definitiva una singola superclasse per ogni associazione superclasse/sottoclasse a cui appartiene
- In alcuni casi e' necessario modellare i dati in modo tale che una singola associazione superclasse/sottoclasse abbia piu' di una superclasse
- Le superclassi in questo caso rappresentano differenti entita' (con ruoli e caratteristiche differenti)
- Questo tipo di sottoclassi vengono chiamate **categorie** oppure **tipi di UNIONE**
- Esempio: in un database per il PRA, il proprietario di un veicolo puo' essere una persona, una banca (con un leasing) o una ditta
	- La categoria (sottoclasse) PROPRIETARIO contiene un *sottoinsieme* delle istanze provenienti dall'**unione** delle istanze delle entita' DITTA, BANCA, PERSONA (superclassi)
	- Un membro della categoria deve esistere *in almeno una* delle sue superclassi
- Nota: la differenza tra categorie e sottoclassi condivise e' che una sottoclasse condivisa contiene *tutte* le istanze provenienti dall'**intersezione** delle sue superclassi. Un membro della sottoclasse condivisa deve esistere *in tutte* le sue superclassi.

## Definizioni formali del modello EER
- **Classe** $C$: un'insieme di istanze; puo' essere un'entita', una sottoclasse, una superclasse, una categoria
- **Sottoclasse** $S$: una classe le cui istanze debbano sempre essere un sottoinsieme delle istanze di un'altra classe, detta **superclasse** $C$ dell'associazione superclasse/sottoclasse (o E'-UN, IS-A) $C/S$. Si ha quindi: $$ S \subseteq C $$
- **Specializzazione** $Z$: $Z = \{ S_1, S_2, \ldots, S_n \}$ e' un insieme di sottoclassi che hanno la stessa superclasse $G$; quindi $G / S_i$ e' un'associazione di superclasse $\forall i=1, \ldots, n$.
	- $G$ e' chiamata una **generalizzazione** delle sottoclassi $\{ S_1, S_2, \ldots, S_n \}$ 
	- $Z$ e' **totale** se si ha sempre (in ogni istante di tempo) $S_1 \cup S_2 \cup \ldots \cup S_n = G$ 
	- Altrimenti $Z$ e' **parziale**
	- $Z$ e' **disgiunta** se si ha sempre: $S_i \cap S_j = \oslash$ (insieme vuoto) per $i \ne j$  ;
	- Altrimenti, $Z$ e' **sovrapposta**

- Una sottoclasse $S$ di $C$ e' **definita tramite un predicato** (attributo) se si usa un predicato $p$ sugli attributi di $C$ per specificare quali istanze di $C$ sono anche membri di $S$; cioe', $S = C[p]$ e' l'insieme delle istanze di $C$ che soddisfano $p$.
- Una sottoclasse non definita tramite un predicato e' detta **definita dall'utente**.
- Specializzazione **definita tramite l'attributo**: se viene usato un predicato $A = c_i$ (dove $A$ e' un attributo di $G$ e $c_i$ e' un valore costante del dominio di $A$) per specificare l'insieme dei membri di ciascuna sottoclasse $S_i$ in $Z$
- Nota: Se $c_i \ne c_j$ per $i \ne j$, e $A$ e' un attributo a valore singolo, allora la specializzazione sara' disgiunta

- **Categoria** (o tipo UNIONE) $T$:
	- una classe di sottoinsiemi dell'unione di $n$ superclassi che la definiscono $D_1, D_2, \ldots, D_3, \quad n>1$: $$ T \subseteq (D_1 \cup D_2 \cup \ldots \cup D_n) $$
	- Un predicato $p_i$ sugli attributi $D_i$ puo' essere usato per specificare ai membri di ciascuna superclasse $D_i$ che sono anche membri di $T$.
	- Se un predicato e' specificato su ciascun $D_i$, si ha: $$ T(D_i[p_i] \cup D_2[p_2] \cup \ldots \cup D_n[p_n]) $$
	- Nota: la definizione di **associazione** deve essere estesa al rispetto del modello ER consentendo ad ogni **classe** di parteciparvi (non solo alle entita')

## Associazioni di grado 3
- *Associazione ternaria*: si considerino ad esempio le entita' FORNITORE, PARTE e PROGETTO e l'associazione di grado 3 FORNITURA le cui istanze sono un insieme di elementi della forma ($s, j, p)$ con $s$ istanza di FORNITORE, $j$ di PARTE e $p$ di PROGETTO.
- Possiamo considerare 3 associazioni binarie tra queste entita': PUO'$\_$FORNIRE tra FORNITORE e PARTE, USA tra PROGETTO e PARTE, FORNISCE tra FORNITORE e PROGETTO
	- Si puo' verificare che l'esistenzsa delle istanze $(s, p)$, $(j, p)$ e $(s, j)$ in PUO'$\_$FORNIRE, USA e FORNISCE non implica necessariamente l'esistenza di un'istanza $(s, j, p)$ nell'associazione ternaria FORNITURA
	  Servono ulteriori **vincoli**!
- Si puo' anche rappresentare FORNITURA come entita' debole con tre associazioni identificanti e le entita' FORNITORE, PARTE e PROGETTO in qualita' di proprietari. **Equivalenza**!
 