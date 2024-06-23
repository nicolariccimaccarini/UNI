
---
# Modello Relazionale
- Il Modello dei Dati Relazionale e' basato sul concetto di Relazione
- Una **Relazione** e' un concetto matematico basato sulle idee degli insiemi
- La forza dell'approccio relazionale alla gestione dei dati deriva dalle *fondamenta formali* date dalla teoria delle relazioni
- E' quindi importante conoscere a fondo il formalismo relazionale

Il modello e' stato proposto da E. F. Codd (IBM) nel 1970.

# Definizioni Informali
- **Relazione**: Una tabella di valori
	- Una relazione puo' essere pensata come un insieme di **righe**
	- Una relazione puo' essere anche rappresentata come un insieme di **colonne**
	- Ciascuna *riga* rappresenta un fatto che corrisponde ad un *istanza* di un'*entità* o di una *associazione* del mini-mondo
	- Ciascuna *riga* possiede un valore per *elemento* ad un *insieme di elementi* che la compongono, che **identifica univocamente** quella riga nella tabella
	- A volte vengono assegnati degli *identificatori di riga* (row-id) o dei *numeri sequenziali* per identificare le righe nella tabella
	- Ciascuna colonna viene tipicamente individuata mediante il *nome* o l'*intersezione della colonna* o il *nome dell'attributo*

# Definizioni Formali
- Una relazione puo' essere definita in molti modi.
- Lo **Schema di Relazione**: $R(A_1, A_2, ..., A_n)$
	- Lo Schema di Relazione $R$ e' definito sugli *Attributi* $A_1, A_2, ..., A_n$

- Esempio:
	- CLIENTI (ID-Cliente, Nome-Cliente, Indirizzo, Tel)
	- CLIENTI e' una relazione definita sui quattro attributi ID-Cliente, Nome-Cliente, Indirizzo e  Tel, ciascuno dei quali possiede un **dominio**, o insieme dei valori validi.
	- Ad esempio, ID-Cliente e' un numero di 6 cifre.

- Una **tupla** e' un insieme ordinato di valori
- Ciascun valore e' derivato da un **dominio** appropriato
- Ciascuna riga nella tabella CLIENTI puo' essere vista come una tupla della relazione, composta da quattro valori.
- <632895, “Giacomo Piva", "via Saragat 1, 44122, Ferrara", "+39 (0532) 974344">
  è una tupla appartenente alla relazione CLIENTI.
- Una relazione puo' essere trattata come un insieme di tuple (righe).
- Le colonne della tabella sono chiamate **attributi** della relazione.

- Un **dominio** ha una *definizione logica*: ad esempio "Numeri di Telefono" e' l'insieme dei numeri telefonici validi, comprensivi del Country Code, del prefisso e del numero di lunghezza massima di 14 cifre.
- Un dominio puo' avere un *tipo di dati* o un *formato definito*. Il dominio "Numeri di Telefono" puo' avere il formato: `+dd (dddd) dddddddd` dove d e' una cifra decimale. 
  Esempio: le date hanno diversi formati, come giorno, mese, anno oppure `dd-mm-yy`, `dd-mm-yyyy`, ...
- Un attributo specifica il *ruolo* ricoperto da un dominio. Ad esempio il dominio date puo' essere usato per definire degli attributi "Data Fattura" e "Data Pagamento".

- Una relazione (stato ella relazione) e' formata sul prodotto cartesiano degli insiemi dominio; ciascun insieme ha dei valori provenienti da un dominio; il dominio e' usato per specificare il ruolo dell'attributo considerato.
- Ad esempio, l'attributo Nome-Cliente e' definito sul dominio delle stringhe di 25 caratteri. Il ruolo di queste stringhe nella relazione CLIENTI e' quello di specificare il nome dei clienti.
- Formalmente, 
	- Dato $R(A_1, A_2, ..., A_n)$
	- $r(R) \subset dom(A_1) \cdot dom(A_2) \cdot \ldots \cdot dom(A_n)$  
		- $R$: schema di relazione
		- $r(R)$: uno specifico "valore" o popolazione di $R$. Chiamato **stato della relazione**
		- $R$ viene anche chiamato intensione della relazione
		- $r$ viene anche chiamato estensione della relazione

- Siano: **S1 = {0, 1}** e **S2 = {a, b, c}**
- Definiamo **R ⊂ S1 × S2**
- Allora, ad esempio:
	- **r(R) = {<0,a>, <0,b>, <1,c>}** 
	  e' un possibile stato (o popolazione, o intensione) r della relazione R, definita sui domini S1 ed S2. Contiene 3 tuple.

| Termini Informali      | Termini Formali     |
| ---------------------- | ------------------- |
| Tabella                | Relazione           |
| Colonna                | Attributo/Dominio   |
| Riga                   | Tupla               |
| Valori di una colonna  | Dominio             |
| Definizione di Tabella | Schema di Relazione |
| Tabella Popolata       | Estensione (Stato)  |

## Esempio
![[lezione5esempio.png]]

# Caratteristiche delle Relazioni
- Ordinamento delle tuple in una relazione $r(R)$:
	- Le tuple non devono considerarsi ordinate, anche se appaiono in forma tabulare
- Ordinamento degli attributi in uno schema di relazione $R$ (e dei valori all'interno di ciascuna tupla *t*):
	- Gli attributi $R(A_1, A_2, ..., A_n)$ ed i valori in $t = <v_1, v_2, ..., v_n>$ devono essere considerati ordinati.
- In ogni caso, una definizione ancora piu' generale di relazione non richiede alcun tipo di ordinamento.
- Valori in una tupla:
	- Tutti i valori sono considerati atomici (indivisibili). Un valore speciale NULL viene spesso usato per rappresentare valori sconosciuti o non applicabili ad alcune tuple.

- NOTAZIONE
	- Ci riferiremo ai valori presenti in una tupla $t$ con $t[A_i] = v_i$ (il valore dell'attrbuto $A_i$ per la tupla $t$)
	- Analogamente, $t[A_u, A_v, ..., A_w]$ si riferisce alla sotto-tupla di $t$ che contiene i valori degli attributi $A_u, A_v, ..., A_w$ rispettivamente.

![[caratteristiche_delle_relazioni.png]]

# Vincoli di Integrità del Modello Relazionale  
- I vincoli sono condizioni che devono essere rispettate da tuti gli stati di relazione **validi**
- Ci sono tre tipi principali di vincoli:
	1. Vincoli sulla Chiave
	2. Vincoli di Integrita' dell'Entita'
	3. Vincoli di Integrita' Referenziale

## Vincoli sulla Chiave
- **Superchiave** di $R$:
	- Un insieme di attributi *SK* di $R$ tale che non esistano due tuple che abbiano lo stesso valore per *SK* ==in nessuno stato di relazione valido per ==$r(R)$; cioe' per ogni coppia di tuple distinte $t_1$ e $t_2$ in $r(R)$, 
	  $t_1[SK] \ne t_2[SK]$.

- **Chiave** di $R$:
	- Una superchiave minimale; cioe', una superchiave $K$ tale che la rimozione di qualsiasi attributo da $K$ risulti in un insieme di attributi che non e' piu' una superchiave.
		- Esempio: Lo schema di relazione AUTO
		  `AUTO(ProvT, NumT, NumTelaio, Casa, Modello, Anno)`
		  ha due chiavi `PK1 = {ProvT, NumT}, PK2 = {NumTelaio},` che sono anche superchiave `({PK1, PK2})`.
		  `{NumTelaio, Casa}` e' una superchiave ma non chiave

- Se una relazione ha diversi candidati a chiave, ne viene scelta una arbitrariamente come **chiave primaria**.

## Integrita' sulle Entita'
- **Schema di database relazionale**:
	- Un insieme *S* di schemi di relazione che appartengono allo stesso database. *S* e' il nome del database.
	  `S = R1, R2, ..., Rn`

- **Integrita' sulle Entità**:
	- Gli attributi chiave primaria *PK* di ciascuno schema di relazione *R* in *S* non possono avere dei valori *null* in nessuna tupla *r(R)*. Questo perche' i valori delle chiavi primarie sono usati per identificare le varie tuple.
	  `t[PK] ≠ null      ∀ tupla t in r(R)`

- Nota: gli attributi di *R* possono avere il vincolo di non premettere valori *null*, anche se non sono membri di chiavi primarie.

## Integrita' Referenziale
- Un vincolo che coinvolge due relazioni (i vincoli visti precedentemente riguardavano una singola relazione).
- Usato per specificare un riferimento tra tuple in due relazioni: la relazione referenziante e la relazione riferita.
- Tuple nella relazione referenziante $R_1$ hanno attributi *FK* (chiamati attributi chiave esterne) che fanno riferimento agli attributi chiave primarie *PK* della relazione riferita $R_2$. Una tupla $t_1$ in $R_1$ e' detta in riferimento (in relazione, relazionata) con una tupla $t_2$ in $R_2$ se
  $t_1[FK] = t_2[PK]$.
- Un vincolo di integrita' referenziale puo' essere indicato in uno schema relazionale con un arco diretto da $R_1.FK$ a $R_2.FK$.

## Vincolo di Integrita' Referenziale
### Definizione di vincolo
- Il valore della colonna (o delle colonne) chiave esterna *FK* della relazione referenziale $R_1$ puo' essere:
	1. un valore **uguale** al valore di una chiave primaria esistente nella corrispondente chiave primaria *PK* della relazione riferita $R_2$, oppure
	2. un valore **null** 
- Nel caso 2., la *FK* in $R_1$ non deve far parte della chiave primaria di $R_1$.

# Altri tipi di vincoli
- **Vincoli di Integrita' Semantici**:
	- basati sulla semantica dell'applicazione
	- non possono essere espressi dal modello dei dati svincolato dall'applicazione/dati

- Esempio: "il numero massimo di ore, per dipendente, per tutti i progetti su cui lavora, deve essere di 56 ore settimanali"
- Un apposito linguaggio di specifica dei vincoli puo' essere usato per esprimere questo tipo di vincoli
	- Il linguaggio SQL-99 consente l'uso di **triggers** e **asserzioni** a questo scopo
- Molto spesso vengono espressi nel contesto dell'applicazione

# Operazioni di aggiornamento sulle relazioni

- **Inserimento** di una tupla (INSERT)
- **Cancellazione** di una tupla (DELETE)
- **Modifica** di una tupla (MODIFY, UPDATE)

- I vincoli di integrita' non devono essere violati dalle operazioni di aggiornamento delle relazioni
- Una serie di operazioni di aggiornamento possono essere raggruppate insieme
- Le operazioni di aggiornamento possono "propagarsi" e causare altri aggiornamenti in modo automatico. Cio' puo' essere necessario per mantenere i vincoli di integrita'.

- Nel caso un operazione di aggiornamento di una relazione violi un vincolo di integrita', si possono intraprendere alcune azioni:
	- Annullare l'operazione che causa la violazione (opzione REJECT)
	- Eseguire l'operazione, informando l'utente della violazione
	- Far partire ulteriori aggiornamenti in modo da correggere la violazione (opzione CASCADE, opzione SET NULL)
	- Eseguire una routine specificata dall'utente per correggere la violazione

---

# Lezione 5b

## ER - Relazione
1. **Entità**
	- Per ciascuna entita' **E** del diagramma ER, creare una relazione **R** che includa tutti gli attributi semplici di E.
	- Scegliere uno degli attributi chiave di E come **chiave primaria** di **R**. Se la chiave E scelta e' composta, l'insieme degli attributi semplici che la formano saranno chiave primaria di R.
	- *Esempio*: si creano le relazioni DIPENDENTE, DIPARTIEMNTO e PROGETTO nello schema relazionale, in corrispondenza delle entita' del diagramma ER. SSN, DNUMBER e PNUMBER sono le chiavi primarie delle tre relazioni.

2. **Entità deboli**
	- Per ciascuna entita' debole **W** nel diagramma ER, avente come entita' proprietaria l'entita' **E**, creare una relazione **R** e includere tutti gli attributi semplici di **W** come attributi R.
	- Inoltre, includere come **chiave esterna** di **R** tutti gli attributi *chiave primaria* dell'entita' proprietaria E e della chiave parziale dell'entita' debole E.
	- *Esempio*: Creare la relazione A_CARICO, includere la chiave primaria SSN della relazione DIPENDENTE come chiave esterna (rinominandola ESSN). La chiave primaria di A_CARICO e' la combinazione {ESSN, NOME} in quanto NOME e' chiave parziale dell'entita' debole A_CARICO.

3. **Associazioni binarie 1:1**
	- Per ciascuna associazione **A** binaria 1:1 nel diagramma ER, identifica le relazioni **S** e **T** che corrispondono alle entita' che partecipano ad A.
		1. **Chiave esterna**: Scegliere una delle relazioni (**S** ad esempio) e si includa in **S** come **chiave esterna** la **chiave primaria** di **T**. Inserire tutti gli attributi semplici dell'associazione A in S. E' meglio scegliere un'entita' con *partecipazione totale* in A del ruolo S.
		2. **Unica relazione:** Riunire le due entita' e l'associazione in una singola relazione. Cio' e' adeguato quando *entrambe le partecipazioni sono totali*.
		3. **Relazione associazione**: Creare una terza relazione R con lo scopo di definire un riferimento incrociato tra le due chiavi primarie delle due relazioni S e T.
	- *Esempio*: L'associazione 1:1 DIRIGE e' trasformata scegliendo l'entita' DIPARTIEMENTO nel ruolo di S.

4. **Associazioni binarie 1:N**
	- Per ciascuna associazione **A** binaria 1:N, identificare la relazione **S** che rappresenta l'entita' partecipante del lato N.
	- Includere la **chiave esterna** in **S** la **chiave primaria** della relazione **T** che rappresenta l'entita' partecipante del lato 1.
	- Includere tutti gli attributi semplici dell'associazione **A** come attributi della relazione **S**.
	- *Esempio*: L'associazione 1:N LAVORA_PER, CONTROLLA, e SUPERVISIONA in figura. Per LAVORA_PER si include la chiave primaria DNUBER della relazione DIPARTIMENTO come chiave esterna della relazione DIPENDENTE, chiamandola DNO.

5. **Associazione binarie N:M**
	- Per ciascuna associazione **A** binaria N:M creare una nuova relazione **S** che rappresenti **A**.
	- Includere come **chiavi esterne** in **S** le chiavi primarie delle relazioni che rappresentano le entita' partecipanti (entrambe!). La loro combinazione forma (non sempre!) la **chiave primaria** di **S**.
	- Includere anche tutti gli attributi semplici dell'associazione **A** come attributi di **S**.
	- *Esempio*: L'associazione N:M LAVORA_SU diventa la relazione LAVORA_SU. Le chiavi primarie di PROGETTI e DIPENDENTE sono incluse come chiavi esterne e rinominate PNO e ESSN. L'attributo ORE viene aggiunto. La chiave primaria della relazione LAVORA_SU e' la combinazione di {ESSN, PNO}.

6. **Attributi multivalore**
	- Per ciascun attributo multivalore A, creare una nuova relazione R. Questa relazione R avra' un'attributo corrispondente ad A e, in qualita' di chiave esterna, l'attributo K corrispondente alla chiave primaria della relazione che rappresenta l'entita' che aveva A come attributo multivalore.
	- La chiave primaria di R e' la combinazione degli attributi A e K. Se l'attributo multivalore e' composto, occorre includere tutte le componenti semplici nella relazione R.

7. **Associazioni N-arie**
	- Per ciascuna associazione **A** *n-aria*, creare una nuova relazione **S** per rappresentare **A**.
	- Includere come chiavi esterne di **S** le chiavi primarie delle relazioni che rappresentano le n entita' partecipanti.
	- Includere che tutti gli attributi semplici di **A** come attributi di **S**.
	- *Esempio*: L'associazione SUPPY nello schema ER seguente puo' essere trasformata nella relazione SUPPLY mostrata nello schema relazionale, la cui chiave primaria e' la combinazione delle tre chiavi esterne {SNAME, PERTNO, PROJNAME}.
![[AssociazioniNarie.png]]

## Riassunto
**Corrispondenza tra modello ER e modello Relazionale**

| Modello ER             | Modello Relazionale                              |
| ---------------------- | ------------------------------------------------ |
| Entita'                | Relazione "Entita'"                              |
| Associazione 1:1 o 1:N | Chiave esterna (oppure Relazione "Associazione") |
| Associazione N:M       | Relazione "Associazione" e due chiavi esterne    |
| Associazione n-aria  | Relazione "Associazione" e n chiavi esterne    |
| Attributo semplice     | Attributo                                        |
| Attributo composto     | Insieme di attributi (componenti semplici)       |
| Attributo multivalore  | Relazione e chiave esterna                       |
| Insieme di valori      | Dominio                                          |
| Attributo chiave       | Chiave primaria                                  |

## EER - Relazionale
8. Specializzazioni e Generalizzazioni
	- Convertire ciascuna specializzazione di *m* sottoclassi {S1, S2, ..., Sm} e superclasse generalizzata **C** avente attributi {k, A1, ..., An} (`k` attributo chiave di `C`) in uno schema relazionale seguendo una delle quattro possibili opzioni:
		- **Opzione A**: Relazioni multiple - superclasse e sottoclasse
		  Creare una relazione **L** per C con attributi `Attr(L) = {k, A1, ..., An}` e `PK(L) = k`. Creare una relazione **Li** per ciascuna sottoclasse `Si`, `1 < i < m`, con attributi `Attr(Li) = {k} U {attr. di Si}` e `PK(Li) = k`. Questa opzione e' adatta per tutte le specializzazioni.
		  **![31755_FIG0404.png](https://lh7-eu.googleusercontent.com/fx2E7tbyOS5dGqlIT12ShX4u3j8I964zcS7b3QrByssR-54CHaXvmhiowDYi8pYdbLonf8vI6MMJ_PCmQMfWSNyXJrmh3ILeZFH2q550LlUYl2AoNLQkMtTbDjja_5q-vhznFBgX8Da9ZzLqG5tNaZn6n02ivD37=s2048)![0704a.png](https://lh7-eu.googleusercontent.com/VVqXuNkTRreueREej6xMntbPdsyBP1fUFpGFpXQH_F3NFk22e9aMstwUU3hDZxjFgo0o2v-kKdTTOmByb214JjcfKCJ3ieROHdQB70Zdp6Z3H3iDulwowUUhvLs-cWn8RGMK44JfqF9AsLmaFWjrKXokRUdGfpQ2=s2048)** 
		  - **Opzione B**: Relazioni multiple - solo relazioni sottoclasse
		    Creare una relazione `Li` per ogni sottoclasse `Si`, `1 < i < m`, con attributi `Attr(Li) = {attr. di Si} U {k, A1, ..., An}` e `PK(Li) = k`. Questa opzione e' adatta per una specializzazione le cui sottoclassi sono totali.
		    **![0403b.png](https://lh7-eu.googleusercontent.com/sSGQW_xSv6yufzqSLxm07dS9qg065UBGVEJvyDhGcv_zlXEXgsUSG1oBXIdpoU9JPfqUuExfX7GK5WEc85BHt4RPpgro8HkDTQP_Yjqqpw4zJB9Vu212MkEdJmj3ABNOx-omRLDcioymyj448GnaSPMdS6uQ13CI=s2048)![0704b.png](https://lh7-eu.googleusercontent.com/FLPk0wkqSsxbBznZaU67UT0qQyJS5UzhmV2ib1GXUUXs9EzhLBoLlPeKE8DS03FTvGdix_pxHNKQ8UiRiPwNO62lYthoyeQHUZwnmuocBZ7nNRMVvSjg0VjNDJOeSc2tZwgKZ5PvTJuW3lvc5v2jPorxdcbVtr-l=s2048)**
		    
		- **Opzione C**: Relazione singola con un attributo tipo.
		  Creare una singola relazione **L** con gli attributi `Attr(L) = {k, A1, ..., An} U {attr. di S1} U ... U {attr. di Sm} U {t}` e `PK(L) = k`. L’attributo `t` è chiamato tipo (o discriminante) e indica a quale sottoclasse appartiene ciascuna tupla.
		  **![31755_FIG0404.png](https://lh7-eu.googleusercontent.com/TKPsDXStG4px5LV17MWi03ltVpK0-ikdsE4RMj5V-T-ED9B7HqTyQaiFDT3uEjU2xuzWphPGeBU_n9LeGtxoh0QARKUfQ2hhSACzE4xMVHRC1xyNMff1baBG-UwlSO3cos4tUv4m65Ph5TxdZ8rPChBYT9Kk_tvr=s2048)![0704c.png](https://lh7-eu.googleusercontent.com/HwF6BiXOt4ZXPu3aYz-SfZV4UKRu9wUXnFK7SDSh8Z9fsZYKYYyMi84_1imVC6qNoatsUjoch3akWkuJPfhC2KD5xW0egOjFRVFMeS0Fq5CGWTfGbj59aSGB_7glfDb4VDKoOWfP_RTEmycx_u0kwkhw1J6sj4s1=s2048)**

		- **Opzione D**: Relazione singola con molti attributi tipo
		  Creare una singola relazione L con gli attributi `Attr(L) = {k, A1, ..., An} U {attr. di S1} U ... U {attr. di Sm} U {t1, t2, ..., tm}` e `PK(L) = k`. Ciascun `ti, 1 < i < m,` è un attributo di tipo booleano che indica se una tupla appartiene alla sottoclasse `Si`.
		  **![31755_FIG0405.png](https://lh7-eu.googleusercontent.com/By_jfwSd8QS0fyzi8GlmujxG83UAiJ1kIrBHu0jAzD3CC935dDL7jKQaGGBHLrKv0VunB-Go5vOrDfuwifQWts4Ah_-Rxedk9pA_emZscIsYhfX9Ur5sAg9Jc8v8D7Kxw7vfKSQ-JkPtuiRtAmARj1tSfwAvNTkJ=s2048)![0704d.png](https://lh7-eu.googleusercontent.com/Yn5JtFofrFC1ISyMtDfYuh7Rxl6ySYKufJoqsJnT_qhfzDw-cn6EXiazTffgMfbtdOsIPfCqGVSPj4vneJ0Jog_u30RC1JXdejaMi2EJYYQ1KSuzEUNSxLHtXGDofPZSeCZXD7GWMQrOjILVEB12yd0t3JI2Rf95=s2048)**
