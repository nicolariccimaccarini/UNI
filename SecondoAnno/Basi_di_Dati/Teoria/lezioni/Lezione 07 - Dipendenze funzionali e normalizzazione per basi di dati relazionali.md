## Obiettivo
Valutare la qualita' della progettazione di schemi relazionali, cioe' per valutare in maniera formale perche' un certo insieme di gruppi di attributi in uno schema relazionale e' da considerarsi migliore di un altro.

Esamineremo la loro bonta':
- ==A livello logico (o concettuale)==
	- come gli utenti interpretano gli schemi di relazione e il significato dei loro attributi
- ==A livello di implementazione (o di archiviazione fisica)==
	- come le tuple in una relazione di base sono memorizzate e aggiornate

## Linee guida informali per la progettazione di schemi di relazione
1. ==Assegnazione di una semantica esplicita agli attributi delle relazioni==
	- Il significato che viene dato agli attributi deve essere non ambiguo e semplice da spiegare
	- Non bisogna mischiare attributi provenienti da piu' entita' e associazioni

2. ==Riduzione dei valori ridondanti nelle tuple==
	- Si vuole ridurre al minimo lo spazio di memoria occupato dalle relazioni
	- es. 
		- nella relazione IMP_DIP, se piu' di un impiegato appartiene a un determinato dipartimento allora l'informazione su quel dipartimento sarebbe ripetuta piu' e piu' volte (spreco di memoria)
	- **Anomalie di aggiornamento**
		- IMP_DIP puo' essere un esempio di JOIN naturale e se volessimo memorizzare questa nuova relazione, questo porterebbe ad anomalie di aggiornamento:
			- Anomalie di inserimento $\rightarrow$ non riesco ad inserire l'informazione perche' non e' completa
			- Anomalie di aggiornamento $\rightarrow$ devo aggiornare piu' tuple che contengono lo stesso valore
			- Anomalie di cancellazione $\rightarrow$ si eliminano informazioni importanti

3. ==Riduzione del numero di valori nelle tuple==
	- I valori nulli presentano due problemi:
		1. Spreco di memoria
		2. Sono ambigui
			- Possono rappresentare che un certo attributo non e' pertinente, valore sconosciuto o che il valore e' noto ma assente
			- Quindi bisogna evitarli o assicurarsi che non rappresentino la maggioranza delle tuple

4. ==Impossibilita' di generare tuple spurie==
	- **Tuple spurie** $\rightarrow$ tuple che rappresentano informazioni non valide 
		- si generano a seguito di JOIN, le cui condizioni di uguaglianza su attributi sono fatte su coppie (chiave primaria, chiave esterna)

## Dipendenze funzionali
- ==Definizione: dipendenza funzionale==
	- Presa la relazione $R = \{A_1, \ldots, A_m\}$, $X, Y, \subseteq R$ sottoinsiemi di attributi di $R$, allora: $$ \forall t_1, t_2, \space \text{ SE } \space t_1[X] = t_2[X], \space \text{ ALLORA } \space t_1[Y] = t_2[Y] $$
	- Si dice che i valori della componente $Y$ di una tupla sono determinati dai valori della componente $X$ o che i valori della componente $X$ determinano funzionalmente i valori della componente $Y$.
- ==Osservazioni==
	1. se $X$ e' un attributo chiave, allora $X \rightarrow Y$ e' valida per ogni insieme non vuoti di attributi $Y$ di $R$.
	2. se $X \rightarrow Y$ in $R$, non e' detto che $Y \rightarrow X$
- ==In sintesi==
	1. Una dipendenza funzionale e' una proprieta' del DB
	2. Ci permette di descrivere i vincoli sugli attributi presenti in una relazione, che deve mantenere in ogni momento

- ==Regole di inferenza== (data una relazione $R = \{A_1, \ldots, A_m\}$ e dati $X, Y, \space e, \space Z \subseteq R$ come insiemi non vuoti di attributi)
	1. **Riflessività** $\Rightarrow$ se $Y \subseteq X$, allora $X \rightarrow Y$
	2. **Aumentazione** $\Rightarrow$ se $X \rightarrow Y$, allora $X \cup Z \rightarrow Y \cup Z$ 
	3. **Transitività** $\Rightarrow$ se $X \rightarrow Y$ e $Y \rightarrow Z$, allora $X \rightarrow Z$
	4. **Unione** $\Rightarrow$ se $X \rightarrow Y$ e $Y \rightarrow Z$, allora $Y \cup Z$
	5. **Decomposizione** $\Rightarrow$ se $X \rightarrow Y \cup Z$, allora $X \rightarrow Y$ e $X \rightarrow Z$
	6. **Pseudo-transitività** $\Rightarrow$ se $X \rightarrow Y$ e $W \cup Y \rightarrow Z$, allora $W \cup X \rightarrow Z$ 

- ==Chiusura di F ($F^+$)==
	- Insieme di tutte le dipendenze funzionali che possono essere inferite dall'insieme di $F$ dipendenze funzionali
	- Se $G^+ \subseteq F^+$, allora $F$ copre $G$
	- $F$ e $G$ sono equivalenti se $F^+ \subseteq G^+$ e $G^+ \subseteq F^+$

## Normalizzazione
- ==Definizione: normalizzazione dei dati==
	- Processo di analisi degli schemi di relazione forniti, basato sulle loro dipendenze funzionali e chiavi primarie, per raggiungere le proprieta' di
		- minimizzazione della ridondanza
		- minimizzazione delle anomalie di inserimento, cancellazione e modifica
	- Quindi il processo di normalizzazione sottopone uno schema di relazione a una serie di test per "certificare" se soddisfa una data formale normale
		- processo iterativo:
			- uno schema relazione che non rispetta le condizioni della normale e' scomposto in schemi relazionali minori che contengono un sottoinsieme degli attributi iniziali che lo rispettano in forma normale

- ==Forma normale==
	- piu' alta condizione di normalita' raggiunta da una relazione (grado di normalizzazione)

- ==Proprieta' del processo di normalizzazione==
	1. Proprieta' di JOIN non additiva
		- Garantisce che non vengano generate tuple spurie dopo la decomposizione
	2. Proprieta' di preservazione delle dipendenze
		- le dipendenze funzionali vengono rispettate anche nelle relazioni scomposte

- ==Alcune definizioni==
	1. Superchiave di uno schema relazionale $R = \{A_1, \ldots, A_m\}$
		- Insieme non vuoto di attributi $S \subseteq R$ tale che non possono esistere due tuple $t_1$ e $t_2$ in $r(R)$ per cui $t_1[S] = t_2[S]$
	2. Superchiave minimale
		- Minimo numero di attributi necessario affinché ogni tupla sia distinta 
	3. Attributo primo di $R$
		- Attributo membro di una qualche chiave candidata di $R$

- ==Forma normale (1NF)==
	- Test
		- Domini degli attributi devono avere solo valori atomici
			- aumento della ridondanza
			- si crea una nuova relazione per ogni attributo multivalore
			- si scompone gli attributi composti in attributi semplici
		- Deve esistere una chiave primaria

- ==Seconda forma informale (2NF)==
	- Test
		- Deve essere 1NF
	- Ogni attributo non primo $A$ in $R$ dipende da una dipendenza funzionale piena della chiave primaria di $R$
		- ogni attributo non primo deve dipendere da tutta la chiave della relazione
		- si scompone la relazione e si crea una nuova relazione per ogni chiave parziale e gli attributi dipendenti da essa

- ==Dipendenza funzionale piena==
	- Una dipendenza funzionale $X \rightarrow Y$ e' una dipendenza funzionale piena se la rimozione di un qualunque attributo da $X$ significa che la dipendenza non e' piu' valida $$ X - \{A\} \nrightarrow Y$$
	- osservazione: Se una relazione ha un unico attributo chiave non c'e' bisogno di eseguire il test
		- la dipendenza e' sempre piena perche' l'attributo chiave e' uno solo

- ==Terza forma normale (3NF)==
	- Test
		- Deve essere 2NF
		- Nessun attributo non primo di $R$ dipende transitivamente dalla chiave primaria
			- si scompone la relazione, in modo tale da rompere la transitivita' se abbiamo che $X \rightarrow Y$ e $Y \rightarrow Z$, allora si crea una nuova relazione in cui $Y$ e' chiave e $Z$ sono gli attributi

- ==Proprieta' della dipendenza transitiva==
	- Una dipendenza funzionale $X \rightarrow Y$ di uno schema relazionale $R$ e' una dipendenza transitiva se:
		1. esiste un insieme di attributi $Y$ in $R$ che non e' ne una chiave candidata ne un sottoinsieme di una chiave di $R$
		2. vale: $X \rightarrow Y$ e $Y \rightarrow Z$

- ==Forma normale di Boyce-Codd (BCNF)==
	- Test
		- se per ogni dipendenza funzionale non triviale $X \rightarrow A$ in $R$, vale che $X$ e' una superchiave di $R$
	- Osservazione
		- una dipendenza funzionale $X \rightarrow T$ e' triviale se $Y \subseteq X$
	- Esempio:
		- se $X$ e' {Impiegato, Stipendio} e $Y$ e' {Stipendio}, allora 2 tuple con gli stessi valori sulla coppia di attributi Impiegato e Stipendio hanno ovviamente lo stesso valore sull'attributo stipendio
		- si definisce triviale perche' non raggiunge informazione

- ==IMPORTANTE==
	- OGNI 2NF E' IN 1NF
	- OGNI 3NF E' IN 2NF
	- OGNI BCNF E' IN 3NF
		- essendo la BCNF piu' restrittiva della 3NF, no e' vero che ogni 3NF e' in BCNF