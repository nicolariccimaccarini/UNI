## Tassonomia
- Statica o dinamica:
	- **statica** $\rightarrow$ se la dimensione e' fissata a priori
	- **dinamica** $\rightarrow$ se la dimensione puo' variare durante l'esecuzione
- compatta o sparsa:
	- **sparsa** $\rightarrow$ gli elementi sono virtualmente vicini ma fisicamente non sappiamo dove si trovano
	- **compatta** $\rightarrow$ gli elementi si trovano in posizioni fisiche di memoria contigue
- basata o non basata su ordinamento:
	- **basata su ordinamento** $\rightarrow$ se gli elementi sono ordinati secondo una chiave
	- **non basata su ordinamento** $\rightarrow$ se non c'e' alcuna relazione con una chiave

--- 
## Liste
- Struttura dati dinamica, sparsa e non basata sull'ordinamento
- E' una struttura dati concreta (o fondamentale)
- Ogni elemento ha un campo dati e un campo puntatore al successivo
- Associamo le operazioni di 
	- inserimento $\rightarrow$ $\Theta(1)$
	- cancellazione $\rightarrow$ $\Theta(1)$
	- ricerca $\rightarrow$ $\Theta(n)$ (nel caso peggiore)

---
## Pile
- Struttura dati astratta
- Struttura dati dinamica, non basata sull'ordinamento, sparsa (basata su liste) o compatta (basata su array)
- Implementa la politica LIFO

### Pile su Array
- Si puo' pensare come un oggetto con i suoi relativi metodi
- Sia $S$ array di interi $\rightarrow$ dotato di parametri `S.top` e `S.max`
- Le operazioni di inserimento di un elemento nelle pile prendono il nome di *Push* e *Pop*.

### Pile su liste
- L'operazione di inserimento in testa di una lista e' la *Push* e la *Pop* e' una semplificazione dell'operazione di delete.

--- 
## Code
- Struttura dati astratta
- Struttura dati dinamica, non basata sull'ordinamento, sparsa (basata su liste) o compatta (basata su array)
- Implementa la politica FIFO
- Permette di accedere ai dati attraverso inserimento ed eliminazione che prendono normalmente i nomi di `Enqueue` e `Dequeue`.

### Code su Array
- Usate per implementare uno stack
- Sia $Q$ una coda $\rightarrow$ dotata di parametri `Q.head` e `Q.tail` inizializzati entrambi a $1$

### Code su liste
- Campo `Q.tail` in aggiunta a `Q.head` per implementare `Enqueue` chiamando l'inserimento in una lista e `Dequeue` chiamando l'eliminazione di un elemento in una lista

---
## Heap
- Struttura dati astratta, parzialmente basata sull'ordimanento e non necessariamente compatta 
- Caratteristica principale $\rightarrow$ mantiene le chiavi semi-ordinate

### Heap binarie su array
- Distinguiamo due tipi di heap: 
	- max-heap $\rightarrow$ il massimo elemento si trova nella radice
	- min-heap $\rightarrow$ il minimo elemento si trova nella radice
- In generale una heap di altezza $h$ contiene **al minimo** $2^h$ elementi ed **al massimo** $2^{h+1} -1$ elementi
- $h = \Theta(log(n))$ 

---
## Code di priorità
- Struttura dati astratta, basata sull'ordinamento e necessariamente compatta.
- Si possono costruire basandosi sulle min-heap
- Associa ad ogni chiave la priorita' che serve $\rightarrow$ estrae l'elemento di priorita' piu' bassa

### Code di priorita' su heap binarie
- Correttezza $\rightarrow$ immediata
- Complessità $\rightarrow$ $\Theta(log(n))$ nel caso pessimo

### Code di priorita' su array
- Implementate direttamente su array senza passare dalle heap
- Complessita' non comparabile $\rightarrow$ migliore in qualche caso e peggiore in qualche caso
- Conviene esplicitare le chiavi associate
- Sia $Q$ un array che contiene $\rightarrow$ $i$, `Q[i]` e `Q[i].empty`
	- costo inizializzazione $\rightarrow$ $\Theta(n)$
- Correttezza e terminazione $\rightarrow$ immediate
- Estrazione del minimo $\rightarrow$ $O(n)$
- Decremento $\rightarrow$ $\Theta(1)$

---
## Tabelle Hash
- Struttura dati astratta, dinamica, parzialmente compatta e non basata su ordinamento
- Uso: dato un numero di oggetti **relativamente piccolo**, ognuno dei quali è denotato da una chiave con universo **relativamente grande**, si vuole memorizzare in maniera dinamica gli oggetti e implementare operazioni di inserimento, cancellazione, ricerca
- La tabella hash ad accesso diretto (uso di array) ha ottime complessità ($\Theta(1)$ per tutte le operazioni) ma solo se l'universo è piccolo $\rightarrow$ si cercano soluzioni migliori
- $T$ $\rightarrow$ array di $m$ posizioni
	- problema: memorizzare una chiave $k > m$ 
		- soluzione: funzione di hash che indirizza l'elemento $k$ alla posizione $h(k)$
	- problema: conflitti $\rightarrow$ quando $k_1 \ne k_2$ ma $h(k_1) = h(k_2)$
		- soluzioni: chaining (in ogni cella della tabella c'è un puntatore alla testa di una lista) e open hashing (provare più posizioni finché non se ne trova una libera o se la tabella è piena) 

### Tabelle hash con chaining
- Tecnica che risolve i conflitti di una tabella hash usando un puntatore alla testa di una lista in ogni cella (testa memorizzata in `T[h(k)]`, quando è vuota punta a `nil`)
- Operazioni:
	- `HashInsert` $\rightarrow$ complessita' $\Theta(1)$
	- `HashSearch` $\rightarrow$ complessita' $\Theta(n)$
	- `HashDelete` $\rightarrow$ complessita' $\Theta(n)$ 

### Funzioni di hash con il chaining
Perché una funzione di hash sia buona, deve distribuire le chiavi in maniera uniforme sulle m posizioni della tabella. Per farlo esistono diversi metodi:
1. **Divisione**: $h(k) = (k \cdot mod(m)) + 1)$ $\rightarrow$ resto della divisione tra $k$ e $m+1$
	- chiavi naturali
	- $m$ numero primo
	- $m$ lontano da una potenza di 2
2. **Moltiplicazione**: $h(k) = \lfloor m \cdot (k \cdot A - \lfloor k \cdot A) \rfloor ) \rfloor +1$ 
	- chiavi naturali
	- $m = 2^p$ per qualche $p$ $\rightarrow$ $h(k)$ dipende dagli ultimi $p$ bit di $k$ 
	- si sceglie $A$ costante tra 0 e 1
3. **Addizione**: $((a_1 \cdot B + a_2) \cdot B + a_3) \cdot B + ... a_d$  
	- chiavi sono stringhe o insiemi di stringhe o oggetti complessi
	- $m$ numero primo
	- $m$ lontano da potenze di 2
	- problema della dimensione $\rightarrow$ risolvo calcolando il modulo (non si memorizza mai il $k$)
	- $d$ = numero di caratteri della stringa
	- $B$ = cardinalità dell'alfabeto (es. 26 per alfabeto inglese)
	- $a_1a_2a_3...a_d$ = stringa

### Tabelle Hash con Open Hashing
- Tecnica che risolve i conflitti di una tabella hash provocando piu' posizioni finche' non se ne trova una libera o se la tabella e' piena.
- Si ipotizza che il numero di chiavi e' minore al numero di celle della tabella
	- $n \le m$
- **Caratteristiche**:
	- elimina le liste $\rightarrow$ una tabella hash di $m$ elementi potra' tenere al massimo $m$ elementi
	- eliminando le liste non si potrà avere la funzione di cancellazione
- Partendo da una funzione di hash qualsiasi si verifica se la cella è libera:
	- se è libera si inserisce la chiave
	- se non è libera si cerca una nuova cella
- Sequenza di probing $\rightarrow$ sequenza di tentativi di inserimento di una chiave in una tabella hash

---
## Insiemi disgiunti
- Struttura dati astratta, parzialmente dinamica, parzialmente sparsa e non basata sull'ordinamento
- La caratteristica principale di un insieme disgiunto e' che le operazioni ad esso associate sono tipicamente:
	- `MakeSet` $\rightarrow$ costruisce un nuovo insieme disgiunto
	- `Union` $\rightarrow$ unisce due insiemi disgiunti in uno solo
	- `FindSet` $\rightarrow$ trova il rappresentante dell'insieme al quale appartiene l'elemento.
- Ogni insieme e' dotato di un elemento rappresentativo.
- Gli insiemi crescono solo in due modi:
	- quando vengono creati (e contengono esattamente un elemento)
	- quando vengono uniti due insiemi in uno solo che contiene gli elementi di entrambi

### Insiemi disgiunti: liste
- Uso delle liste collegate per gestire $S$(`calS`)
- $S \in \mathcal{S}$ e' una lista con:
	- `S.head` $\rightarrow$ punta al primo elemento
	- `S.tail` $\rightarrow$ punta all'ultimo elemento
- Ogni elemento $x$ e' dotato di:
	- `x.next` $\rightarrow$ punta all'elemento successivo
	- `x.head` $\rightarrow$ punta all'insieme $S$ che lo contiene 
	- `x.key` $\rightarrow$ chiave dell'elemento
- L'informazione aggiuntiva che contiene ogni $S[i]$ e' un puntatore all'elemento $i$ in memoria, cioe' alla casella $x$ che contiene la chiave $i$. Lo chiamiamo per esempio `calS[i].set`.
- `MakeSet(x)` $\rightarrow$ crea nuovo oggetto $S$ (`S.head = S.tail = x`) quindi costa $O(1)$
- Rappresentante di ogni $S$ $\rightarrow$ `S.head`
- `FindSet(x)` $\rightarrow$ dato $x$, si cerca prima `x.head` poi `x.head.head` (restituisce il rappresentante dell'insieme di $x$) $\rightarrow$ $O(1)$
- `Union(x, y)` $\rightarrow$ unisce due insiemi in uno già esistente ed eliminando l'altro $\rightarrow$  $\Theta(n)$ 
- Operazioni corrette e terminanti
- **Complessità** 
	- Analisi ammortizzata $\rightarrow$ calcolare il costo **medio** di una operazione qualsiasi nel contesto di un gruppo di operazioni, piuttosto che il costo per operazioni
		- Nell'analisi ammortizzata non ci sono considerazioni probabilistiche
	- il costo medio ammortizzato di un'operazione e' $\frac{\Theta n^2}{\Theta(n)} = \Theta(n)$ 

### Insiemi disgiunti: liste con unione pesata
- **Euristica** $\rightarrow$ strategia migliorativa e non un'implementazione diversa, ha un effetto soprattutto dal punto di vista pratico  
- **Algoritmo Euristico** $\rightarrow$ algoritmo che da una soluzione non ottimale si trova un problema la cui soluzione ottimale esiste, lo si fa per scelta per evitare il costo computazionale che avrebbe avuto quella esecuzione
- Strategia per migliorare la situazione $\rightarrow$ **unione pesata**
	- scriviamo in ogni insieme $S$ il numero di elementi (`S.rank`)e con le `Union()` si fanno gli aggiornamenti dei puntatori sempre sull'insieme piu' piccolo
- **Complessità**
	- si ha il caso peggiore quando tutti gli $s$ sono di dimensione uguale
	- costo totale delle $\Theta(n)$ unioni $\rightarrow$ $\Theta(n \cdot log(n))$ 

### Insiemi disgiunti: foreste di alberi
- Metodo piu' efficiente per implementare gli insiemi disgiunti
- In questa rappresentazione gli elementi vivono come prima, nelle strutture S, e sono puntati in un albero, mentre i rank sono i limiti superiori dell'altezza dell'albero.
- **Rappresentazione**:
	- nodo `x` (elemento che non ha puntatore ai figli) contiene:
    - `x.p` $\rightarrow$ padre
    - `x.rank` $\rightarrow$ rango
    - `x.key` $\rightarrow$ chiave
- Attenzione: questi alberi sono liste particolari e non vanno confusi con alberi e grafi
- **Operazioni**:
	- `Makeset` $\rightarrow$ crea un nuovo albero di altezza massima 0 con solo il nodo $x$
	- `Union` $\rightarrow$ unisce due alberi in uno solo:
		1. si trovano i rappresentanti degli elementi (x, y) usati come indici
		2. si sceglie l'elemento di rango inferiore e si aggiorna solo il padre (si fa puntare il rappresentante con rango inferiore al rappresentante con rango superiore)
	- `FindSet` $\rightarrow$ scorrendo i puntatori verso l'alto li aggiorna appiattendo il ramo al quale appartiene il rappresentante e lo restituisce
- **Complessità**:
	- La complessita' di $m$ operazioni si vede con $O(m \cdot \alpha(n))$ con $\alpha(n)$ funzione inversa di Ackermann che cresce lentamente (tanto da essere costante)
	- costo totale $\rightarrow$ $\Theta(m)$ 

--- 
## Alberi
- Strutture dati fondamentali dinamiche e spare, possono essere basate sull'ordinamento oppure no
- Generalizzano le liste ma se ne fa un uso diverso
- **Albero** (o **albero radicato**) $\rightarrow$ grafo aciclico connesso tale che ogni coppia di vertici e' connessa da al piu' un cammino
- **Nodi** $\rightarrow$ vertici dell'albero.  
- **Foglie** $\rightarrow$ nodi senza figli.  
- **Albero k-ario** $\rightarrow$ ogni nodo ha al massimo k figli.  
- **Altezza** $\rightarrow$ massimo numero di archi su un percorso semplice dalla radice ad una foglia (l'altezza è legata al numero di nodi in maniera non triviale).
	- BST $\rightarrow$ $\Theta(n \cdot \log(n))$ 
	- nessuna struttura:
		- caso medio $\rightarrow$ $\Theta(\log(n))$
		- caso peggiore $\rightarrow$ $\Theta(n)$
- **Albero diretto** $\rightarrow$ ogni sottoalbero ha un nodo predeterminato chiamato radice, che dota l'albero di un ordinamento topologico privilegiato
- **Albero indiretto** $\rightarrow$ la radice viene invece individuata in maniera arbitraria, cosi' come la direzionalita' dei cammini

- I nodi di un albero binario possono esser pensati come oggetti che possiedono, almeno, una **chiave** (`x.key`) e tre puntatori:
	1. il **padre** (`x.p`)
	2. il **figlio destro** (`x.right`)
	3. il **figlio sinistro** (`x.left`).
- Tutti i puntatori sono **nil** quando non sono definiti.
- Tipi di visite:
	- visite **in-order**;
	- visite **pre-order**;
	- visite **post-order**.

### Alberi binari di ricerca (BST)
- Sono una struttura dati dinamica, basata sull'ordinamento e implementata in maniera sparsa
- Essendo basata sull'ordinamento possiamo fare le operazioni di inserimento, cancellazione, ricerca, minimo, massimo, successore e predecessore

- Le regole che un albero binario di ricerca deve rispettare (anche note come **proprieta' BST**), sono:
	1. Per ogni nodo $x$, se un nodo $y$ si trova nel **sotto-albero sinistro**, allora `y.key <= x.key`;
	2. Per ogni nodo $x$, se un nodo $y$ s trova nel **sotto-albero destro**, allora `y.key > x.key`.
- Dunque si puo' dire che un BST e' **parzialmente ordinato**.

- I BST si creano vuoti come nel caso generale ed hanno esattamente la stessa struttura
- Sui BST ha senso effettuare la visita in order per restituire l'insieme di chiavi ordinate, ma non ha senso usare gli altri due tipi di visite.

- **Operazioni**:
	- Ricerca:
		- complessita' direttamente proporzionale all'albero
			- casso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$ 
	- Minimo e massimo 
		- il nodo minimo e' il nodo piu' a sinistra dell'albero mentre il massimo e' quello piu' a destra
		- complessita' direttamente proporzionale all'albero
			- casso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$ 
	- Successore e predecessore:
		- se $x$ ha figlio destro $\rightarrow$ il successore e' il minimo del sottoalbero destro di $x$
		- se $x$ non ha figlio destro $\rightarrow$ il successore è tra i suoi antenati ovvero bisogna risalire finché la relazione padre-figlio diventa padre-figlio sinistro
		- complessita' direttamente proporzionale all'albero
			- casso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$ 
	- Inserimento:
		- inserisce un nuovo elemento come foglia nell'albero, questo permette di rispettare la proprietà BST ed essere efficiente.
		- Correttezza: si vuole dimostrare che il risultato di un inserimento sia un BST (T = BST, T' = risultato di un inserimento)
			- se $T$ è vuoto $\rightarrow$ while non eseguito $\rightarrow$ z è la radice di $T'$ $\rightarrow$ $T'$ è un BST
			- se $T$ non è vuoto $\rightarrow$ invariante del ciclo = posizione corretta di $z$ è nel sotto-albero di x e y ne mantiene il padre:
			    - caso base: $x$ = `T.root`
			    - caso induttivo: dopo la `i-1` esecuzione, $z$ è confrontato con $x$ e $y$, mantenendo la proprietà
			    - alla fine del ciclo `x = nil` ed è nella posizione corretta di $z$
		- Complessita': direttamente proporzionale all'altezza dell'albero
			- caso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$
	- Eliminazione: 
		- si distinguono 3 casi:
			1. se $z$ è foglia (non ha figli) -> si elimina $z$
			2. se $z$ ha un solo figlio -> si elimina $z$facendo puntare il padre di $z$ al figlio di $z$ (come eliminare un nodo in una lista)
			3. se $z$ ha due figli -> si cerca il successore di $z$ (es. $y$) e si scambia $z$ con $y$, poi si elimina $y$
		- Complessita':
			- caso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$

### Alberi Red-Black (RBT)
- E' un BST bilanciato per costruzione
- Ha tutte le caratteristiche di un BST ma la sua altezza e' sempre $\Theta(\log(n))$ ($n =$ numero di nodi)
- E' una struttura dati dinamica, sparsa e basata sull'ordinamento
- Ogni nodo in un RBT ha delle informazioni in piu' rispetto a un nodo di BST:
	- colore (`x.color`) $\rightarrow$ rosso o nero (per convenzione)
	- ogni foglia possiede due figli virtuali che non contengono chiave e sono sempre di colore nero
	- definita una sentinella `T.nil` $\rightarrow$ campo aggiuntivo di $T$ (colore fissato a nero per il ruolo di foglia esterna)
- I nodi si differenziano tra 
	- interni $\rightarrow$ ha sempre due figli e una chiave
	- esterni $\rightarrow$ nodo nero senza chiave con proprieta' uguali agli altri nodi
- Le regole che ogni RBT deve rispettare sono:
	1. Ogni nodo e' rosso o nero;
	2. La radice e' nera;
	3. Ogni foglia (esterna, **nil**) e' nera;
	4. Se un nodo e' rosso, entrambi i suoi figli sono neri;
	5. Per ogni nodo, tutti i percorsi semplici da lui alle sue foglie, contengono lo steso numero di nodi neri.

- Le proprieta' garantiscono il bilanciamento dell'albero
- Il bilanciamento e' una proprieta' dinamica $\rightarrow$ relazione tra numero di nodi e altezza
- Altezza nera (`bh(x)`) di un nodo $x$ in $T$ $\rightarrow$ numero di nodi neri su un qualsiasi percorso semplice da $x$ ad una foglia esterna 
- Altezza nera dell'albero $T$ $\rightarrow$ altezza nera della root `bh(T.root)`
- numero $n$ di nodi in $T$ $\rightarrow$ $2^{\text{bh(T.root)}}-1$, cioe' $n \ge 2^{\frac{h}{2}}-1$
- $h \le 2 \cdot \log(n+1)$ $\rightarrow$ l'altezza nel caso peggiore è di 2 volte il logaritmo del numero di nodi interni più 1 quindi è $\Theta(\log(n))$
- $h = \Theta(\log(n)) \rightarrow$ bilanciato

- Le operazioni di inserimento e cancellazione potrebbero violare le proprietà di un RBT quindi implementando queste operazioni si modifica la struttura dell'albero per ripristinare le proprietà.
- Per ripristinare la struttura si usa la **rotazione** $\rightarrow$ operazione per mantenere le proprietà di un BST (non di un RBT).
- Complessità $\rightarrow$ $O(1)$ 

- Usando l'algoritmo di inserimento dei BST, le proprieta' dei BST sono rispettate
- Se il nodo inserito e' rosso si avra':
	- proprieta' 1 $\rightarrow$ rispettata
	- proprieta' 2 $\rightarrow$ violata ($z$ diventa la root che dovrebbe essere nera)
	- proprieta' 3 $\rightarrow$ rispettata
	- proprieta' 4 $\rightarrow$ violata ($z$ diventa figlio di un nodo rosso che dovrebbe essere nero)
	- proprieta' 5 $\rightarrow$ rispettata
- Per mantenere le proprieta' si usa `RBTreeInsertFixup`
- **Correttezza** $\rightarrow$ se esiste il problema dopo l'inserimento (proprieta' 2 e 4 violata) si spinge verso l'alto con il caso 1 finche' possibile. Quando non e' piu' possibile si passa al caso 2 o 3. Una rotazione risolve il problema e esce dal ciclo
- **Invariante**:
	1. $z$ e' rosso
	2. se $z.p$ e' la root $\rightarrow$ $z.p$ e' nero
	3. se $T$ viola qualche proprieta' $\rightarrow$ viola una sola proprieta'
		- se viola la proprieta' 4 $\rightarrow$ $z$ e $z.p$ sono rossi
		- se viola la proprieta' 2 $\rightarrow$ $z$ e' la root rossa
- **Complessità** $\rightarrow$ nel caso peggiore costa tanto quanto risalire l'albero fino alla root quindi $\Theta(h) = \Theta(\log(n))$ 

### Alberi B (BT = Balanced Tree)
- Struttura dati dinamica, sparsa e basata sull'ordinamento
- Generalizza un RBT $\rightarrow$ mantiene le proprieta' di bilanciamento ma ha fini diversi
- E' sempre completo $\rightarrow$ il nodo stesso deve contenere piu' chiavi
- Ottimizzati per minimizzare gli accessi al disco (memoria secondaria)
- Un BT si caratterizza per:
	1. possedere un'arietà (branching factor) superiore ad un parametro, il grado minimo = $t$, che in questo caso è 2 (spesso dell'ordine delle migliaia)
	2. un'altezza proporzionale al logaritmo in base $n$ (con $n$ numero di chiavi)
	3. avere nodi con molte chiavi ordinate tra loro
	4. crescere verso l'alto, non verso il basso (un nodo inizia da radice poi diventa un nodo interno e il nuovo nodo è radice, ...)
- Complessita' proporzionale all'altezza
- Un nodo di un BT e' composto da:
	- `x.p` $\rightarrow$ padre
	- `x.n` $\rightarrow$ numero di chiavi nel nodo
	- `x.leaf` $\rightarrow$ booleano, indica se il nodo e' foglia
	- `x.c_1, ..., x.c_x.n+1` $\rightarrow$ puntatori ai figli definiti se 
	- `x.key_1, ..., x.key_x.n` $\rightarrow$ $n$ chiavi ordinate
- Le proprieta' di un albero B sono:
	1. Ogni nodo, tranne la radice, ha almeno $t-1$ chiavi;
	2. Ogni nodo puo' contenere al massimo $2 \cdot t-1$ chiavi;
	3. Per ogni nodo $x$, `x.key_1 <= x.key_2 <= ... <= x.key_x.n`;
	4. Per ogni nodo $x$, se un nodo $y$ contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono minori o uguali a `x.key_i`
	5. Per ogni nodo $x$ , se un nodo $y$ e; contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono maggiori di `x.key_i-1`
- Altezza massima $\rightarrow$ $h \le log_t(\frac{n+1}{2}) = O(log_t(n))$ 

- **Operazioni**
	- Convenzioni:
		- usiamo esplicitamente le operazioni `DiskRead` e `DiskWrite` per tenere conto degli accessori al disco; 
		- diciamo che la radice `T.root` e' sempre in memoria principale
		- rispetto a un nodo passato come parametro, assumiamo che sia gia' stata eseguita `DiskRead` su di esso.
		- calcoliamo la complessita' sia in termini di uso della CPU sia in termini di numero di accessi al disco
	- Ricerca (`BTreeSearch`):
		- si sceglie tra $x.n+1$ possibili figli
		- prende in input un nodo $x$ di $T$ e una chiave $k$
		- restituisce
			- un puntatore a un nodo $y$
			- un indice $i$ se ricerca ha successo altrimenti nil
		- Correttezza immediata
		- Complessita':
			- numero di accessi al disco: $O(h) = O(\log_t(n))$
			- uso di CPU: $\Theta(t \cdot h) = \Theta(t \cdot log_t(n))$ 
	- Inserimento (`BTreeCreate`):
		- Prima di fare l'inserimento l'albero deve esistere
		- `Allocate()` $\rightarrow$ funzione che crea e occupa uno spazio sufficiente per un nodo di memoria
		- Split del figlio per l'inserimento:
			- Funzionamento:
				1. riempire un nodo fino a quando diventa pieno ($2t-1$ chiavi)
				2. dividere il nodo in due nodi di $t-1$ chiavi l'uno
				3. inserire la nuova chiave del nodo padre
				4. se il nodo padre e' pieno, ripetere l'operazione un livello piu' in alto
			- Complessita':
				- numero di accessi al disco $\rightarrow$ $\Theta(1)$
				- uso di CPU $\rightarrow$ $\Theta(t)$
		- Inserimento di una chiave:
			- Usa `BTreeInsertNonFull` $\rightarrow$ inserisce ricorsivamente una chiave in un nodo non pieno
			- La chiave non va sempre inserita in una foglia:
				- se il nodo considerato e' foglia $\rightarrow$ si inserisce la chiave
				- se il nodo considerato non e' foglia $\rightarrow$ si inserisce ricorsivamente in un nodo e se e' pieno si procede ricorsivamente
			- Il pre-emptive split permette di effettuare split su ogni nodo pieno che si trova sul percorso di discesa fino alla foglia in cui si deve andare.
			- In questo modo si minimizza il numero di accessi al disco e l'uso di CPU.
			- Correttezza:
				- **caso base:** all'inizio di ogni esecuzione di `BTreeInsertNonFull`, il nodo $x$ è non pieno e la chiave $k$ viene inserita nel sotto-albero di $x$, in uno dei suoi figli.
				- **caso induttivo:** si trova il posto giusto per $k$ e, poiché $x$ non è foglia, carica il giusto figlio. Essendo il nodo non pieno per ipotesi (se nodo pieno $\rightarrow$ split) si richiama `BTreeInsertNonFul`
			- Complessita':
				- numero di accessi al disco $\rightarrow$ $\Theta(h) = \Theta(\log_t(n))$
				- uso di CPU $\rightarrow$ $\Theta(h \cdot t) = \Theta(t \cdot log_t(n))$

---
## Grafi
- Strutture dati statiche, sparse o compatte e non basate sull'ordinamento
- Il fatto di essere statiche comporta che non ci siano operazioni di inserimento o cancellazione di elementi
- Un grafo e' una tripla $G = (V, E, W)$ dove:
	- $V$ $\rightarrow$ insieme di vertici
	- $E \subseteq V * V$ $\rightarrow$ insieme di archi 
	- $W: E \rightarrow \mathbb{R}$ $\rightarrow$ funzione che assegna un peso ad ogni arco
- $u \rightsquigarrow v$ $\rightarrow$ quando da un vertice $u$ possiamo raggiungere un vertice $v$
- Gradi di un grafo:
	- **grado entrante** $\rightarrow$ numero di archi entranti in un vertice
	- **grado uscente** $\rightarrow$ numero di archi uscenti da un vertice
- Proprieta' statiche di un grafo:
	- **Diretto** $\rightarrow$ gli archi sono orientati
	- **Indiretto** $\rightarrow$ gli archi non sono orientati
	- **Pesato** $\rightarrow$ gli archi hanno un peso ($W$ non costante)
	- **Non pesato** $\rightarrow$ gli archi non hanno un peso ($W$ costante $\rightarrow$ $G = (E, V)$)
- Proprieta' dinamiche di un grafo:
	- **Sparso**: $|E| << |V|^2$ $\rightarrow$ numero di archi molto minore rispetto al numero di vertici
	- **Denso**: $|E| \approx |V|^2$ $\rightarrow$ numero di archi simile al numero di vertici
- Rappresentazione di un grafo:
	- **liste di adiacenza** $\rightarrow$ si usa un array $Adj[1, \ldots, |V|]$ dove ogni elemento punta ad una lista (preferibile nei grafi sparsi)
	- **matrice di adiacenza** $\rightarrow$ si usa una matrice $A[1, \ldots, |V| \space * \space 1, \ldots, |V|]$ dove $A[u, v] = w$ e $w$ e' il peso dell'arco (preferibile per i grafi densi)
- `u.att` $\rightarrow$ indica un attributo `att` associato con un vertice $v$.

### Grafi: visita in ampiezza (BFS)
- È dato un grafo $G = (V, E)$ diretto o indiretto, non pesato, ed un vertice sorgente $s \in V$   
- **Obiettivo**: conoscere quanti archi sono necessari per raggiungere qualunque altro vertice (raggiungibile) da $s$  
- **Soluzione**: si usa una visita di $G$ in ampiezza (BFS - *Breadth First Search*)

**BFS**:
- Esplorazione degli archi di $G$ e scoperta di nuovi vertici raggiungibili da quelli sconosciuti
- Si usano i colori:
	- **bianco** $\rightarrow$ vertice non ancora scoperto, tutti i vertici sono bianchi all'inizio tranne $s$ che e' grigio
	- **grigio** $\rightarrow$ vertice scoperto
	- **nero** $\rightarrow$ quando tutti i vertici adiacenti sono stati scoperti
- Attributi:
	- `u.color` $\rightarrow$ colore del vertice
	- `u.pi` $\rightarrow$ predecessore di $u$ nel cammino minimo da $s$ a $u$
	- `u.d` $\rightarrow$ distanza minima in numero di archi da $s$ a $u$
- L'algoritmo computa la distanza minima in numero di archi tra $s$ ed ogni vertice scoperto e produce un albero di visita in ampiezza con tutti i vertici raggiungibili da $s$
- $\delta(s,v) \rightarrow$ **distanza piu' corta** da $v$ a $s$ (numero minimo di archi che sono necessari per raggiungere $v$ da $s$)
- Proprieta':
	- he sia zero tra un vertice e se stesso $\rightarrow$ $\delta(s,s) = 0$;
	- che sia infinito da $s$ a $v$ quando il secondo e' irraggiungibile dal primo $\rightarrow \delta(s,v) = \infty$;
	- che sia una distanza $\rightarrow$ disuguaglianza triangolare (per ogni coppia di vertici $v, u$ tali che esiste un arco $(u,v)$, succede che $\delta(s,v) \le \delta(s,u)+1$).
- **Correttezza**: dopo l'esecuzione, per ogni $v$ raggiungibile da $s$, $\text{v.d} = \delta(s,v)$ e se $v \ne s$, almeno uno dei percorsi piu' brevi da $s$ a $v$ si ottiene da uno dei percorsi piu' brevi da $s$ a $v.\pi$ con l'arco $(v.\pi, v)$.
	- **caso base**: $v=s \rightarrow$ all'entrata in $Q$ $s.d = 0 = \delta(s,s)$
	- **caso induttivo**: $v$ scoperto da $u$ quindi gia' in $Q \rightarrow u.d \ge \delta(s,u)$ quindi $v.d = u.d + 1$ e $v.d \ge \delta(s,u)$
- **Complessità**: per algoritmi sui grafi si usa l'analisi aggregata (si guarda il numero di elementi/archi)
	- **grafo connesso** $\rightarrow$ $\Theta(|V| + |E|)$
	- **grafo non connesso** $\rightarrow$ $O(|V| + |E|)$
	- **grafo denso** $\rightarrow$ $\Theta(|V|^2)$ ma si accetta anche $\Theta(|V| + |E|)$ 

### Grafi: visita in profondita' (DFS)
- Consideriamo un grafo $G = (V,E)$, se questo e' diretto di puo' visitare in maniera diversa per avere informazioni piu' utili
- Con la DFS si possono risolvere 3 problemi:
	1. stabilire se $G$ e' **ciclico** $\rightarrow$ stabilire se contiene almeno un ciclo
	2. costruire un **ordinamento topologico** di $G$ $\rightarrow$ elencare tutti i suoi vertici in un ordine qualsiasi tale che ogni vertice $v$ e' elencato solo se tutti i vertici **dai quali** $v$ si puo' raggiungere sono stati elencati prima
	3. conoscere ed enumerare tutte le **componenti fortemente connesse** di $G$ $\rightarrow$ elencare tutti i sottoinsiemi massimali di $V$ tale che, ogni vertice in ogni sottoinsieme raggiunge ogni altro vertice di quel sottoinsieme

**DFS**:
- Obiettivo $\rightarrow$ scoprire tutti i vertici raggiungibili da ogni potenziale sorgente $s$
- Output $\rightarrow$ foresta di alberi (uno per ogni sorgente)
- **Differenza con BFS** $\rightarrow$ i vertici vengono scoperti il prima possibile a partire da quelli gia' scoperti (e' ricorsiva)
- Assunzioni:
	- $G$ e' rappresentato con liste di adiacenza
	- `u.color` $\rightarrow$ colori per distinguere i vertici ancora da scoprire e quelli gia' scoperti
	- `v.pi` $\rightarrow$ predecessore di $v$
	- `v.d` $\rightarrow$ momento della scoperta di $v$
	- `v.f` $\rightarrow$ momento di abbandono di $v$ (tutto il sotto-grafo e' stato scoperto)
	- campi `v.d` e `v.f`:
		- interi tra $1$ e $2 \cdot |V|$
		- generalmente chiamati tempi (di scoperta e di abbandono)
		- Per ogni vertice $u$, abbiamo $u.d < u.f$.
- Ogni volta che `DepthVisit(G, u)` viene chiamata in `DepthFirstSearch(G)` si scopre un nuovo albero della foresta.
- **Complessità**:
	- `for (u in G.V)` $\rightarrow$ $\Theta(|V|)$
	- `for (u in G.V)` con `DepthVisit(G, u)` $\rightarrow$ $\Theta(|V| + |E|)$
	- `for (v in G.Adj[u])` $\rightarrow$ $\Theta(|E|)$
	- Totale $\rightarrow$ $\Theta(|V| + |E|)$

**Cycle Detection**:
- Ciclo: percorso $v_1, v_2, ..., v_k$ di vertici tali che per ogni $i$ esiste l'arco $(v_i, v_{i+1})$ e che $v_1 = v_k$
- Quando un grafo diretto e' privo di cicli, lo chiamiamo `DAG` (Directed Acyclic Graph)
- Dato un grafo diretto $G$ stabilire se presenta o no un ciclo
	1. si esegue la DFS modificato in modo che si interrompa se visita un nodo grigio
	2. restituisce `True` se visita un nodo grigio, `False` altrimenti
- **Correttezza**: l'algoritmo e' corretto se e solo se restituisce `True` quando $G$ e' ciclico e `False` altrimenti
	- caso $G$ ciclico: qualunque nodo si prenda (per ipotesi si visitano tutti i nodi), se c'è un ciclo viene trovato dall'algoritmo e deve restituire `True`
	- caso $G$ aciclico: non ci sono cicli quindi è impossibile incontrare un nodo grigio e l'algoritmo deve restituire `False`
- **Complessità** $\rightarrow$ identica a DFS

**Topological Sort**:
- Prende in input un grafo connesso $G$ senza cicli
- Restituisce una lista collegata $v_1, ..., v_{|V|}$ di vertici topologicamente ordinati
- Per ogni coppia $v_i, v_j$ di vertici, $v_i$ appare prima nella lista di $v_j$ se e solo se $v_i$ precede topologicamente $v_j$
- `TopologicalSort(G)`
	- Si chiama la DFS per calcolare i tempi di scoperta `v.d` e abbandono `v.f` $\forall \space v \in G.V$  
	- Ogni nodo finito $v$ (`v.color = BLACK`) viene inserito alla testa di una lista collegata
	- Restituisce la lista collegata
- **Correttezza** $\rightarrow$ un arco $(u, v)$ è tale che $u$ precede $v$ in $G$
- **Complessità** $\rightarrow$ stessa della DFS

**SCC (Strongly Connected Components)**:
- Dato un grafo diretto $G$, una **componente fortemente connessa** (SCC) e' un sottoinsieme massimale $V' \subseteq V$ tale che, per ogni $u, v \in V'$, succede che $u ⇝ v$ e che $v ⇝ u$. 
- **Grafo trasposto** $\rightarrow$ $G^T$ di $G$ e' ottenuto invertendo la direzione di ogni arco (complessita' $\Theta(|V| + |E|)$)
- La proprieta' piu' interessante di $G^T$ e' che $G$ e $G^T$ hanno le stesse SCC.
- **Complessità** $\rightarrow$ stessa della DFS

### Grafi per copertura minima
