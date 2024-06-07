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
- 