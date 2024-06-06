## Min e Max Heap
Un heap e' una struttura dati ==astratta, parzialmente basata sull'ordinamento e necessariamente compatta== (sara' dunque basata su array). La caratteristica principale e' quella che una heap mantiene le chiavi semi-ordinate. Useremo le heap come base per le code di priorita' (anche loro strutture dati astratte), ma anche come base per un nuovo algoritmo di ordinamento che risolve il maggior problema di `MergeSort` (quello di non essere in place) ed il maggior problema di `QuickSort` (quello di non avere tempo quadratico nel caso peggiore).

Una **(min/max) heap** e' un array $H$ che puo' essere visto come un albero binario **quasi completo**, cioe' tale da avere tutti i livelli pieni, meno l'ultimo. I nodi dell'albero corrispondono agli elementi dell'array. L'elemento $H[1]$ dell'array e' la **radice** dell'albero e, normalmente, si tende a differenziare i valori `H.lenght()` (lunghezza dell'array che **contiene** l'heap) e `H.heapsize()` (numero di elementi della heap contenuti in $H$). Tipicamente si ha che `0 <= H.hepsize <= H.lenght`. 
Una heap e' un array con il fatto che per convenienza si puo' visualizzare come albero, dal punto di vista algebrico non ci sono differenze, ma dal punto di vista delle strutture dati le heap **non** sono alberi.

![[full-complete-perfect-trees.png]]

La corretta implementazione di una heap prevede che i **figli** di un nodo nella posizione $i$ siano precisamente gli elementi nelle posizioni $2 \cdot i$ e $2 \cdot i+1$ (sinistro e destro rispettivamente). Per conseguenza, il **padre** di un nodo $i$ e' identificato dall'indice $\lfloor \frac{i}{2} \rfloor$.

``` Pseudocodice
proc Parent(i) {
	return floor(1/2)
}

proc Left(i) {
	return 2*i
}

proc Right(i) {
	return 2 * i + 1
}
```

## Heap binarie su array
Immaginiamo una variabile *H*, che e' un array di interi con un campo aggiuntivo `H.heapsize()`. Distinguiamo tra due tipi di heap: **max-heap** e **min-heap**. Entrambe soddisfano una proprieta':
- nel primo caso abbiamo che per ogni *i*, `H[Parent(i)] >= H[i]`
- nel secondo caso, per ogni *i*, `H[Parent(i)] <= H[i]`
Di conseguenza, ==il massimo elemento di una max-heap si trova alla radice==, mentre nel caso di una ==min-heap== e' ==il minimo elemento a trovarsi alla radice==.
L'**altezza** di una heap e' la lunghezza del massimo cammino dalla radice alla foglia.

Esempio di min-heap:
![[min-heap.png]]

## Heap binarie su array: calcolo dell'altezza
Se una heap ha altezza *h*, quali sono il minimo e il massimo di numeri che puo' contenere?
In generale: una heap di altezza $h$ contiene **al minimo** $2^h$ elementi ed **al massimo** $2^{h+1} -1$ elementi. Questa proprieta' dipende esclusivamente dal fatto che la heap e' come un albero binario quasi completo. Da qui otteniamo che:
$$
\begin{align}
& 2^h \le n \le 2^{h+1}-1 \\ 
&\Rightarrow 2^h \le n < 2^{h+1} \\ 
&\Rightarrow h \le log(n) < h+1
\end{align}
$$
Quindi:
- dalla prima si ottiene che $h ≤ log(n))$ cioe' $h = O(log(n))$;
- dalla seconda si ottiene che $h > log(n)-1$ cioe' $h = \Omega(log(n))$.
Pertanto $h = \Theta(log(n))$.

## Heap binarie su array: `BuildMinHeap()` e `MinHeapify()` 
Problema: data una (non)heap $H$ di numeri interi non negativi (cioe' una array), trasformarlo una una min-heap.
A questo fine risolviamo un problema piu' semplice: dato un array $H$ ed un indice $i$ su di esso tale che `H[Left(i)]` `H[Right(i)]` sono gia' delle min-heap, trasformare $H$ in un array tale che anche `H[i]` e' una min-heap.
Procediamo in maniera ricorsiva: sistemiamo il potenziale errore localmente a `i, Left(i) e Right(i)` e poi correggiamo ricorsivamente gli errori che vengono generati dalla sistemazione a livelli piu' bassi. Vediamo una procedura che si chiama `MinHeapify` che fa quanto detto. 
Si potrebbe anche fare riferimento a max-heap e alla procedura simmetrica `MaxHeapify()`.

``` Pseudocodice
proc MinHeapify(H, i) {
	l = Left(i)
	r = Right(i)
	smallest = i
	if ((I <= H.heapsize) and (H[l] < H[i]))
		then smallest = l
	if ((r <= H.heapsize) and (H[r] < H[smallest]))
		then smallest = r
	if smallest != i  then {
		SwapValue(H, i, smallest)
		MinHeapify(H, smallest)	
	}
}
```

### Correttezza e complessità di `MinHeapify` 
Per la **terminazione**, osserviamo che la procedura termina in due casi:
- o perche' l'indice *i* non cambia durante una esecuzione (in questo caso non si effettuano chiamate ricorsive)
- oppure perche' l'indice e' diventato piu' grande della dimensione dell'heap (poiche' se cambia cresce sempre)
Per quanto riguarda la **correttezza**, osserviamo che `MinHeapify()` e' costruita in maniera ricorsiva. Quindi dobbiamo trovare una **invariante** ricorsiva: dopo ogni chiamata a `MinHeapify()` su un nodo di altezza $h$ tale che entrambi i figli sono radici di min-heap prima della chiamata, quel nodo e' la radice di una min-heap.

Dimostriamo l'invariante:
- Supponiamo, come **caso base**, `MinHeapify()` venga chiamata su un nodo ad altezza $h=0$. Le ipotesi sono rispettate (il nodo e' figlio); inoltre la procedura non ha alcun effetto, ma allo stesso tempo, un nodo senza figli e' gia' una min-heap.
- Per il **caso induttivo**, consideriamo un nodo in posizione *i* ad altezza $h>0$. Sappiamo che entrambi i suoi figli, in posizione $2 \cdot i$ e $2 \cdot i + 1$, se esistono, sono radici di min-heap per ipotesi.
  Poiche' `H[i]` e' il minimo tra $H[i], H[2 · i], \space e \space H[2 · i + 1]$, allora anche il nodo *i* e' radice di una min-heap, come volevamo.

Per calcolare la **complessità** di `MinHeapify()`, dobbiamo costruire una ricorrenza. 
Incorriamo in due problemi:
- primo, dobbiamo capire qual'e' il caso peggiore
- secondo, dobbiamo renderci conto che, da un lato vorremmo che come sempre la complessità fosse in funzione della quantita' di elementi nella struttura dati e dall'altro la complessita' di `MinHeapify()` dipende dall'altezza dell'elemento su cui e' richiamato.
In primo luogo, il caso peggiore occorre quando la heap sulla quale la procedura e' richiamata tende ad essere sbilanciata, forzando piu' chiamate ricorsive. Possiamo mostrare che il peggior sbilanciamento possibile e' $\frac{2}{3}$. Inoltre, poiche' vogliamo un risultato che possiamo usare in ogni situazione, utilizziamo una ricorrenza leggermente piu' debole, cioe':
$$
T(n) \le T(\frac{2}{3}n) + \Theta(1).
$$

Risolvendo la ricorrenza classica associata a quella precedente si ottiene che $T(n) = \Theta(log(n))$ (Master Theorem, caso 2). Pertanto, nel caso peggiore `MinHeapify()` costa $O(log(n))$.
Questo e' un'approssimazione dovuta al fatto che dovremmo calcolare la complessita' in base all'altezza del nodo su cui la procedura viene chiamata; una forma alternativa di scrivere questo risultato e' dire che la complessità e' $O(h)$, dove $h$ e' l'altezza della heap (anche in questo caso stiamo approssimando perche' non teniamo conto dell'altezza reale del nodo).

## Heap binarie su array: `BuildMinHeap`
Problema originale: dato un array di $H$ interi, convertirlo in una min-heap.
``` Pseudocodice
proc BuildminHeap(H) {
	H.heapsize = H.lenght
	for (i = floor(H.lengt/2) downto 1) 
		MinHeapify(H, i)
}
```

### Correttezza e complessita' di `BuildMinHeap`
La **terminazione** della procedura e' ovvia.
Per la **correttezza**, l'**invariante** che usiamo e': all'inizio di ogni iterazione del ciclo **for**, ogni elemento $H[i+1], H[i+2], ...$ e' la radice di una min-heap e all'uscita dall'iterazione anche $H[i]$ lo e'.
Dimostriamo:
- Nel **caso base** $i = \lfloor \frac{A.lenght}{2} \rfloor$: ogni elemento del tipo $H[i+k]$ con $k>0$ e' una foglia e pertanto la radice triviale di un solo elemento.
- Nel **caso induttivo**, e' sufficiente riferirsi alla correttezza di `MinHeapify()`. Questa proprieta', riferita all'uscita dal ciclo, dice: $H[1]$ e' una min-heap.

Un calcolo della **complessità** approssimativo ci porterebbe alla seguente conclusione: ogni chiamata di `MinHeapify()` costa $O(log(n))$ nel caso peggiore e si chiama $\Theta(n)$ volte, pertanto il costo totale e' $O(n \cdot log(n))$. 
In questo caso possiamo dare un limite piu' stretto grazie ad un'analisi piu' dettagliata. Il costo di `MinHeapify()` puo' essere espresso come $O(h)$; supponiamo che *h* sia l'altezza del **nodo** su cui viene chiamato. Una semplice osservazione ci dice che se in un albero binario quasi completo ci sono *n* elementi, allora al massimo $\lceil \frac{n}{2^{h+1}} \rceil$ di loro si trovano ad altezza *h*.

Il costo totale nel caso peggiore (quando `MinHeapify()` deve sempre arrivare alle foglie), si puo' limitare con:
$$
\sum^{log(n)}_{h=0} (\lceil \frac{n}{2^{h+1}} \rceil \cdot O(h)) = O(n \cdot \sum^{log(n)}_{h=0} \frac{h}{2^h}) $$
L'altezza va da 0 a $log(n)$ e, fissata un'altezza *h*, ci sono $\lceil \frac{n}{2^{h+1}} \rceil$ nodi. Per ogni nodo ad altezza $h$, chiamare `MaxHeapify()` costa $O(h)$ e quindi $\sum^{log(n)}_{h=0} (\lceil \frac{n}{2^{h+1}} \rceil \cdot O(h))$. Ma $n$ non dipende da $h$ (e quindi moltiplica la sommatoria) e $\frac{1}{2^{h+1}} \cdot h$ si puo' maggiorare con $\frac{h}{2^h}$, e quindi e' $O(n \cdot \sum^{log(n)}_{h=0} \frac{h}{2^h} )$.

$$
\begin{align}
& O \Big(n \cdot \sum^{log(n)}_{h=0} \frac{h}{2^h} \Big) = \\
& O \Big(n \cdot \sum^{log(n)}_{h=0} h \cdot (\frac{1}{2})^h \Big) = \\
& O \Big(n \cdot \sum^{\infty}_{h=0} h \cdot (\frac{1}{2})^h \Big) = \\
& O(n)
\end{align}
$$

La sommatoria $\sum^{log(n)}_{h=0} h \cdot (\frac{1}{2})^h$ e' sostituita dalla serie infinita $\sum^{\infty}_{h=0} h \cdot (\frac{1}{2})^h$ perche' quest'ultima converge ad una costante e poi scompare nella notazione $\Theta$. Il fatto che $\sum^{\infty}_{h=0} h \cdot (\frac{1}{2})^h$ converga a una costante si ha perche'
$$
lim_{h \rightarrow \infty} \frac{(h+1) \cdot (\frac{1}{2})^{h+1}}{h \cdot (\frac{1}{2})^h} = \frac{1}{2}
$$

e, per il teorema del rapporto, questa condizione e' sufficiente per la convergenza.
Il caso migliore (quello in cui un array e' gia' una min-heap) ha costo $\Theta(n)$, perche' la procedura e' governata da un ciclo **for**. 

## Ordinamento con `HeapSort()`
Una max-heap puo' essere adesso usata efficientemente per progettare un algoritmo di ordinamento. Consideriamo una max heap e ricordiamo una delle priorita' e' che il massimo elementi di H si trova in $H[1]$. Se consideriamo $H[1]$come gia' ordinato (basta metterlo sulla giusta posizione: l'ultima) e sostituiamo il contenuto di $H[1]$ succede che $H[2]$ e $H[3]$ sono ancora max-heap. Quindi chiamando `MAxHeapify` rispettiamo le ipotesi e possiamo ripetere il processo. Il codice di `HeapSort` si basa precisamente su questa osservazione.

``` Pseudocodice
proc HeapSort(H) {
	BuildMaxHeap(H)
	for (i=H.lenght downto 2) {
		SwapValue(H, i, 1)
		H.heapsize = H.heapsize - 1
		MaxHeapify(H, 1)
	}
}
```

### Correttezza e complessita' di `HeapSort()` 
Nel caso di `HeapSort()` la **correttezza** e' immediata, perche' dipende direttamente dalle procedure su cui e' basato. Anche la **terminazione** e' ovvia. La **complessità** di `HeapSort()`, nel caso peggiore, si calcola come segue. La chiamata a `BuildMaxHEap()` costa $\Theta(n)$; per ogni *i* si effettua uno scambio 
($O(1)$) ed una chiamata a `MaxHeapify()` ($\Theta(log(n))$). Il totale e' $\Theta(n \cdot log(n))$. La complessità e' la stessa nel caso migliore e quindi nel caso medio: dopo aver effettuato `BuildMaxHeap()`, per definizione ogni chiamata successiva a `MaxHeapify()` (dopo lo scambio) deve arrivare alle foglie. Possiamo anche osservare che la nostra implementazione di `HeapSort()` non e' stabile: si puo' dimostrare osservando il suo comportamento sull'array $H = [1,1]$. D'altra parte e' certamente in place, a meno delle chiamate ricorsive, che, come abbiamo osservato, sono unicamente tail-recursive.

## Code di priorita'
Una **coda di priorità** e' una struttura dati astratta basata sull'ordinamento e necessariamente compatta. Possiamo costruire una coda di priorita' basandoci su una min-heap. A differenza di una coda classica, che implementa una politica FIFO, una coda di priorita' associa ad ogni chiave la **priorità** e serve (cioe' estrae) l'elemento a priorita' piu' bassa. Questa estrazione e' associata all'operazione che **aggiusta** la struttura dati, ed anche alla possibilita' di **inserire** nuovi elementi, o **cambiare la priorità** di un elemento inserito. Sia quindi $Q$ una min-heap senza campi aggiuntivi.

``` Pseudocodice
proc Enqueue(Q, priority) {
	if (Q.heapsize = Q.lenght)
		then return "overflow"
	Q.heapsize = Q.heapsize + 1
	Q[heapsize] = ∞
	DecreaseKey(Q, Q.heapsize, priority)
}

proc DecreaseKey(Q, i, priority) {
	if (priority > Q[i])
		then return "error"
	Q[i] = priority
	while ((i>1) and (Q[Parent(i)] > Q[i]))
		SwapValue(Q, i, PArent(i))
		i = Parent
}

proc ExtractMin(Q) {
	if (Q.heapsize < 1)
		then return "undeflow"
	min = Q[1]
	Q[1] = Q[Q.heapsize]
	Q.heapsize = Q.heapsize - 1
	MinHeapify(Q, 1)
	return min
}
```

### Code di priorita' su heap binarie
La **correttezza** di queste operazioni e' immediata da dimostrare usando gli stessi ragionamenti visti prima; nello stesso modo e' immediato costruire una versione simmetrica sia dell'estrazione (quindi del massimo), sia del decremento (quindi incremento) di una chiave. La **complessità** di tutte queste operazioni e' $\Theta(log(n))$ nel caso pessimo.

### Code di priorita' su array
Le code di priorita' come struttura dati astratta possono essere implementate anche direttamente su array, senza dover passare dalle heap. Questa soluzione ha il vantaggio della semplicita' di implementazione. La complessita' delle operazioni, invece, non sono comparabili. Questo significa che questa soluzione e' migliore in qualche caso e peggiore in qualche altro. In questa soluzione conviene esplicitare le chiavi associate per evitare confusioni.

$Q$ e' un array (che immaginiamo sempre con campo `Q.lenght`) e diciamo che ogni posizione $i$ ha tre valori: $i$ stesso, `Q[i]` e `Q[i].empty`, immaginiamo di aver inizializzato tutti i valori `Q[i].empty` a falso. In questo modo, abbiamo fatto l'equivalente della costruzione della coda. Il **costo** di questa inizializzazione e' $\Theta(n)$.

``` Pseudocodice
proc Enqueue(Q, i, priority) {
	if (i > Q.lenght)
		then return "overflow"
	Q[i] = priority
}

proc DecreaseKey(q, i, priority) {
	if ((Q[i] < priority) or (Q[i].empty = 1))
		then return "error"
	Q[i] = priority
}

proc ExtractMin (Q) {
	MinIndex = 0
	MinPriority = ∞
	for (i = 1 to Q.length) {
		if ((Q[i] < MinPriority) and (Q[i].empty = 0)) 
			then 
			MinPriority = Q[i]
			MinIndex = i		
	}
	if (MinIndex = 0)
		then return "underflow"
	Q[MinIndex].empty = 1
	return MinIndex
}
```

In questa soluzione, la cui **correttezza** e **terminazione** sono immediate, l'operazione di estrazione del minimo costa $O(n)$, perche' nel caso peggiore il ciclo **for** deve scorrere tutti gli elementi. Invece, grazie all'ipotesi sul valore degli elementi, il decremento costa $\Theta(1)$, cosi' come l'operazione di inserimento di un nuovo elemento.

## Code di priorita' su array vs su heaps: confronto

|                   | array (c. peggiore e medio) | heaps (c. peggiore e medio) |
| ----------------- | --------------------------- | --------------------------- |
| inizializzazione  | $\Theta(n)$                 | $\Theta(n)$                 |
| inserimento       | $\Theta(1)$                 | $\Theta(log(n))$            |
| decrememto        | $\Theta(1)$                 | $\Theta(log(n))$            |
| estrazione minimo | $\Theta(n)$                 | $\Theta(log(n))$            |
