## Strutture Dati
Cosa possiamo dire delle strutture dati? Quali caratteristiche ci interessano?

La **tassonomia** classica delle strutture dati prevede tre dimensioni originali tra loro. Una struttura dati puo' essere 
- **statica** o **dinamica**: con dinamica intendiamo una struttura che e' pensata per aggiungere e togliere elementi durante l'esecuzione di un algoritmo (per esempio un'array e' dinamico mentre un grafo e' statico); 
- **compatta** o **sparsa**: tipicamente le strutture dinamiche sono sparse, ovvero non possiamo fare ipotesi sulla posizione fisica degli elementi in memoria (per esempio, gli array sono una struttura compatta perche', indipendentemente da dove da dove e' memorizzato, possiamo assumere che sia fatto in maniera da avere $n$ posizioni fisiche vicine tra loro);
- **basata** o **non basata sull'ordinamento** delle chiavi: se gli elementi sono disposti in maniera dipendente dal valore delle chiavi, allora sono basate sull'ordinamento, altrimenti non. 

Un altro elemento fondamentale e' la differenza tra **chiave** e **dato satellite**. Cio' che associamo ad una chiave si chiama **dato satellite**, e normalmente questo e' il contenuto informativo della chiave. Quando effettuiamo un movimento di chiave, implicitamente muoviamo anche i dati satelliti, la cui dimensione e' considerata costante.

Array e liste sono considerate strutture dati **concrete** (o **fondamentali**). Su di esse, a volte, e' conveniente costruire strutture dati **astratte**, che nascondono l'implementazione soggiacente. Questo e' cio' che viene fatto ad esempio nelle librerie offerte dai linguaggi di programmazione. La distinzione e' a volte non completamente netta (come nel caso delle liste).

Possiamo pensare gli array come una struttura dati statica (quindi senza algoritmi di inserimento o cancellazione) associata al problema dell'ordinamento. Quando studiamo strutture dati basate sull'ordinamento delle chiavi, potremmo, in linea teorica, associare anche ad esse il problema dell'ordinamento: normalmente questo non si fa, perche' si presta attenzione ad altri problemi.

Una **lista concatenata** e' una struttura dati ==dinamica, non basata sull'ordinamento e sparsa==. Ad essa, associamo le operazioni di inserimento, cancellazione e ricerca. In certo modo, una lista e' la versione dinamica di un array; cio' nonostante, l'operazione di ordinamento delle chiavi normalmente avviene attraverso la copiatura degli elementi su un oggetto di tipo array e non direttamente.

## Puntatori e lista
Un **puntatore** e' un tipo di dato fondamentale e supportato da quasi tutti i linguaggi di programmazione. Quei linguaggi che non lo supportano offrono comunque le strutture dati principali in termini di oggetti e metodi. 
Un puntatore e' un tipo di variabile che contiene un indirizzo di memoria. Viene associato al **tipo di dato puntato**, per cui un puntatore ad un intero e' in generale diverso da un puntatore ad un tipo complesso.

Nel caso di liste concatenate, immaginiamo un tipo di dato **elemento** (che contiene almeno la chiave ed un puntatore al contenuto) e due puntatori ad elemento, che chiamiamo **predecessore** e **successore**. Un elemento e' quindi un tipo di dato ricorsivo e come tale va dichiarato. Nella realta' dei linguaggi di programmazione i puntatori vanno creati, dando ordine al sistema operativo di allocare sufficiente spazio di memoria. 
Ai fini didattici, creiamo adesso una variabile $L$ (un oggetto di tipo lista) come una struttura che contiene, almeno, un attributo `L.head`, di tipo puntatore ad elemento, che punta alla testa dell'oggetto.. Si puo' pensare che contenga anche attributo tipo `L.numel` che indica il numero di elementi presenti in ogni momento. All'inizio, `L.head = nil` e `L.numel = 0`.


Concentriamoci nel caso di liste **non ordinate**, **doppiamente collegate**; quindi stabiliamo che  un nodo *x* ha definiti la chiave (`x.key`), e i puntatori al prossimo (`x.next`) e al precedente (`x.prev`) elemento. All'inizio la lista e' vuota. Immaginiamo che sia *x* il nuovo elemento, gia' allocato, per esempio con la chiave 5.
![[PuntatoriELista1.png]]

L'operazione `ListInsert` crea il collegamento.
![[PuntatoriELista2.png]]

``` Pseudocodice
proc ListInser(L, x) {
	x.next = L.head
	if (L.hea != nil)
		then L.head.prev = x 
	L.head = x
	x.prev = nil
}
```

Il puntatore `L.head` punta sempre al primo elemento. Se l'oggetto e' vuoto, al primo inserimento *x* e' sia il primo che l'ultimo elemento, quindi il suo prossimo 
(`next`) e' vuoto. Se d'altra parte non è vuoto, allora l'elemento che segue *x* è quello che era il primo. Poiché si tratta di una lista doppiamente collegata (gli elementi puntano al loro predecessore), se non è vuoto (= se `L.head` non è `nil`) allora il predecessore di quello che prima era il primo elemento deve diventare *x*. Se invece era vuoto, questo puntatore non si modica e rimane `nil`. 

``` Pseudocodice
proc ListSearch(L, k) {
	x = L.head
	while (x != nil) and (x.key != k) x = x.next
	return x
}
```

In quanto a `ListDelete` abbiamo:
![[PuntatoriELista3.png]]

Eliminando *x*, dove *x* punta all'elemento che contiene la chiave 3, si ottiene:
![[PuntatoriELista4.png]]

Il codice e':
``` Pseudocodice
proc ListDelete(L, x) {
	if (x.prev != nil) 
		then x.prev.next = x.next
		else L.head = x.next
	if (x.next != nil)
		then x.next.prev = x.prev
}
```

## Correttezza e complessità delle operazioni su lista
La **correttezza** di queste operazioni e' immediata da dimostrare, cosi' come le loro **complessità**: l'inserimento prende $\Theta(1)$, in quanto inserisce sempre in testa all'oggetto, la cancellazione prende $\Theta(1)$ assumendo di conoscere il puntatore dell'elemento da cancellare, il quale si ottiene attraverso la ricerca che costa $\Theta(n)$ (nel caso peggiore). Anche se abbiamo detto che non ha molto senso parlare di inserimento e cancellazione in array (come struttura concreta), possiamo domandarci come, dal punto di vista della complessità, array e liste si possano confrontare.

## Array vs Liste: confronto

|                  | array (c. peggiore e medio) | lista (c. peggiore e medio) |
| ---------------- | --------------------------- | --------------------------- |
| ricerca          | $\Theta(n)$                 | $\Theta(n)$                 |
| minimo           | $\Theta(n)$                 | $\Theta(n)$                 |
| massimo          | $\Theta(n)$                 | $\Theta(n)$                 |
| successore       | $\Theta(n)$                 | $\Theta(n)$                 |
| predecessore     | $\Theta(n)$                 | $\Theta(n)$                 |
| inserimento      | $\Theta(1)$ *               | $\Theta(1)$                 |
| cancellazione ** | $\Theta(1)$                 | $\Theta(1)$                 |

$*$ : assumendo di avere allocato abbastanza spazio, altrimenti costa $\Theta(n)$.
$**$ : per la cancellazione di una **chiave** (senza conoscere l'indirizzo della sua posizione), va sommato il costo della ricerca.

Sebbene dal confronto le due soluzioni sembrano identiche, si osservi che gli array permettono l'accesso diretto, mentre le liste no. Questa differenza gioca un ruolo fondamentale quando le strutture dati di cui abbiamo bisogno sono indicizzabili. Per esempio, un insieme di nomi e' naturalmente memorizzato in un oggetto di tipo lista, ed un nome non e' visto come un indice. Invece, un insieme (abbastanza piccolo) di numeri interi positivi puo' trovare posto in un array e il contenuto di una posizione puo' essere visto come un indice.

## Pile e code
Le pile e le code sono entrambe due strutture dati astratte. Nella nostra tassonomie sono da ritenersi ==dinamiche e non basate sull'ordinamento==. Possono essere implementate in maniera ==compatta== (basate su array), o ==sparsa== (basate su liste). La loro caratteristica distintiva e' che l'accesso agli elementi non e' libero, ma vincolato ad una **politica**. La ragione per cui puo' essere utile avere una politica di accesso e' che questa puo' fare risparmiare dei dettagli implementativi, assicurando un certo ordine di inserimento ed estrazione.

### Pile
Una **pila** (o **stack**) e' una struttura dati astratta che implementa la politica **last in first out** (**LIFO**). Una pila si utilizza in diversi contesti, tra cui valutazione di espressioni, processi di backtracking (per ricordarci le mosse che abbiamo fatto e l'ultimo punto di scelta), eliminazione della ricorsione, e molte altre. 

### Pile su Array
Per implementare una pila ci possiamo basare su un array; avremo quindi una implementazione compatta. L'idea e' quella di **mascherare** la struttura portate all'utente finale. Si puo' pensare come un **oggetto** con i suoi relativi **metodi**. Assumeremo quindi che $S$ sia un array di interi, che interpretiamo come uno stack e che viene dotato con i parametri (naturali) `S.top` (inizializzato a $0$) e `S.max` (che indica la massima capacita' di $S$). Le operazioni di inserimento ed eliminazione di un elemento nelle pile prendono il nome di *Push* e *Pop*, rispettivamente. La variabile $S$ e' un array dotato di struttura.

``` Pseudocodice
proc Empty(S) {
	if (S.top = 0) 
		then return true
	return false
}

proc Push(S, x) {
	if (S.top = S.max)
		then return "overflow"
	S.top = S.top + 1
	S[S.top] = x
}

proc Pop(S) {
	if (Empty(S))
		then return "underflow"
	S.top = S.top - 1
	return S[S.top + 1]
}
```
### Code su array
Una **coda** (o **queue**) e' una struttura dati elementare che implementa una politica **first in first out** (**FIFO**). Permette di accedere ai dati attraverso inserimento ed eliminazione che prendono normalmente i nomi di `Enqueue` e `Dequeue`, rispettivamente. Anche una coda puo' avere diversi usi: in una **playlist** le canzoni si trovano in una coda (circolare), in maniera che la canzone appena ascoltata si ripetera' **il piu' tardi possibile**; oppure nei processi di visita di strutture dati piu' complesse come i grafi.

Per implementare uno stack ci possiamo basare su un array. Assumendo quindi che $Q$ sia una coda (un array dotato di struttura), con parametri `Q.head`, `Q.tail` (naturali), la cui inizializzazione e' `Q.head = Q.tail = 1`. Per assicurare di essere capaci di distinguere perfettamente le due situazioni di pila vuota e di pila piena, entrambe caratterizzate da `Q.head = Q.tail`, aggiungiamo una variabile, chiamata $dim$ inizialmente a $0$ (perche' la coda e' vuota).

``` Pseudocodice
proc Enqueue(Q, x) {
	if (Q.dim = Q.lenght)
		then return "overflow"
	Q[Q.tail] = x
	if (Q.tail = Q.lenght)
		then Q.tail = 1
		else Q.tail = Q.tail + 1
	Q.dim = Q.dim + 1
}

proc Dequeue(Q) {
	if (Q.dim = 0)
		then return "underflow"
	x = Q[Q.head]
	if (Q.head = Q.lenght)
		then Q.head = 1
		else Q.head = Q.head + 1
	Q.dim = Q.dim - 1
	return x
}
```

**Correttezza**, **complessità** e **terminazione** di queste operazioni sono triviali da dimostrare.

### Pile su liste
Pile e code possono anche essere implementate in maniera sparsa, utilizzando delle liste concatenate come supporto. Per quanto riguarda le pile, l'operazione di inserimento in testa di una lista e' proprio l'operazione di `Push` e l'operazione di `Pop` e' una semplificazione dell'operazione di delete. In questo caso, $S$ e' semplicemente una lista, senza campi aggiuntivi.

``` Pseudocodice
proc Empty(S) {
	if (S.head = nil)
		then return true
	return false
}

Proc Push(S, x) {
	Insert(S, x)
}

proc Pop(S) {
	if (Empty(S))
		then return "underflow"
	x = S.head
	Delete(S, x)
	return x.key
}
```

**Correttezza**, **complessità** e **terminazione** di queste operazioni non presentano nessun problema.

### Code su liste
Per implementare una coda attraverso una lista dobbiamo immaginare che la lista $Q$ sia dotata del campo `Q.tail` in aggiunta al campo `Q.head`. In questo modo possiamo implementare `Enqueue` semplicemente chiamando l'inserimento in una lista e `Dequeue` semplicemente chiamando l'eliminazione di un elemento in una lista.

``` Pseudocodice
proc Empty(Q) {
	if (Q.head = nil)
		then return true 
	return false
}

proc Enqueue(Q, x) {
	Insert(Q, x)
}

proc Dequeue(Q) {
	if Empty(Q)
		then return "underflow"
	x = Q.tail
	Delete(Q, x)
	return x.key
}
```

**Correttezza**, **complessità** e **terminazione** sono immediate.

## Conclusione
==La lista e' la piu' semplice delle strutture dati dinamiche== e puo' essere implementata con collegamento semplice, doppio, oppure in maniere molto complesse che permettono di sfruttare delle euristiche di miglioramento delle complessita'. Le pile e le code sono strutture dati che non solo servono nelle situazioni che abbiamo descritto, ma sono anche rappresentative di concetti fondamentali in informatica e permettono di porsi domande fondamentali per la teoria della complessita' e della calcolabilita'