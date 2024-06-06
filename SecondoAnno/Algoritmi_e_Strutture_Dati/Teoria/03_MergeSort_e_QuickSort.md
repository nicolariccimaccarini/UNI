## Algoritmi di ordinamento
Il problema dell'ordinamento di un array e' un mattone fondamentale dell'algoritmica. Non solo appare come problema in innumerevoli applicazioni dirette, ma e' anche particolarmente adatto a fare emergere alcune delle idee piu' importanti dell'algoritmica. 

Chiameremo **elementari** quei metodi, basati sui confronti, che condividono la seguente caratteristica: ogni elemento viene spostato verso il/al posto giusto separatamente dagli altri, in maniera iterativa o ricorsiva. Questi metodi includono `InsertionSort`, `SelectionSort`, `BubbleSort`, e altri.

## Alternative a *InsertionSort*: *SelectionSort*
`SelectionSort()` e' un'alternativa elementare a `InsertionSort()` per l'ordinamento:
``` Pseudocodice
proc SelectionSort(A){
	for (j=1 to A.lenght-1) {
		min = j
		for (i=j+1 to A.lenght) {
			if (A[i] < A[min])
				then min=i
		}
		SwapValue(A, min, j)
	}
}
```

Nel codice di `SelectionSort()` abbiamo usato una procedura `SwapValue()` che, dati due indici dell'array, ne scambia i contenuti. Questa procedura ha costo costante.

## Algoritmi di ordinamento non elementari
Tipicamente, gli algoritmi di ordinamento non elementari che non fanno uso di ipotesi aggiuntive sono ricorsivi. Nel nostro programma ne vediamo tre: `MergeSort()`, `QuickSort()` e `HeapSort()`. 

## *MergeSort* e *Merge* 
Per la definizione dell'algoritmo `MergeSort()`, studiamo prima un algoritmo che risolve un problema piu' semplice, cioe' quello di costituire una sequenza ordinata partendo da due sequenze ordinate.
L'input e' un array $A$ di interi e tre indici $p$, $q$, $r$ su di esso, tali che $p \le q \lt r$ e che entrambi $A[p, ..., q]$ e $A[q+1, ..., r]$ sono ordinati. Vogliamo, come risultato, una permutazione di $A$ tale che $A[p, ..., r]$ e' ordinato.

``` Pseudocodice
proc Merge(A, p, q, r) {
	n1 = q - p + 1
	n2 = r - q
	let L[1, ..., n1] and R[1, ..., n2] be new array      // dichiarazione e allocazione
	// inizializzazione di L e R
	for (i=1 to n1) L[i] = A[p+i-1]
	for (j=1 to n2) R[j] = A[q+j]
	i = 1
	j = 1
	// MERGE
	for (k=p to r) {
		if (i <= n1) then { 
			if (j <= n2) then {
				if (L[i] <= R[j]) then {
					CopyFromL(i)
				} else {
					CopyFromR(j)
				}
			} else {
				CopyFromL(i)
			}
		} else {
			CopyFromR(j)
		}
	}
}
```

L'idea alla base di `Merge()` e' scegliere il minimo trai i due elementi in cima a L ed a R e posizionarlo nel giusto posto di A; in seguito, a seconda della scelta, si avanza su L o su R. Utilizziamo due procedure elementari chiamate `CopyFromL` e `CopyFromR` che hanno come effetto quello di copiare in A un elemento (quello il cui indice e' dato come parametro) di L (rispettivamente, R) e avanzare di una posizione la variabile indicata come parametro. La scelta di usare oggetti esterni L ed R e; molto conveniente per poter usare A come array di arrivo, ma questo significa che, **nella nostra implementazione**, ==`Merge()` non e' in place==: quanto e' piu' grande l'input, maggiore e' la quantita' di spazio utilizzata da L e R, che pertanto non e' costante. Chiaramente ==*MergeSort*== eredita questa caratteristica, quindi ==non e' in place==.

### Correttezza e complessita' di *Merge* 
Studiamo la **terminazione**. I primi due cicli **for** terminano quando $i=n1$ e $j=n2$; il terzo ciclo **for** e' sempre vincolato tra *p* e *r*, e per ipotesi $p \lt r$. Pertanto `Merge()` termina sempre quando la chiamata rispetta l'ipotesi.
Per la **correttezza**, la nostra **invariante** e': nel terzo ciclo **for**, $A[p, ..., k-1]$ contiene $k-p$ elementi piu' piccoli di $L[1, ..., n_1]$ e di $R[1, ..., n_2]$, ordinati; inoltre $L[i]$ e $R[j]$ sono i piu' piccoli elementi di $L$ ed $R$ non ancora copiati in A. Per dimostrare che l'invariante va bene, dobbiamo mostrare che dalla stessa si ottiene la proprieta' desiderata sull'output.

Adesso mostriamo che l'invariante vale.
- **Caso base**: $k=p$. In questo caso $A[p, ..., k-1]$ e' vuoto. Si verifica ce contiene i $k-p=0$ elementi piu' piccoli di L e R, e poiche' $i=j=1$ si ha che $L[i]$ ed $R[j]$ sono gli elementi piu' piccoli dei rispettivi oggetti non ancora ricopiati in A.
- **Caso induttivo**: si assume che la proprieta' valga per *k* e si dimostra che vale per $k+1$. All'inizio della iterazione $k+1$, $L[i]$ ed $R[j]$ sono gli elementi piu' piccoli dei rispettivi oggetti non ancora ricopiati in A (ipotesi induttiva). Se $L[i] \le R[j]$ (e $i, j \le n_1, n_2$), allora $L[i]$ e' il piu' piccolo elemento non ancora copiato in $A$. Poiche' $A[p, . . . , k − 1]$ sono gli elementi piu' piccoli e sono ordinati (ipotesi induttiva) e poiche' $L[i]$ si copia in $A[k]$, dopo la $k + 1$-esima iterazione $A[p, . . . , k]$ contiene gli elementi piu' piccoli, ordinati, e $L[i]$ è l'elemento piu' piccolo di $L$ (giacche' i si è incrementato). Il caso $L[i] \gt R[j]$, e i casi in cui $i$ o $j$ sono $n_1$ o $n_2$, rispettivamente, si dimostrano in maniera simile.

Per completare lo studio di `Merge()`, manca ancora da vedere la **complessità**. I primi due cicli **for** impiegano $\Theta(n_1)$ e $\Theta(n_2)$, ed entrambi sono equivalenti a $\Theta(n)$. Il terzo ciclo **for** impiega $\Theta(n)$. Poiche' $\Theta(n) + \Theta(n) = \Theta(n)$, quest'ultima e' la complessita' di `Merge()`.

## Ordinamento con *MergeSort* 
L'algoritmo di *MergeSort* presenta un codice molto semplice, pero' e' **ricorsivo**. Queto implica che il calcolo della complessità portera' a una ricorrenza. La caratteristica di essere ricorsivo spiega anche che nell'input dell'algoritmo appaiano due parametri *p, r* che sembrano non avere senso, quando il problema dell'ordinamento e' definito su tutto l'array di input. Infatti dobbiamo pensare che l'algoritmo **richiama se stesso** durante la computazione; quindi da un punto di vista dell'utente, dobbiamo solo capire qual'e' la giusta chiamata iniziale.

``` Pseudocodice
proc MergeSort(A, p, r) {
	if (p < r) then {
		q = [(p+r)/2]
		MergeSort(a, p, q)
		MergeSort(A, q+1, r)
		Merge(A, p, q, r)
	}
}
```
![[MergeSortAlgorithm.png]]

**Terminazione**: ad ogni chiamata ricorsiva, la distanza tra *p* e *r* diminuisce. Si effettua una nuova chiamata solo quando $p \lt r$. Adesso vediamo la **correttezza**, che si basa su un'invariante ricorsiva (o induttiva). In questo caso affermiamo che, dopo ogni chiamata ricorsiva `MergeSort(A, p, r)`, $A[p, ..., r]$ e' ordinato.

Dobbiamo far vedere, per induzione, che l'invarante e' corretta.
- **Caso base**. Quando $p=r$, non si effettuano chiamate; ma $A[p, ..., r]$ contiene una sola posizione, ed e' quindi ordinato.
- **Caso induttivo**. Supponiamo quindi che $p \lt r$, e ragioniamo sulla chiamata $k+1$-esima. Questa corrisponde a due chiamate a `MergeSort()`, entrambe a livello *k* della ricorsione. Per ipotesi induttiva, entrambe restituiscono un array ordinato. Questi due risultati vengono poi passati a `Merge()`, il quale restituisce un array ordinato tra le posizioni *p* ed *r*.

Stabiliamo adesso la **complessità** di questo algoritmo. Come in *Merge*, non ha senso chiedersi se ci sono degli input che migliorano, o peggiorano le sue prestazioni. *MergeSort* da luogo alla seguente ricorrenza:
$$
T(n) = 2 \cdot T(\frac{n}{2}) + \Theta(n)
$$
Seguendo il Master Theorem, questa si esplicita in:
$$
T(n) = \Theta(n \cdot log(n))
$$

La funzione $n \cdot log(n)$ e' chiamata **pseudo lineare** (piu' efficiente rispetto a $n^2$)

## *QuickSort* e *Partition*
Come `MergeSort()`, si tratta di un algoritmo ricorsivo che usa una procedura ausiliare, chiamata `Partition`, che permette di formulare diverse soluzioni a problemi che sono considerati **satellite** rispetto all'ordinamento.

L'algoritmo `Partition` ha l'effetto di generare una permutazione dell'array in input in maniera tale che tutti gli elementi piu' piccoli o uguali ad un elemento, chiamato **pivot** e che viene individuato arbitrariamente sullo stesso, si trovino a sinistra di esso, e tutti gli elementi piu' grandi a destra. L'idea piu' importante di questo algoritmo e' il fatto che questa permutazione venga trovata in tempo lineare e senza spazio aggiuntivo.

``` Pseudocodice
proc Partition(A, p, r) {
	x = A[r]
	i = p - 1
	for (j=p to r-1) {
		if (A[j] <= x) then {
			i = i + 1
			SwapValue(A, i, j)
		} 
		SwapValue(A, i+1, r)
	}
	return i+1
}
```

Per osservare che `Partition` vale la **terminazione** osserviamo che il ciclo principale e' un **for**, che noi sappiamo termina sempre.
Per quanto riguarda la **correttezza** usiamo la seguente **invariante**: all'inizio di ogni iterazione, per ogni indice *k*: se $p \le k \le i$, allora $A[k] \le x$; se $i + 1 ≤ k ≤ j − 1$, allora $A[k] \gt x$ e se $k=r$, allora $A[k] = x$. Non possiamo dire nulla sul contenuto di $A$ da $j$ in poi, in quanto non siamo ancora arrivati a quel punto del ciclo.
Alla terminazione ($j=r$); applicando l'invariante, dopo l'ultima istruzione si ottiene un array che , tra le posizioni *p* ed *r*, e' tale che esiste un valore **pivot** ($i + 1$) e tutti i valori di A prima di $i+1$ sono minori o uguali al valori $A[i+1]$ il quale a sua volta e' minore o uguale a tutti i valori dopo la posizione $i+1$.

### Correttezza di *Partition*
Dimostriamo la validita' dell'invariante. L'induzione e' sulla variabile $j$.
- **Ciclo base**. Osserviamo cosa accade prima della prima esecuzione del ciclo **for**. Si ha che $i=p-1$ e $j=p$, quindi la parte di A fino alla posizione $j-1$ (cioe' $p-1$) e' vuota, pertanto l'invariante predica trivialmente su un insieme vuoto ed e' vera.
- **Caso induttivo**. Prima della j-esima esecuzione, l'elemento j-esimo non e' stato ancora visto, e l'invariante predica su tuti gli elementi fino al $j-1$-esimo compreso. Sul destino del j-esimo elemento decide la condizione $A[j] \le x$. Supponiamo che sia vera: quindi l'elemento $A[j]$ va spostato nel primo gruppo. Per fare questo, $i$ avanza di uno e si scambiano le posizioni $i+1$ e $j$. Quindi, a fine di questo ciclo, il gruppo di sinistra ha un elemento in piu' (prima condizione dell'invariante) e il gruppo di destra ha lo stesso numero di elementi, ma j e' avanzato di una posizione. Se si verificasse che $A[j] \gt x$, *i* non si sposterebbe (mantenendo lo stesso numero di elementi a sinistra), ma *j* si, aumentando il gruppo a destra di un elemento.

### Ordinamento con *QuickSort*
L'idea alla base di `QuickSort()` e' procedere ricorsivamente (divide and conquer) nella seguente maniera. Un array A, considerato dalla posizione *p* alla posizione *r*, viene prima separato in due sotto-strutture $A[p, ..., q-1]$ e $A[q, ..., r]$ in maniera che tutti gli elementi **prima** di *q* siano minori o uguali a $A[q]$, il quale, a sua volta, sia minore o uguale a tutti gli elementi dalla posizione $q+1$ in poi, attraverso `Partition`. Quindi, richiamiamo ricorsivamente l'algoritmo su $A[p, ..., q-1]$ e $A[q, ..., r]$: al ritorno dalla chiamata ricorsiva, entrambi sono ordinati e pertanto non c'e' bisogno di alcuna operazione per combinarli. Abbiamo cosi' ordinato $A[p, ..., r]$.

``` Pseudocodice
proc QuickSort(A, p, r) {
	if (p < r) then {
		q = Partition(A, p, r)
		QuickSort(A, p, q-1)
		QuickSort(A, q+1, r)
	}
}
```

### Correttezza di *QuickSort*
La correttezza di `QuickSort()` puo' essere dimostrata banalmente a partire da quella di `Partition`. La sua **terminazione** dipende dal fatto che, come in `MergeSort`, la distanza tra *p* ed *r* diminuisce sempre perche' *q* cade sempre in mezzo ai due. Quindi prima o poi non ci saranno piu' chiamate ricorsive e il processo terminera'. Per la **correttezza** abbiamo la seguente **invariante ricorsiva**: al termine di ogni chiamata ricorsiva con indici *p, r*, $A[p, ..., r]$ e' ordinato.

Dimostriamo la validita' dell'invariante:
- il **caso base** e' triviale: quando $p=r$, $A[p, ..., r]$ e' compreso da una sola posizione. ed e' quindi ordinato.
- Nel **caso induttivo**, si considerino certi indici $p \lt r$. La prima operazione consiste nel chiamare *Partition*, che abbiamo mostrato essere corretto, e che restituisce un indice *q* e una permutazione di $A[p, ..., r]$ tale che $A[p, ..., q-1]$ contiene solo gli elementi piu' piccoli o uguali a $A[q]$, il quale a sua volta e' piu' piccolo di $A[q+1, ..., r]$. Dopo le due chiamate ricorsive, per ipotesi induttiva, $A[p, ..., q-1]$ e $A[q+1, ..., r]$ sono entrambi ordinati. Ma allora $A[p, ..., r]$ e' ordinato, come volevamo dimostrare.

### Complessita' di *QuickSort*
La **complessità** di `Partition` e' facile da calcolare: $\Theta(n)$. Il calcolo della complessita' di `QuickSort()`, invece, presenta molti problemi. 
Per cominciare, analizziamo il caso peggiore. Il primo problema e' stabilire qual'e' la situazione che ci porta al caso peggiore: lo denominiamo **partizione sbilanciata**. Una partizione sbilanciata e' si ottiene quando la scelta del pivot e' tale che, ad ogni passo, genera un sottoproblema di dimensione 0 e un sottoproblema di dimensione $n-1$. In questo caso la ricorrenza che si ottiene e':
$$
T(n) = T(n-1) + \Theta(n)
$$
la cui situazione, vista come sviluppo di una serie aritmetica, e' evidentemente $T(n) = \Theta(n^2)$. Quindi, stiamo considerando un algoritmo che si comporta in maniera peggiore di `MergeSort()`, e, inoltre, e' tale che il suo caso peggiore si verifica quando l'array i partenza e' gia' ordinato in partenza: proprio una situazione nella quale addirittura `InsertionSort()` si comporta meglio ($\Theta(n)$).

D'altra parte, il caso migliore si verifica quando la partizione e' sempre bilanciata nel modo piu' equo possibile. In questo caso la ricorrenza che si ottiene e':
$$
T(n) = 2 \cdot T(\frac{n}{2}) + \Theta(n)
$$
la cui soluzione, per il Master Theorem, e' $\Theta(n \cdot log(n))$. Cosa accade quando la partizione e' bilanciata in modo iniquo? Per esempio assumendo che si creino sempre due sottoproblemi di dimensione relativa 9 a 1?
Anche questo e' un caso particolare. La ricorrenza risultante e'
$$
T(n) \le T(\frac{9 \cdot n}{10} + T(\frac{n}{10}) + \Theta(n))
$$
la cui soluzione si puo' mostrare essere $T(n) = O(n \cdot log(n))$. Questi esempi suggeriscono che quando la partizione e' costante allora non e' mai sbilanciata.

La considerazione precedente si puo' portare all'estremo, osservando che **per ogni** scelta del pivot  (quindi, per ogni versione di `Partition`) **esiste** un input tale che portera' la corrispondente versione di `QuickSort()` al caso peggiore. Possiamo pensare a questo problema in termini di gioco: per ogni mossa del giocatore **buono** (chi costruisce `Partition`) esiste una mossa del giocatore **cattivo** (chi costruisce l'input) in maniera da generare il caso peggiore. 
Ma per un dato input ed una data versione di `Partition`, qual'e' la possibilita' che questo succeda?

Mostreremo che la complessita' del caso medio di `QuickSort()` e' $O(n \cdot log(n))$, sotto l'ipotesi che tutti gli input siano equamente probabili. In particolare possiamo **garantire** che tutti gli input siano equamente probabili attraverso una versione **randomizzata** dell'algoritmo.
Otteniamo due vantaggi:
1. non ci possono essere input **volutamente** pessimi (possono ancora esistere, ma con una bassa probabilita')
2. l'assunzione "tutti gli input sono equamente probabili" e' rispettata, il che rende leggermente piu' semplice il calcolo

## La variante *RandomizedQuickSort* e la sua complessità
``` Pseudocodice
proc RandomizedPartition(A, p, r) {
	s = p<=Random()<=r
	SwapValue(A, s, r)
	x = A[r]
	i = p-1
	for j=p to r-1 {
		if (A[j] <= x) then {
			i = i +1
			SwapValue(A, i, j)
		}
	}
	SwapValue(A, i+1, r)
	return i+1
}
```

``` Pseudocodice
proc RandomizedQuickSort(A, p, r) {
	if (p < r) then {
		q = RandomizedPartition(A, p, r)
		RandomizedQuickSort(a, p, q-1)
		RandomizedQuickSort(A, q+1, r)
	}
}
```

La scelta casuale del pivot implica che ad ogni passo tutte le partizioni sono ugualmente probabili. Questo significa che possiamo parlare della complessita' di `RandomizedQuickSort()` nel caso medio in termini di probabilita' di una determinata partizione. Guardiamo cosa succede quando effettuiamo una partizione qualsiasi. Supponiamo che, dopo aver eseguito `Partition`, la situazione sia la seguente:
![[RandomizedQuickSort.png]]

La posizione di $P$ (il pivot) determina su quanti elementi di `QuickSort()` si richiamera'. Detta *k* la cardinalita' del lato sinistro, la cardinalita' del lato destro sara' $n-1-k$, considerando che il pivot non viene piu' toccato.

Se tutte le partizioni sono ugualmente probabili, la probabilita' di una specifica partizione e' proprio $\frac{1}{n}$. Formalizzando, si ottiene che 
$$
T(n) = \frac{1}{n} \cdot \sum_{k=0}^{n-1}(T(k) + T(n-k-1)) + \Theta(n)
$$
e quindi:
$$
T(n) = \frac{2}{n} \cdot \sum_{k=0}^{n-1}T(k) + \Theta(n)
$$
Dimostreremo che $T(n) = O(n \cdot log(n))$ attraverso la tecnica della sostituzione. In particolare, mostreremo che $T(n) \le a \cdot n \cdot log(n) + b$ per qualche costante $a, b$.

Procediamo. In prima istanza semplifichiamo la nostra espressione:
$$
\begin{align}
T(n) &= \frac{n}{2} \cdot \sum_{k=0}^{n-1}T(k) + \Theta(n) \qquad &&\text{ricorrenza} \\
&\le \frac{2}{n} \cdot \sum_{k=1}^{n-1}(a \cdot k \cdot log(k) + b) + \frac{2 \cdot b}{n} + \Theta(n) \qquad &&\text{ip. produttiva} \\
&= \frac{2}{n} \cdot \sum_{k=1}^{n-1}(a \cdot k \cdot log(k) + b) + \Theta(n) \qquad &&k=0 \quad \text{messo in } \Theta(n) \\
&= \frac{2}{n} \cdot \sum_{k=1}^{n-1} a \cdot k \cdot log(k) + \frac{2}{n} \cdot \sum_{k=1}^{n-1}b + \Theta(n) \qquad &&\text{somma distributiva} \\
&= \frac{2 \cdot a}{n} \cdot \sum_{k=1}^{n-1} k \cdot log(k) + \frac{2 \cdot a}{n} \cdot (n-1) + \Theta(n) \qquad &&\text{seconda somma valutata} \\
&\le \frac{2 \cdot a}{n} \cdot \sum_{k=1}^{n-1} k \cdot log(k) + 2 \cdot b + \Theta(n) \qquad &&\frac{n-1}{n} \lt 1
\end{align}
$$

Secondariamente, osserviamo che $\sum_{k=1}^n k \cdot log(k)$ puo' essere limitata verso l'alto, come segue:
$$
\begin{align}
\sum_{k=1}^{n-1}(k \cdot log(k)) &= \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} (k \cdot log(k)) + \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} (k \cdot log(k)) \qquad &&\text{spezzo} \\
&\le \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} (k \cdot log(\frac{n}{2})) + \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} (k \cdot log(n)) \qquad &&\text{maggioro} \\
&= (log(n) - 1) \cdot \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k + log(n) \cdot \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} k \qquad &&\text{estraggo} \\
&= (log(n) - 1) \cdot \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k - \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k + log(n) \cdot \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} k \qquad &&\text{distribuisco} \\
&= log(n) \cdot \sum_{k=1}^{n-1}k - \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k \qquad &&\text{sommo} \\
&\le \frac{1}{2} \cdot log(n) \cdot n \cdot (n-1) - \frac{1}{2} \cdot \frac{n}{2} \cdot (\frac{n}{2} - 1) \qquad &&\text{valuto} \\
&= \frac{1}{2} \cdot (n^2 \cdot log(n) - n \cdot log(n)) - \frac{1}{8} \cdot n^2 + \frac{1}{4} \cdot n \qquad &&\text{valuto} \\
&\le \frac{1}{2} \cdot log(n) - \frac{1}{8} \cdot n^2 \qquad &&n \ge 2 \\
\end{align}
$$

Finalmente, usiamo questo risultato per completare il caso induttivo:
$$
\begin{align}
T(n) &\le \frac{2 \cdot a}{n} \cdot (\frac{1}{2} \cdot (n^2 \cdot log(n) - \frac{1}{8} \cdot n^2) + 2 \cdot b + \Theta(n)) \qquad &\text{dalla precedente} \\
&= a \cdot n \cdot log(n) - \frac{a \cdot n}{4} + 2 \cdot b + \Theta(n) \\
&\le a \cdot n \cdot log(n) + b \\
\end{align}
$$
Quindi il tempo del caso medio di `RandomizedQuickSort()` e' precisamente $O(n \cdot log(n))$ e abbiamo escluso la possibilita' di **poter** scegliere un'istanza di input al fine di ottenere il caso peggiore!

## Conclusione
`MergeSort()` e `Quicksort()` sono esempi di strategia **divide and conquer**. Questa strategia si basa sull'idea di dividere il problema in due o piu' sotto-problemi separati tra loro, e poi combinare i risultati di questi ultimi. Si contrappone a strategie piu' semplici, come quelle iterative (come `InsertionSort()`), piu' complesse, come la **programmazione dinamica**, o semplicemente diverse come la strategia **greedy**. Allo stesso modo, `QuickSort()`, `Partition()` e `RandomizedQuickSort()` racchiudono numerosi concetti che possono essere considerati archetipici di molte idee algoritmiche. 