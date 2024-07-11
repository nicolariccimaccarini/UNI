## Analizzare gli algoritmi
Ci sono quattro caratteristiche fondamentali che ci interessano di un algoritmo:
- **correttezza** $\rightarrow$ affermare che esso restituisce sempre una risposta corretta;
- **completezza** $\rightarrow$ affermare che ogni risposta corretta e', prima o poi, effettivamente restituita
- **terminazione** $\rightarrow$ assicurare che per ogni input la computazione finisca
- **complessità** 

Molti problemi che vedremo sono di tipo **funzionale**: trova il minimo, il massimo, restituisci l'input ordinato, ... In questi casi, distinguere tra le caratteristiche viste prima non e' sempre conveniente ne' naturale. In generale, noi ci riferiremo semplicemente alla correttezza di un algoritmo funzionale, e diremo appunto che esso e' **corretto** solo se restituisce tutte e solamente le risposte giuste, e termina sempre.

### Analizzare gli algoritmi: correttezza nel caso iterativo
Per comprendere formalmente la correttezza degli algoritmi iterativi, partiamo dall'algoritmo di ordinamento di numeri interi, cioe' `InsertionSort()`. 

``` Pseudocodice
proc InsertionSort(A) {
	for (j=2 to A.lenght) {
		key = A[j]
		i = j - 1
		while ((i > 0) and (A[i] > key)) {
			A[i + 1] = A[i]
			i = i + 1
		}
		A[i + 1] = key
	}
}
```

Vogliamo dimostrare la correttezza dell'algoritmo. Per la **terminazione**, osserviamo che il ciclo **for** termina quando `j > A.lenght`, e il ciclo **while** e' sempre vincolato tra $j - 1$ e $0$, e che per ogni $j$, la variabile $i$, che inizia da un valore positivo, si decrementa sempre. Dunque la terminazione e' garantita sia a livello del ciclo piu' esterno che di quello piu' interno.
Per la **correttezza** possiamo usare la tecnica dell'**invariante**, che consiste nello stabilire una proprieta' del ciclo principale (o di un ciclo dell'algoritmo) che sia vera dalla prima esecuzione, durante ogni esecuzione, e dopo l'ultima esecuzione, e che implichi la correttezza. Nel nostro caso una **invariante** del ciclo piu' esterno e' che `A[1, ..., j-1]` e' sempre ordinato in maniera non decrescente; si noti che quando `j = A.lenght + 1`, l'algoritmo termina, verificando che `A[1, ..., n]` e' ordinato in maniera non decrescente.

### Analizzare gli algoritmi: complessita' nel caso iterativo
Passiamo allo studio della **complessità** di InsertionSort,
``` Pseudocodice
proc InsertionSort(A) {
	for (j = 2 to A.lenght)
		key = A[j]
		i = j - 1
		while ((i > 0) and (A[i] > key))
			A[i+1] = A[i]
			i = i - 1
		A[i+1] = key
}
```

$c1$, $c2$, ... sono costanti; $n$ e' la dimensione dell'input; $t_j$ va da 2 a $n$ e dipende da istanza a istanza. **Nel caso migliore** $t_j = 1$ per ogni $j = 2, ..., n$ e questo caso corrisponde all'input gia' ordinato in partenza. **Nel caso peggiore** $t_j = j$ per ogni $j = 2, ..., n$ e corrisponde all'input **ordinato in ordine inverso** in partenza. 

Nel caso migliore quindi:
$$ T(n) = c_1 \cdot n + c_2 \cdot (n-1) + c_3 \cdot (n-1) + c_7 \cdot (n-1) + c_4 \cdot (n-1) $$ 
Quindi $T(n) = a \cdot n + b$ e' una funzione lineare per qualche costante $a, b$.

Nel caso peggiore invece, succede quanto segue. L'istruzione di controllo del **while** si esegue, ad ogni istanza del ciclo piu' esterno, esattamente $j$ volte; le due istruzioni interne, esattamente $j-1$ volte. Succede che: 
$$ \sum_{j=2}^n j = \frac{n · (n+1)}{2}-1 $$
e che 
$$ \sum_{j=2}^n (j-1) = \frac{n · (n-1)}{2}$$

e pertanto
	$$ T(n) = C + c_4 · (\frac{n · (n+1)}{2} - 1) + c_5 · (\frac{n · (n-1)}{2}) + c_6 (\frac{n · (n-1)}2) $$  

dove la parte $C$ e' rimasta come nel caso migliore.
Il risultato e' quindi una funzione *quadratica* $T(n) = a · n^2 + b · c$ per qualche a, b, c.

Il nostro scopo era calcolare $T(n)$ cioe' il numero di operazioni semplici effettuate per l'entrata di dimensione *n*, e lo abbiamo fatto **a meno di qualche costante**. Il caso migliore non e' mai una buona scelta. Lo faremo sempre nel **caso peggiore** o nel **caso medio**, il quale a sua volta coincide nella maggior parte dei casi nella speranza che il caso medio presenti un comportamento migliore.

### Analizzare gli algoritmi: correttezza nel caso ricorsivo
Per introdurre l'analisi della complessita' di un algoritmo ricorsivo, utilizziamo l'algoritmo di ricerca binaria, `RecursiveBinarySearch()` 
``` Pseudocodice
proc RecursiveBinarySerach(A, low, high, k) {
	if (low > high)
		return nil       // OVVERO NULL
	mid = (high + low)/2
	if (A[mid] = k)
		return mid
	if (A[mid] < k)
		return RecursiveBinarySerach(A, mid + 1, high, k)
	if (A[mid] > k)
		return RecursiveBinarySerach(A, low, mid - 1, k)
}
```

Questo algoritmo serve per risolvere il seguente problema: dato un'array, orinato, ed una chiave *k*, restituisce, se esiste, l'indice *k* nell'array, altrimenti restituisce **nil**. `RecursiveBinarySearch()` e' un algoritmo interessante perche' molto piu' efficiente dell'idea naïve di scorrere l'array in cerca di *k*.

Come nel caso iterativo, vogliamo assicurare la **terminazione** dell'algoritmo: ad ogni chiamata ricorsiva, la differenza tra high e low diminuisce. La condizione per effettuare un'altra chiamata ricorsiva e' che **low < high**, cosa che, prima o poi, deve necessariamente essere falsa, il che implica che ad un certo punto non ci saranno piu' chiamate.
Per la **correttezza**, utilizziamo la tecnica dell'**invariante introduttiva**. Invece di concentrarci su un ciclo, ci concentriamo su cio' che accade **dopo ogni chiamata ricorsiva**.

Le invarianti ricorsive si mostrano vere attraverso l'induzione:
- Nel **caso base**, cioe' prima della chiamata ricorsiva, $low = 1$ e $high=n$; quindi se $k$ e' in $A$, e' chiaramente in $A[low, ..., high]$.
- Nel **caso induttivo** si assume che *k*, se esiste, sia in $A[low, ..., high]$ (prima della chiamata ricorsiva); poiche' $A$ e' ordinato, e mid è l'indice mediano, se $k$ esiste è $A[mid]$, oppure è alla sua sinistra (se $A[mid] > k$), oppure è alla sua destra (se $A[mid] < k$), e la successiva chiamata ricorsiva, se si effettua, cambia low o high in maniera da rispettare proprio questo principio. Dunque $k$, se esiste, si trova di nuovo in $A[low, . . . , high]$ all'inizio della seguente chiamata ricorsiva, dove low o high è stato opportunamente cambiato.

**Complessità**. Operare in come `InsertionSort()` per operare la complessita' di `RecursiveBinarySearch()` non porta a nulla: tutte le operazioni semplici hanno complessita' costante e non ci sono cicli. Ci sono, pero', due chiamate ricorsive che rendono difficile formalizzare il costo totale. $T(n)$, cioe' la nostra funzione di complessita', e' un **incognita**, della quale sappiamo le seguenti cose: costa una costante *c* **prima di dichiararsi**, che possiamo approssimare a 1, e nel **caso peggiore** si richiama esattamente una volta; poiché la variabile mid assume il valore dell'indice centrale dell'array arrotondato, eventualmente, all'intero inferiore, quando la procedura si richiama lo fa, al massimo, sulla metà degli elementi originali.
Formalizzando
$$ T(n) = T(\frac{n}{2}) + 1 $$
Diciamo che l'espressione che descrive $T(n)$ e' una **ricorrenza**, e che questa si risolve trovando l'aspetto esplicito di $T(n)$. 

### Notazione asintotica: principi
Informalmente, data una funzione $T(n)$, diremo che $T(n) = Θ(f(n))$ se e solo se si ottiene da $f(n)$ **eliminando tutti i termini di ordine inferiore al maggiore e tutte le sue costanti**.
E' piu' facile confrontare differenti algoritmi secondo il **loro comportamento nel caso peggiore**.

**Quali sono le operazioni che davvero contano quando si va a calcolare la complessità?**
Osserviamo che la logica di un algoritmo e' data appunto dalle operazioni **logiche**. Quindi, e' naturale contare queste, invece che tutte le operazioni, perche' queste ultime danno un rapporto costante, o comunque limitato dalle operazioni logiche sotto le quali sono inserite.
A seconda del contesto, ci chiederemo quanti sono i **confronti**, o piu' in generale le operazioni logiche di un certo algoritmo. In particolare, quando calcoliamo la complessità asintotica contiamo tutte le operazioni, e quando facciamo una analisi piu' specifica ci concentriamo sulle operazioni logiche.

A questo punto possiamo studiare con maggiore dettaglio il concetto di **ordine di grandezza** di una funzione sui numeri naturali.

Nel caso di `InsertionSort()` abbiamo detto che:
$$ T(n) = a·n^2 + b·n + c = Θ(n^2) $$Ma cos'e' $Θ(f(n))$?
Per una funzione $f(n)$ diremo, che $f(n)$ e' **limitata da** (o e' un **o grande** di). $g(n)$ ($f (n) = O(g(n)))$) se e solo se **esiste una costante** $c > 0$ tale che **per tutti gli** $n ≥ n_0$ e' il caso che:
$$ 0 ≤ f (n) ≤ c · g(n) $$
In maniera simile, per una funzione $f(n)$ diremo che $f(n)$ e' **limitata dal basso da** (o e' un **omega grande** di) $g(n)$ ($f (n) = Ω(g(n))$) se e solo se **esiste una costante** $c>0$ tale che **per tutti gli** $n ≥ n_0$ e' il caso che:
$$ 0 ≤ c · g(n) ≤ f (n) $$   
Nel caso piu' semplice, abbiamo, ad esempio, che $f(n) = a · n^k$ e' tale che $f(n) = O(n^k)$, perche', per ogni $n ≥ 0$, si ha che esiste una costante *c* (che e' proprio a) tale che $f(n) ≤ c · n^k$. In questi semplicissimo esempio, abbiamo anche $f(n) = Ω(n^k)$, per la stessa ragione.
Generalizzando, possiamo dire che:
$$ \sum_{i=0}{k} a^i · n^i = Ω(n^k) $$

Naturalmente, $f(n) = O(g(n))$ e $f(n) = Ω(g(n))$ **non** sono definizioni equivalenti. Per esempio, se prendiamo il caso di $f(n) = log(n)$ (come sempre, non specificando altrimenti, usiamo logaritmi in base 2). Non e' vero che $log(n) ≥ c · n$ per $n ≥ n_0$ per nessuna scelta di *c* e di $n_0$. Quindi $log(n) \ne  Ω(n)$.

Arriviamo adesso alla nozione che avevamo al principio. Per una funzione $f(n)$, diremo che $f(n)$ e' **dello stesso ordine** di $g(n)$ se e solo se **esistono due costanti** $c_1, c_2 > 0$  tali che, per qualche $n_0$, **per tutti gli** $n ≥ n_0$ e' il caso che:
$$ 0 ≤ c_1 · g(n) ≤ f (n) ≤ c2 · g(n) $$
In altre parole l'insieme $Θ(g(n))$ contiene tutte e solo quelle funzioni $f(n)$ tali che, a partire da un certo momento in poi (per tutti gli $n ≥ n_0$), il valore di $f(n)$ e' descrittivo al valore $g(n)$ a meno di una costante. Quindi, asintoticamente, $f(n)$ si comporta come $g(n)$.

Chiaramente:
$$ f(n) = Θ(g(n)) $$
se e solo se:
$$ f (n) = O(g(n)) \quad e  \quad f(n) = \Omega(g(n)) $$
![[NotazioneAsintoticaGrafici.png]]

Per confrontare gli ordini di grandezza di funzioni diverse, usiamo la notazione $o()$ e $ω()$ (**o piccolo** e **omega piccolo**).
Diremo quindi che $f(n) = o(g(n))$ (**e' di un ordine inferiore a**) se e solo se per ogni costante $c > 0$, esiste una costante $n_0$ tale che, per ogni $n > n_0$ si verifica che $0 ≤ f (n) ≤ c · g(n)$ . Questo si puo' vedere in maniera piu' semplice: $$lim_{n→∞} \frac{f(n)}{g(n)} = 0$$
Analogamente, $f(n) = ω(g(n))$ (**e' di un ordine superiore a**) se e solo se per ogni costante $c > 0$, esiste una costante $n_0$ tale che, per ogni $n > n_0$ si verifica che $0 ≤ c · g(n) ≤ f (n)$. Piu' semplicemente: $lim_{n \rightarrow \infty} \frac{f(n)}{g(n)} = \infty$.  