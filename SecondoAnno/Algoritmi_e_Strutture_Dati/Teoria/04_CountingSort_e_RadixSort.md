## Limiti per l'ordinamento basato su confronti
Qual'e' la complessita' minima del problema dell'ordinamento? Per rispondere, possiamo focalizzarci su un tipo specifico di operazione elementare; se riusciamo a dire **questo e' il minimo numero di operazioni di quel tipo**, allora abbiamo un limite inferiore per tutte le operazioni.

Un algoritmo di ordinamento si **basa sui confronti** se ogni passo puo' essere visto come un'operazione di confronto tra due elementi, seguita da uno spostamento. Gli algoritmi visti fino ad ora sono basati sul confronto.
Possiamo **generalizzare** il processo di ordinare per confronti?
Assumendo che possiamo solo confrontare 2 elementi per volta, il processo di ordinare puo' essere visto come un albero binario la cui radice e' l'inpiut. Ad ogni nodo si associa la **permutazione** dell'oggetto che corrisponde ad uno scambio.

Quante permutazioni possibili con *n* elementi? Precisamente $n!$, e un albero binario con $n!$ foglie e' alto, almeno, $log(n!)$. Quindi $log(n!)$ e' un limite inferiore alla lunghezza massima di un ramo nell'albero che rappresenta l'esecuzione di un qualsiasi algoritmo di ordinamento basato sui confronti. Sappiamo che:
$$
log(n!) = \Theta(n \cdot log(n))
$$
Quindi il nostro limite inferiore e' $\Omega(n \cdot log(n))$. Per esempio, questo significa che `MergeSort()` e' **ottimo**, visto che ha complessita', nel caso peggiore, $\Theta(n \cdot log(n))$. `QuickSort()` non e' ottimo, neppure nella sua versione randomizzata, ma altre considerazioni che abbiamo fatto ci portano a preferirlo in tate situazione.
Per numeri sufficientemente **piccoli**, possiamo fare meglio di $n \cdot log(n)$, In realta', il limite che **non possiamo** vincere e' $log(n!)$.

E' possibile ordinare correttamente 5 numeri basandoci sui confronti in meno di 8 confronti? Se rispondiamo di no, allora sbagliamo, anche se e' vero che $5 \cdot log(5) = 11.6 ≈ 12$. La risposta e' si, perche' $log(5!) = log(120) = 6,90 ≈ 7$, che e' il numero minimo di confronti necessari per decidere l'ordine di 5 elementi diversi. In linea di principio ne esiste uno specifico per qualunque numero **fissato** di elementi: questi algoritmi non sono eleganti, non insegnano nulla di concettuale e servono solo esclusivamente per rappresentare un'idea.

Oltre ad esistere un numero minimo di confronti che qualunque algoritmo deve fare per poter risolvere il problema dell'ordinamento, esiste un numero minimo di operazioni, e quindi di complessita' minima.

## Ipotesi aggiuntive
Aggiungendo delle ipotesi agli oggetti che vogliamo ordinare possiamo vincere dei limiti. Un esempio che vediamo sono gli algoritmi `CountingSort()` e `RadixSort()`.
Entrambi gli algoritmi sono basati sull'ipotesi aggiuntiva di stare ordinando dei numeri interi du cui conosciamo il limite superiore. `CountingSort()` lavora direttamente coi numeri e `RadixSort()` ne generalizza l'uso.
`CountingSort()` si basa su un array $A$, su un appoggio di $C$ (quindi ==non e' in place==) e su uno di uscita B, dove si ottiene il risultato.

## *CountingSort*
``` Pseudocodice
proc CountingSort(A, B, k) {
	let C[0, ..., k] new array
	for (i=0 to k) C[i] = 0    // azzera dei contatori
	for (j=1 to A.length) C[A[j]] = C[A[j]] + 1   // inizializza i contatori
	for (j=1 to k) C[i] = C[i] + C[i-1]    /* calcola in C il numero di volte in
											  cui un numero piu' piccolo o uguale
											  a 'i' compare in A */  
	for (j=A.lenght downto 1) {
		B[C[A[j]]] = A[j]
		C[A[j]] = C[A[j]] - 1
	}
}
```

![[CountingSort.png]]

### Correttezza di *CountingSort*
Per la **terminazione**, vediamo che tutto l'algoritmo e' governato da cicli **for**, e quindi termina per definizione.
Guardiamo la **correttezza**. Alla fine del secondo ciclo **for**, in C la posizione i-esima contiene il numero di elementi di $A$ che sono uguali ad $i$, e alla fine del terzo, la posizione i-esima contiene il numero di elementi di $A$ che sono uguali ad $i$. Un'**invariante** corretta per il quarto ciclo **for** e': al j-esimo passo, $C[A[j]]$ e' la posizione corretta di $A[j]$ in B. In primo luogo osserviamo che, prima di iniziare il quarto ciclo, per ogni *j* si ha che $C[A[j]] - z$ e' la posizione corretta di $A[j]$ in B se $A[j]$ e' tale che $|{A[I]|I > j, A[I] = A[j]}| = z$. 

Adesso dimostriamo il caso generale.
- **Caso base**. L'invariante e' chiaramente vera per $j=n$, per l'osservazione precedente.
- **Caso induttivo**. Supponiamo che l'invariante sia vera per un certo *j*. Dopo aver effettuato l'inseriemnto di $A[j]$ nella posizione $C[A[j]]$, quest'ultimo valore diminuisce di un'unita'. Consideriamo adesso l'elemento $A[j-1]$. Se questo e' diverso da tutti gli elementi $A[I]$ con $I \ge j$, allora per l'osservazione precedente l'invariante e' ancora vera. Supponiamo invece che esistano *p* elementi del tipo $A[I]$, con $I \ge j$, tali che $A[I] = A[j-1]$. Nei passaggi precedenti, il valore $C[A[j-1]]$ e' dunque diminuito di *p* unita'. Questo significa che adesso il valore di $C[A[j-1]]$ e' quello della posizione corretta di $A[j-1]$ in B.

La correttezza di questa invariante implica direttamente che l'ultimo ciclo dell'algoritmo ordina $A$ in $B$, come richiesto.

### Complessita' di *CountingSort*
La **complessità** di `CountingSort()` e' data dai quattro cicli. Due di questi hanno complessita' $\Theta(n)$ e gli altri due hanno complessita' $\Theta(k)$. Pertanto se $k = O(n)$ la complessita' totale e' $\Theta(n)$. Ma essendo precisi, la complessita' andrebbe scritta come $\Theta(n+k)$. Quindi se volessimo usare `CountingSort()`su un'array qualsiasi che contiene interi, di cui non conosciamo il massimo, l'idea di fare una passata iniziale per computare il massimo, e dichiarare $C$ in maniera dinamica, e' rischiosa: se *k* e' troppo grande, l'allocazione potrebbe fallire, e comunque, se $k>>n$, la complessita' non e' piu' lineare.

## L'algoritmo *RadixSort* per ordinare elementi multi-indice
Immaginiamo di voler ordinare $n$ elementi multi-indice. Un esempio potrebbe essere quello di ordinare $n$ date (giorno-mese-anno). 
Un algoritmo classico che si puo' utilizzare per risolvere questo problema prevede di ordinare gli elementi rispetto all'indice piu' significativo, per poi passare al secondo, e cosi' via. Nell'esempio, avremmo le date ordinate per anno, mese ed infine giorno. Il difetto principale di questa strategia e' che dopo la prima passata di ordinamento dobbiamo separare gli elementi per gruppi. Quando disponiamo di un algoritmo stabile come `CountingSort()` possiamo usare una strategia contro-intuitiva: ordinare prima secondo gli indici meno significativi.
`RadixSort()` **non** e' un algoritmo a se' stante, ma un **meta-algoritmo**: si basa sun algoritmo interno di ordinamento, per esempio *CountingSort*.

``` Pseudocodice
proc RaidxSort(A, d) {
	for (i=1 to d) AnyStableSort(A) on digit i
}
```

![[RadixSort.png]]

### Correttezza di *RadixSort*
La **terminazione** e' completamente ovvia, assumendo che la procedura interna termini. Per la **correttezza**, l'**invariante** e': dopo la i-esima esecuzione del ciclo piu' interno, gli elementi formati dalle ultime *i* colonne sono correttamente ordinati.

Mostriamo che l'invariante funziona.
- **Caso base.** Se $i=1$, allora stiamo parlando di elementi fatti da un solo indice. Quindi, la correttezza segue dalla correttezza dell'algoritmo stabile utilizzato come sotto-procedura.
- **Caso induttivo.** Assumiamo allora che $i > 1$, e che l'invariante valga per $i-1$. Consideriamo l'indicei-esimo. Dopo la i-esima esecuzione del ciclo **for**, gli elementi sono lessicograficamente ordinati sull'indice i-esimo (il piu' significativo). Consideriamo due elementi a, b che hanno lo stesso valore per l'indice i-esimo, ma valori diversi sull'indice ($i-1$)-esimo. Per ipotesi induttiva, dopo la ($i-1$)-esima esecuzione *a* viene posto prima di *b*; poiche' la sotto-procedura e' stabile, questa relazione viene mantenuta dopo la i-esima esecuzione, implicando che vale ancora $a<b$ dopo la i-esima esecuzione.

### Complessita' di *RadixSort*
Tipicamente, ogni indice considerato da solo e' numerico con massimo *k* costante (per esempio, nelle date i giorni vanno da 1 a 31). Quindi, tipicamente, si usa `CountingSort()` come procedura interna. Pertanto, la complessità di `RadixSort()` per *n* elementi su *d* indici, essendo ogni indice limitato tra 0 e *k*, e' $\Theta(d \cdot (n+k))$. In generale, la complessita' del caso pessimo e' $O(d \cdot f(n))$, dove $O(f(n))$ e' la complessita' nel caso pessimo della procedura interna.
