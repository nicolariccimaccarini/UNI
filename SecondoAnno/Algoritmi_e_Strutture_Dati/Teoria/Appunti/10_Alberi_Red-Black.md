## Albero red-black: introduzione
Un **albero red-black** (RBT) e' un albero binario di ricerca (BST) **bilanciato** per costruzione. Possiede tutte le caratteristiche di un BST, ma la sua altezza e' sempre $\Theta(log(n))$, dove $n$ e' il numero di elementi di un albero.
Un RBT e' una ==struttura dati dinamica, basata sull'ordinamento e sparsa==. Tutte le operazioni, ed in particolare quelle di ricerca, che funzionano in un tempo proporzionale all'altezza diventano esponenzialmente piu' efficienti su un RBT.
La caratteristica principale di queste strutture e' che l'inserimento e l'eliminazione mantengono la proprieta' di bilanciamento.

Ogni nodo in un RBT ha un'informazione in piu' rispetto a un nodo di BST: oltre a un puntatore al padre, i puntatori ai due figli e la chiave, abbiamo il **colore** (`x.color`), che per convenzione e' rosso o nero.
Ogni foglia possiede due figli **virtuali**, che non contengono chiave e sono sempre di colore nero. 
Per convenzione, il padre della radice e' anche lui un nodo virtuale senza chiave, senza figli e di colore nero.

Le regole che ogni RBT deve rispettare sono:
1. Ogni nodo e' rosso o nero;
2. La radice e' nera;
3. Ogni foglia (esterna, **nil**) e' nera;
4. Se un nodo e' rosso, entrambi i suoi figli sono neri;
5. Per ogni nodo, tutti i percorsi semplici da lui alle sue foglie, contengono lo steso numero di nodi neri.

Chiameremo i nodi di un albero RB **interni**, per distinguerli dai nodi **esterni** che aggiungiamo in maniera artificiale a ogni albero RB. Una foglia esterna e' un nodo che ha tutte le proprieta' di ogni altro nodo ma non porta alcuna chiave, ed e' sempre di colore nero.
Ogni nodo interno di un RBT ha sempre due figli, a differenza di quello esterno che non ha figli.
Dal punto di vista implementativo, definiamo una sentinella `T.nil` come un nodo con tutte le proprieta' di un nodo $T$ e colore fissato a nero per il ruolo di foglia esterna.

![[RBT_introduzione.png]]

Il principio fondamentale degli RBT e' che le proprieta' sono valide quando l'albero e' vuoto e vengono mantenute tali dopo ogni inserimento e eliminazione. Dobbiamo ancora dimostrare che esse garantiscono il bilanciamento dell'albero - a meno di una costante. 
Cominciamo definendo l'**altezza nera** (`bh(x)`) di un nodo $x$ in $T$ come il numero di nodi neri su qualsiasi percorso semplice da $x$ (senza contare $x$) a una foglia esterna (contandola).
L'altezza nera di $T$ e' `bh(T.root)`.

Dimostriamo che se $T$ e' un RBT con $n$ nodi interni (quindi escludendo le foglie esterne), allora la sua altezza massima e' $2 \cdot log(n+1)$. 
Mostriamo che il sotto-albero radicato in $x$ contiene almeno $2^{bh(x)}-1$ nodi interi, per induzione.
Quando `bh(x)` e' 0, allora per definizione `x = T.Nil`, e il sotto-albero indicato in $x$ non ha nodi interni; l'alteza nera di $x$ e' 0 (perche' non si include il nodo stesso), ed abbiamo che $2^{bh(x)}-1 = 1-1 = 0$, come volevamo. Se `bh(x)` e' positiva, allora l'altezza nera di entrambi i suoi figli e' almeno `bh(x) - 1`. Per ipotesi induttiva ognuno dei due sotto-alberi ha almeno $2^{bh(x)-1}-1$ nodi interni. Quindi il sotto-albero radicato in $x$ ha almeno $2 \cdot (2^{bh(x)-1}-1)+1$ nodi interni, che e' esattamente $2^{bh(x)}-1$.

Consideriamo adesso $T$ di altezza $h$. Per la proprieta' 4, almeno la meta' dei nodi della radice (esclusa) ad una foglia di qualsiasi ramo e' nera. Quindi $\text{bh(T.root)} \ge \frac{h}{2}$. Dalla proprieta' precedente, il numero $n$ di nodi in $T$ e' $n \ge 2^{\text{bh(T.root)}}-1$, cioe' $n \ge 2^{\frac{h}{2}}-1$.
Quindi: 
$$
\begin{align}
n &\ge 2^{\frac{h}{2}}-1 \qquad &&\text{risultato precedente} \\
n+1 &\ge 2^{\frac{h}{2}} &&\text{calcolo algebrico} \\
log(n+1) &\ge \frac{h}{2} &&\text{proprieta' logaritmi} \\
h &\le 2 \cdot log(n+1) &&\text{tesi}
\end{align}
$$
Un albero binario completo ha altezza $h$ sempre maggiore o uguale a $log(n)-1$, dove $n$ e' il numero di nodi totali.
Pertanto, $log(n)-1 \le h \le 2 \cdot log(n+1)$, cioe' $h = \Theta(log(n))$.

## Alberi red-black: rotazioni
Inserimento ed eliminazioni in un RBT possono violare le proprieta' e che la maggiore difficolta' nell'implementare queste procedure consiste precisamente nel modificare la struttura dell'albero per ripristinare queste proprieta'.
Un passo intermedio fondamentale per questa riparazione e' la **rotazione**, che puo' essere destra o sinistra e che preserva le proprieta' BST. L'idea e' che possiamo ribilanciare l'albero e poi preoccuparci dei colori.
Risolviamo il problema di **rotazione sinistra**: dato un RBT $T$ ed un nodo $x$ in $T$ con figlio destro $y$, ottenere un nuovo albero $T'$, dove $y$ ha come figlio sinistro $x$.
Simmetricamente, potremmo definire il problema della rotazione destra. In entrambi i casi la complessita' e' $\Theta(1)$.

``` Pseudocodice
proc BSTTreeLeftRotate(T, x) {
	y = x.right
	x.right = y.left
	if (y.left != T.Nil)
		then y.left.p = x
	y.p = x.p
	if (x.p = T.Nil)
		then T.root = y
	if ((x.p != T.Nil) and (x = x.p.left))
		then x.p.left = y
	if ((x.p != T.Nil) and (x = x.p.right))
		then x.p.right = y
	y.left = x
	x.p = y
}
```

Una volta capito come funzionano i cambi di puntatori, mostrare la **correttezza** delle rotazioni e' immediato. Inoltre, si vede subito che la complessita' e' costante in entrambi i casi destro e sinistro.

## Alberi red-black: inserimento
Risolviamo il problema di inserire un nodo $z$ in un RBT $T$ in maniera da mantenere tutte le proprieta' di $T$. Chiaramente usiamo `BSTTreeInsert` cosi' com'e', abbiamo la garanzia che la proprieta' BST sia rispettata. Se il nodo inserito e' colorato di rosso, allora anche la proprieta' 5 e' rispettata; inoltre, poiche' $z$ sara' sempre una nuova foglia, inserendo correttamente le sue foglie esterne, garantiamo anche la proprieta' 3. La proprieta' 1 e' rispettata semplicemente assegnando il colore (rosso) a $z$. Quindi, solo due proprieta' possono essere violate: se $z$ diventa la radice, allora **violiamo 2**, se invece $z$ diventa figlio di un nodo rosso, **violiamo 4**.

``` Pseudocodice
proc RBTreeInsert(T, z) {
	y = T.Nil
	x = T.root
	while (x != T.Nil)
		y = x
		if (z.key < x.key)
			then x = x.left
		else x = x.right
	z.p = y
	if (y = T.Nil)
		then T.root = z
	if ((y != T.Nil) and (z.key < y.key))
		then y.left = z
	if ((y != T.Nil) and (z.key >= y.key))
		then y.right = z
	z.left = T.Nil
	z.right = T.Nil
	z.color = RED
	RBTreeInsertFixup(T, z)
}
```

Nell'albero di esempio, inseriamo $z$ con chiave 27, ottenendo una violazione della proprieta' 4:
![[RBT_inserimento.png]]

``` Pseudocodice
proc RBTreeInsertFixup(T, z) {
	while (z.p.color = RED)
		if (z.p = z.p.p.left)
			then RBTreeInsertFixUpLeft(T, z)
			else RBTreeInsertFixUpRight(T, z)
		T.root.color = BLACK
}

proc RBTreeInsertFixUpLeft(T, z) {
	y = z.p.p.right
	if (y.color = RED) then
		z.p.color = BLACK
		y.color = BLACK
		z.p.p.color = RED
		z = z.p.p
	else 
		if (z = z.p.right) then 
			z = z.p
			TreeLeftRotate(T, z)
		z.p.color = BLACK
		z.p.p.color = RED
		TreeRightrRotate(T, z.p.p)
}

proc RBTreeInsertFixUpRight(T, z) {
	y = z.p.p.left
	if (y.color = RED) then
		z.p.color = BLACK
		y.color = BLACK
		z.p.p.color = RED
		z = z.p.p
	else 
		if (z = z.p.left) then 
			z = z.p
			TreeRightRotate(T, z)
		z.p.color = BLACK
		z.p.p.color = RED
		TreeLeftRotate(T, z.p.p)
}
```

## Alberi red-black: correttezza dell'inserimento
La scelta che si fa all'inizio di `RBTreeInsertFixup` genera due casi, che dipendono dal fatto che `z.p` sia figlio destro o sinistro di `z.p.p`.
All'interno di ogni caso vi sono tre sotto-casi, che si distinguono dal colore di $y$ (lo **zio** di $z$):
- se e' rosso e' un caso
- se e' nero:
	- se $z$ e' figlio destro e' un secondo caso
	- se $z$ e' figlio sinistro e' un terzo caso

Il totale e' quindi di 6 casi, i primi tre completamente simmetrici ai secondi tre. Osserviamo che se $z$ e' la radice (abbiamo inserito un nodo in un albero vuoto), allora `z.p = T.Nil` e  `T.Nil.color = BLACK`: quindi la condizione del ciclo **while** e' corretta e determina un corretto caso di terminazione. Similmente, se $z$ e' un figlio diretto della radice, allora `z.p` e' la radice, e quindi `z.p.p = T.Nil`, pertanto `z.p.p.left` e `z.p.p.right` sono entrambi `Nil` e diversi da `z.p`: quindi tutte le condizioni **if** sono ben definite.

Analizziamo il codice. L'idea di fondo e': se esiste un problema dopo l'inserimento (violazione della proprieta' 2 o della proprieta' 4), questo si **spinge verso l'alto** con il caso 1. Quando non e' piu' possibile, si salta al caso 2 (immediatamente convertito al caso 3) o al caso 3: una rotazione risolve il problema in forma definitiva e garantisce l'uscita dal ciclo (**terminazione**). 
Per mostrare la **correttezza**, usiamo la seguente **invariante**: $z$ e' rosso, se `z.p` e' al radice, allora e' nera, e se $T$ viola qualsiasi proprieta', allora ne viola esattamente una, che e' la 2 o la 4.
La condizione di uscita (nei tre casi) e' che `z.p` e' di colore nero; quindi l'invariante sommata alla condizione di uscita piu' l'ultima istruzione di `RBTreeInsertFixup` ci da la correttezza.

Sappiamo che $T$ e' un RBT legale prima di chiamare `RBTreeInsertFixup`. Per quanto riguarda l'**inizializzazione**, dobbiamo mostrare che l'invariante e' vera prima di chiamare `RBTreeInsertFixup` Osserviamo, prima di tutto, che $z$ viene inserito rosso. Inoltre, se `z.p` e' la radice, allora `z.p` era nera e prima di chiamare `RBTreeInsertFixup` questo non e' cambiato. Infine, sappiamo che le proprieta' 1, 3 e 5 non sono violate alla chiamata di `RBTreeInsertFixup`. Se $T$ viola 2, deve essere perche' $z$ e' la radice (e $T$ era vuoto prima dell'inserimento); in questo caso `z.p = z.left = z.right = T.Nil` sono tutti nodi neri, percio' la proprieta' 4 non e' violata e la violazione della 2 e' l'unica. Se invece $T$ viola 4, poiché `z.left = z.right = T.Nil` sono neri, e il resto di $T$ non ha violazioni, deve essere perché `z.p` è rosso come $z$.

Cosa accade dopo la fine dell'inserimento?
- Al termine della procedura`z.p` e' nero. 
- La proprieta' 4 e' rispettata al termine.
- Se al termine del ciclo la proprieta' 2 e' violata, l'ultima linea di codice la ripristina.

Ci rimane da dimostrare che l'invariante e' mantenuta da un ciclo al seguente. Dei sei casi da analizzare ne analizziamo solo tre, assumendo che `z.p` e' figlio sinistro di `z.p.p`. Stiamo quindi assumendo che si esegue `RBTreeInsertFixUpLeft`.

- **Caso 1**: lo zio $y$ di $z$ e' rosso. Poiche' `z.p.p` e' nero, coloriamo di nero sia `z.p` che $y$ e coloriamo di rosso `z.p.p`, per mantenere la proprieta' 5. Adesso `z.p.p` diventa $z$ (quindi spostiamo il potenziale problema un passo piu' in alto). Dobbiamo mostrare che il nuovo $z$ e' tale che l'invariante e' mantenuta. Prima di tutto, $z$ e' rosso; poi `z.p` non cambia colore, quindi, se e' la radice, e' rimasta nera; infine, le proprieta' 1 e 3 non sono a rischio e sappiamo gia' che 5 e' mantenuta: se $z$ e' la radice, allora e' rossa e si viola 2, giacche' `z.p = T.Nil` e' nero, se il nuovo `z` non e' la radice allora solo 4 puo' essere ancora violata e grazie alle altre tre ipotesi ed alla correzione nel ciclo seguito, questa violazione e' dovuta a che `z.p` e' rosso.
  
- **Caso 3**: lo zio di $y$ e' nero e $z$ e' figlio sinistro di suo padre. Il caso 2 ($z$ e' figlio destro di suo padre) si riporta immediatamente al caso 3 attraverso una rotazione ed un ricoloramento. Il nodo `z.p` diventa nero e il nodo `z.p.p` diventa rosso. La rotazione a destra su `z.p.p` ripristina la proprieta' 5. Ci rimane da mostrare che $z$  e' tale che l'invariante e' mantenuta: prima di tutto, $z$ e' rosso, poi, se `z.p` e' la radice, e' diventata nera; infine le proprieta' 1 e 3 non sono a rischio e sappiamo gia' che 5 e' mantenuta; inoltre in questo caso la proprieta' 2 non si puo' violare. L'unica violazione alla proprieta' 4 ($z$ e `z.p` entrambi rossi) viene corretta e non ci sono altre violazioni.

Concludendo l'analisi dell'inserimento (che ha **complessità**, nel caso peggiore, $\Theta(h) = \Theta(log(n))$: nel peggior caso si esegue tutto il ciclo while seguendo il caso 1 e si percorre un ramo intero), osserviamo che il caso 1 si verifica in un RBT previamente bilanciato e si sistemano i colori per mantenere, al peggio, un leggero sbilanciamento. I casi 2 e 3 invece operano su un RBT gia' leggermente sbilanciato: ma questo si rivela facile da sistemare grazie a, al massimo, due rotazioni e poi un ricoloramento sistematico. Questo complesso sistema ci permette di mantenere una struttura bilanciata anche quando si opera un inserimento di elementi in ordine, che invece genererebbe un BST molto sbilanciato. Come si puo' intuire, l'eliminazione di un nodo da un RBT e' **molto** complessa. Anche questa operazione ha costo $\Theta(h) = \Theta(log(n))$.

## Alberi binari di ricerca, red-black e liste: confronto


|               | Liste       | BST (c. medio)   | BST (c. peggiore) | RBT (c. peggiore) |
| ------------- | ----------- | ---------------- | ----------------- | ----------------- |
| Inserimento   | $\Theta(1)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Cancellazione | $\Theta(1)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Visita        | $\Theta(n)$ | $\Theta(n)$      | $\Theta(n)$       | $\Theta(n)$       |
| Ricerca       | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Successore    | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Predecessore  | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Massimo       | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Minimo        | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
