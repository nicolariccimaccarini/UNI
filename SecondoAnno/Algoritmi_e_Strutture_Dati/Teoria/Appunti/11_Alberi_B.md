## Alberi B: introduzione
Un albero B (BT) generalizza un albero RB con fini differenti, ma mantenendo la sua proprieta' fondamentale di bilanciamento. Un albero B e' sempre completo.
Nel modello piu' semplice e diffuso di computazione, dobbiamo distinguere tra memoria **principale** e **secondaria**. La memoria secondaria e' vari ordini di grandezza piu' capace, ma e' piu' lenta della memoria primaria.
Gli alberi B sono una struttura dati ottimizzata per minimizzare gli accessi al disco; la complessita' delle operazioni si da lungo le direttive, tempo di CPU e numero di accessi al disco. La loro principale applicazione e' nelle basi di dati.
Gli alberi B sono una ==struttura dinamica, basata sull'ordinamento e memorizzata in maniera sparsa==.

> Alberi B $\rightarrow$ BT (Balanced Tree)

Un **albero B (BT)** si caratterizza per possedere una varieta' (che in questo contesto si conosce come **branching factor** (t)) superiore a 2, spesso dell'ordine di migliaia, un'altezza proporzionale a un algoritmo a base molto alta di $n$, dove $n$ e' il numero di chiavi, per avere nodi che contengono molte chiavi, tra loro ordinate, e per crescere verso l'alto, non verso il basso: un nodo comincia con essere la radice, e poi si converte in nodo interno generando una nuova radice.
La complessita' delle operazioni e' proporzionale all'altezza: quindi i BT possono essere usati come degli RBT.

Un nodo $x$ in un BT si caratterizza per avere comunque il puntatore al nodo padre (`x.p`), ma gli altri dati sono diversi da nodi degli alberi binari visti fino ad ora. Infatti, abbiamo:
- numero delle chiavi memorizzate nel nodo (`x.n`)
- l'informazione sull'essere, o meno, una foglia (`x.leaf` = 1 se x e' foglia)
- i puntatori agli $n+1$ figli di $x$ (`x.c_1, ..., x.c_x.n+1`) definiti se $x$ e' foglia
- $n$ chiavi (`x.key_1, ..., x.key_x.n`) invece di una
Il sotto-albero puntato da `x.c_i` e' legato alle chiavi `x.key_i-1` e `x.key_i`.

In un nodo $x$ il numero di chiavi, e quindi il branching factor, e' vincolato da un parametro che si chiama **grado minimo**, si denota con $t$ ed e' sempre $\ge 2$.
Le proprieta' di un albero B sono:
1. Ogni nodo, tranne la radice, ha almeno $t-1$ chiavi;
2. Ogni nodo puo' contenere al massimo $2 \cdot t-1$ chiavi;
3. Per ogni nodo $x$, `x.key_1 <= x.key_2 <= ... <= x.key_x.n`;
4. Per ogni nodo $x$, se un nodo $y$ contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono minori o uguali a `x.key_i`
5. Per ogni nodo $x$ , se un nodo $y$ e; contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono maggiori di `x.key_i-1`

Esempio di albero B:
![[BT_esempio.png]]
E' immediato osservare che per $t=2$, si ottiene un albero che e' sempre **isomorfo** a un albero red-black. Questi particolari alberi B sono noti come **alberi 2-3-4**.

Le prime due regole implicano che ogni nodo interno, tranne la radice, deve avere almeno $t$ figli. Se l'albero non e' vuoto, allora la radice ha almeno una chiave; un nodo interno puo' avere fino a $2 \cdot t$ figli: in questo caso lo chiamiamo **nodo pieno**.
Come conseguenza delle regole, l'altezza massima di un BT $T$ con $n$ chiavi e grado minimo $t \ge 2$ e': 
$$ h \le log_t(\frac{n+1}{2}) = O(log_t(n)) $$
e tutte le foglie sono alla stessa altezza. Dunque un albero B e' sempre completo.

## Alberi B: caratteristiche e altezza
Per dimostrare la proprieta' sull'altezza logaritmica, osserviamo che nel caso peggiore la radice di $T$ contiene una sola chiave e tutti gli altri almeno $t-1$. Percio', $T$ di altezza $h$ ha almeno due nodi ad altezza 1, almeno $2 \cdot t$ di altezza 3, almeno $2 \cdot t^2$ ad altezza 3, e cosi via, fino a $2 \cdot t^{h-1}$ ad altezza $h$. Percio' il numero di chiavi e':
$$ 1+2 \cdot (t-1) + 2 \cdot t \cdot (t-1) + 2 \cdot t^2 \cdot (t-1) + \space ... \space + 2 \cdot t^{h-1}(t-1) $$
Abbiamo quindi la seguente disuguaglianza:
$$
\begin{align}
n &\ge 1 + (t-1) \cdot \sum^h_{i-1} 2 \cdot t^{i-1} \qquad &&\text{risultato precedente} \\
n &= 1+2 \cdot (t-1) \cdot \frac{t^h-1}{t-1} &&\text{calcolo algebrico} \\
n &= 2 \cdot t^h -1 &&\text{calcolo algebrico, che implica} \\
t^h &\le \frac{n+1}{2} &&\text{calcolo algebrico} \\
h &\le log_t \Big(\frac{n+1}{2}\Big) &&\text{proprieta' dei logaritmi} \\
\end{align}
$$

Questo risultato puo' essere accoppiato con uno simile per stabilire il **massimo** numero di chiavi per ogni determinata altezza. Per farlo, costruiamo una tabella simile a quella fatta nel caso delle **heap**, per ottenere un risultato che adesso diventa abbastanza intuitivo: in un BT, si ha sempre che $h = \Theta(log_t(n))$. 
Facciamo delle osservazioni sul modo che abbiamo usato per calcolare l'altezza massima di un albero in 4 casi diversi:
- nel caso delle **heap**, abbiamo usato la proprieta' di essere alberi binari quasi completi, osservando che un certo sotto-albero deve contener almeno tanti nodi interni;
- nel caso dei **BST**, abbiamo osservato che non ci sono limiti minimi al numero di figli di un nodo, potendo quindi l'albero degenerare in una lista;
- nel caso dei **RBT** abbiamo utilizzato l'altezza nera ed il fatto che questa e' la stessa per tutti i rami.

## Alberi B: operazioni
Descriviamo adesso le operazioni di ricerca, creazione e inserimenti di una chiave. 
Convenzioni: 
- usiamo esplicitamente le operazioni `DiskRead` e `DiskWrite` per tenere conto degli accessori al disco; 
- diciamo che la radice `T.root` e' sempre in memoria principale
- rispetto a un nodo passato come parametro, assumiamo che sia gia' stata eseguita `DiskRead` su di esso.
Nel caso degli alberi B, calcoliamo la complessita' **sia in termini di uso della CPU sia in termini di numero di accessi al disco**. Infatti questi due aspetti sono separati e possiamo avere procedure che migliorano solo uno di essi, o entrambi.

## Alberi B: ricerca
L'operazione di `BTreeSearch` e' la generalizzazione della ricerca su BST. Dobbiamo prendere una decisione tra molte possibilita' per ogni nodo esplorato: invece di scegliere tra due figli, scegliamo tra `x.n + 1` possibili figli. `BTreeSearch` prende in input un puntatore $x$ ad un nodo di $T$, ed una chiave da cercare, e ritorna un puntatore ad un nodo $y$ piu' un indice $i$ (nel nodo) nei casi positivi, o **nil** se la chiave cercata non esiste nel sotto-albero radicato in $x$.
``` Pseudocodice
proc BTreeSearch(x, k) {
	i = 1
	while ((i <= x.n) and (k > x.key_i)) i = i+1
	if ((i <= x.n) and (k = x.key_i))
		then return (x, i)
	if (x.leaf = True)
		then return nil
	DiskRead(x.c_i)
	return BTreeSearch(x.c_i, k)
}
```

### Complessità di `BTreeSearch` 
La **correttezza** di questa procedura e' immediata. 
Per quanto riguarda la **complessità**, e' facile vedere che si operano, al massimo, $O(h) = O(log_t(n))$ accessi al disco. Inoltre, se utilizziamo la ricerca lineare sul nodo, otteniamo che il tempo totale di CPU nel caso peggiore e' $\Theta(t \cdot h) = \Theta(t \cdot log_t(n))$. La forza dei BT non si apprezza nella notazione $O()$. 

## Alberi B: inserimento
Prima di poter fare inserimento di una chiave, dobbiamo assicurare che l'albero esista.
A questo scopo, utilizziamo `BTreeCreate`, dove assumiamo di poter chiamare una procedura `Allocate()` che si occupa di creare e occupare uno spazio sufficiente nella memoria secondaria. Assumiamo che questo prenda un tempo costante. Questa fase di creazione mette in evidenza il fato che BT cresce a partire dalle foglie e non dalla radice: il primo nodo creato e' infatti una foglia.
``` Pseudocodice
proc BTreeCreate(T) {
	x = Allocate()
	x.leaf = True
	x.n = 0
	DiskWrite(x)
	T.root = x
}
```

In sintesi, cerchiamo di riempire un nodo fino a quando diventa pieno; quando il nodo e' pieno, **dividiamo** il nodo in questione (che ha $2 \cdot t-1$ chiavi) in due nodi di $t-1$ ciascuno, ed inseriamo la nuova chiave nel nodo padre: se il padre diventa pieno i seguito a tale inserimento, ripetiamo l'operazione un livello piu' in alto. Quindi $T$ cresce solamente quando la divisione ha luogo sulla radice: in questo caso si crea un nuovo nodo radice e si opera la divisione. 

> divisione di un nodo pieno non e' legata a inserire una chiave in quel nodo

``` Pseudocodice
proc BTreeSplitChild(x, i) {
	z = Allocate()
	y = x.c_i
	z.leaf = y.leaf
	for j=1 to t-1  z.key_j = y.key_j+t
	if (y.leaf = False)
		then for j=1 to t  z.c_j = y.c_j+t
	y.n = t-1
	for j = x.n + 1 downto i+1  x.cj+1 = x.cj
	x.ci+1 = z
	for j=x.n downto i  x.keyj+1 = x.keyj
	x.keyi = y.keyt
	x.n = x.n + 1
	DiskWrite(y)
	DiskWrite(z)
	DiskWrite(x)
}
```

La procedura assume che $x$ sia un nodo interno non pieno gia' nella memoria principale e che il nodo figlio `x.c_i`, anche lui gia' nella memoria principale, sia pieno. Il nodo `x.c_i` e' diviso in due nodi, ognuno con la meta' delle chiavi ($2 \cdot t-1$ e' sempre dispari!).

![[BT_inserimento.png]]
Nell'albero in figura a sinistra, l'esecuzione di `BTreeSplitChild(x, 2)` dove `x` e' il nodo piu' in alto provoca che il risultato sia quello a destra.

Il numero di operazioni CPU di `BTreeSplitChild` e' $\Theta(t)$, mentre il costo in termini di operazioni su disco e' $\Theta(1)$. 
La procedura `BTreeInsert` utilizza una seconda procedura (`BTreeInsertNonFull`) che si occupa, ricorsivamente, di inserire una chiave assumendo che il nodo considerato (ma non ancora scelto per l'inserimento) non sia pieno. Poiche' la chiave nuova va sempre inserita in una foglia, se quello considerato e' gia' una foglia, allora la chiave si inserisce semplicemente. Se invece non e' una foglia, allora scendendo ricorsivamente da un nodo non pieno, potremmo trovarci su un nodo pieno; in questo caso, eseguiamo la divisione e procediamo ricorsivamente.

Quindi la coppia `BTreeInsert` piu' `BTreeInsertNonFull` inserisce una nuova chiave e si occupa che tutti i nodi pieni che si trovano sul percorso che dalla radice arriva alla foglia corretta non siano pieni. Invece di inserire prima e aggiustare dopo, sistemiamo la situazione di tutti i nodi coinvolti mentre troviamo la posizione corretta per l'inserimento. In questo modo otteniamo una minimizzazione sia del numero di operazioni su disco sia del tempo di CPU.

``` Pseudocodice
proc BTreeInsert(T, k) {
	r = T.root
	if (r.n = 2*t-1) then 
		s = Allocate()
		T.root = s
		s.leaf = False
		s.n = 0
		s.c_1 = r
		BTreeSplitChild(s, 1)
		BTreeInsertNonFull(s, k)
	else 
		BTreeInsertNonFull(r, k)
}
```

Questo e' il momento in cui un albero B cresce di altezza. Quindi al chiamare di `BTreeInsertNonFull`, lo si fa o sulla radice originale che non era piena, o su quella nuova, che per costruzione non e' piena.

``` Pseudocodice
proc BTreeInsertNonFull(x, k) {
	i = x.n
	if (x.leaf = True) then 
		while ((i >= 1) and (k < x.key_i))
			x.key_i+1 = k.key_i
			i = i-1
		x.key_i+1 = k
		x.n = x.n + 1
		DiskWrite(x)
	else
		while ((i >= 1) and (k < x.key_i))  i = i-1
		i = i+1
		DiskRead(x.c_i)
		if (x.c_i.n = 2*t-1) then 
			BTreeSplitChild(x, i)
			if (k > x.key_i) then 
				i = i+1
		BTreeInsertNonFull(x.c_i, k)
}
```

La condizione di **non essere un nodo pieno** e' certamente vera all'entrata di `BTreeInsertNonFull`. Se $x$ e' un nodo foglia, e non e' pieno, ci limitiamo a scegliere la posizione della nuova chiave e a muovere le altre chiavi per mantenere l'ordine. I puntatori ai figli sono tutti **nil** e cosi' rimangono. Altrimenti, si va a cercare la posizione per la scelta del figlio di $x$ dove proseguire la ricerca della foglia corretta. 
Quando il figlio viene caricato in memoria principale possono succedere due cose:
- non e' pieno, in questo caso semplicemente si fa una chiamata ricorsiva, giacche' sono rispettate le condizioni;
- e' pieno, in quest caso, operiamo la divisione, scegliamo tra i due nodi nuovi qual'e' quello giusto e facciamo la chiamata ricorsiva.

## Correttezza e complessità di `BTreeInsert`
L'inserimento in BT e' una procedura tail-recursive (`BTreeInsertNonFull`). Quindi possiamo mostrare la **correttezza** usando un invariante.
Scegliamo: **all'inizio di ogni esecuzione di `BTreeInsertNonFull` $x$ e' non pieno, e $k$ va inserito nel sotto-albero radicato in $x$**.
L'invariante e' vera all'inizio (**caso base**), perche' la procedura viene chiamata sulla radice dell'albero. Supponiamo che sia vera all'inizio di una certa esecuzione di `BTreeInsertNonFull` (**caso induttivo**). Poiché stiamo assumendo che ci sarà una prossima esecuzione, non siamo nel caso base della ricorsione, quindi entriamo nel ciclo **while**. Si trova il posto corretto per $k$, e poiché $x$ non è una foglia, carica il giusto figlio. Poiché $x$ non è pieno per ipotesi, se il nodo caricato fosse pieno potrebbe essere eseguito lo split, rendendolo non pieno. Questo è il nodo su cui poi verrà richiamato `BTreeInsertNonFull`, e quindi l'inviariante è ancora vera. Nel caso in cui $x$ fosse una foglia, l'invariante dice che non è piena e che $k$ va inserito esattamente in $x$.
La **complessità** in numero di operazioni su disco e' chiaramente $\Theta(h)$; invece, quella in termini di operazioni CPU e' $\Theta(h \cdot t) = \Theta(t \cdot log_t(n))$.
