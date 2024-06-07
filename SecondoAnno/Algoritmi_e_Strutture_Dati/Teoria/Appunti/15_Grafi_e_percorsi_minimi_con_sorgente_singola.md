## Percorsi minimi con sorgente singola
Ci concertiamo su grafi diretti, pesati e connessi. Dato $G = (V, E, W)$ e dato un vertice $s$, ci proponiamo di trovare per ogni $v \in V$ il percorso di peso minimo che porta da $s$ a $v$. 
Questo problema, chiamato **percorsi minimi con sorgente singola** e' da considerarsi la generalizzazione di `BreadthFirstSearch`.

Anche nel caso dei grafi pesati possiamo definire il concetto di distanza minima tra i due vertici.
Immaginiamo di individuare un vertice $s$ e diciamo che $\delta(s, v)$ e' la **distanza piu' corta** tra $s$ e $v$ e la definiamo come segue.
Dato un certo percorso $p = v_0, v_1, \ldots, v_k$ (nella nostra direzione: $v_0 \rightsquigarrow v_k$), il suo **peso** e':
$$
w(p) = \sum^k_1 W(v_{i-1}, v_i)
$$ e poi diciamo che:
$$
\delta(s, v) = 
\begin{cases}
\min w(p) \quad & p = s, \ldots, v \\
\infty & \text{se v non si raggiunge da s}
\end{cases}
$$
La distanza piu' corta gode delle stesse proprieta' di quella definita nel caso non pesato (in particolare la disuguaglianza triangolare: se esiste l'arco $(u, v)$, allora $\delta(s, v) \le \delta(s, u) + W(u, v)$) e vale inoltre la sottostruttura ottima.

Un percorso di peso minimo da $s$ a $v$, denotato con $s = v_0, v_1, \ldots, v_k = v$ ha una **struttura ottima**.
Per ogni $i, j$, anche $v_1, \ldots, v_j$ e' un percorso minimo che va da $v_i$ a $v_j$ (per vederlo si ragiona per contraddizione sull'ipotesi che $v_0, \ldots, v_k$ fosse di peso minimo).
Dobbiamo distinguere la situazione in cui i pesi negativi rendono la definizione di percorso minimo priva di senso e quelle in cui il percorso minimo esiste ancora ma un particolare algoritmo non e' in grado di gestirli.
Se $G$ ha pesi negativi, ma non ci sono **cicli negativi** raggiungibili da $s$, allora i percorsi minimi da $s$ sono ancora ben definiti.
D'altra parte se da $s$ c'e' un ciclo negativo raggiungibile, allora non ha senso definire un percorso di peso minimo da $s$: ogni nuovo passaggio attraverso quel ciclo diminuirebbe il peso.
In questo caso diciamo che il peso del percorso da $s$ a $v, \space \delta(s, v)$ e' $-\infty$. Se $v$ semplicemente non e' raggiungibile da $s$, diremo che il peso minimo e' $\infty$.

Ma che ruolo hanno i cicli sui percorsi minimi?
- se $s \rightsquigarrow v$ e' un percorso minimo da $s$ a $v$ che contiene un ciclo positivo, allora eliminando il ciclo si ottiene un nuovo percorso minimo; pertanto $s \rightsquigarrow v$ non contiene un percorso minimo.
- se il ciclo fosse negativo, il peso del percorso minimo sarebbe $-\infty$.
- se il ciclo ha peso 0, allora eliminandolo si ottiene un nuovo percorso minimo dello stesso peso. Pertanto, indipendentemente dalla presenza di pesi negativi, possiamo sempre dire che un percorso minimo e' aciclico e quindi ci limitiamo a studiare percorsi minimi di lunghezza $|V|-1$ al massimo.

Interpretiamo l'output di un qualsiasi algoritmo che dati $G$ (diretto, pesato, che non contiene cicli negativi) ed $s \in G.V$ come un assegnamento del valore $v.\pi$ per ogni $v \in V \backslash\{s\}$. 
Cerchiamo l'**albero dei cammini di peso minimo** in $G$ (questa volta si tratta di un albero radicato diretto), non necessariamente unico e tale che ogni ramo rappresenta un cammino di peso minimo da $s$ al vertice in questione.
Gli algoritmi per il calcolo di questo albero utilizzano una tecnica chiamata **rilassamento**, che funziona sulla base di $v.\pi$ come di $v.d$, rappresentando il cammino di peso minimo **che si conosce fino ad un determinato momento** da $s$ a $v$.

Il valore $v.d$ deve essere visto pertanto come **una stima** del peso del cammino minimo da $s$ a $v$. Come tale delve essere inizializzata.
``` Pseudocodice
proc InizializeSingleSource(G, s) {
	for (v in G.V)
		v.d = infty
		v.pi = Nil
	s.d = 0
}
```

**Rilassare** un arco $(u, v)$ consiste nel testare se la stima su $v.d$ puo' essere migliorata grazie all'arco $(u, v)$.
In questo caso si aggiorna tanto $v.d$ come $v.\pi$.
Assumiamo quindi che $(u, v)$ esista e vediamo il codice di `Relax`.
Osserviamo che ogni algoritmo per il calcolo dell'albero dei cammini minimi che vediamo si basa sull'esecuzione come passo previo di `InizializeSingleSource` e sul fatto che ogni modifica ai pesi minimi dei cammini e la loro struttura e' fatta esclusivamente da `Relax`.
``` Pseudocodice
proc Relax(u, v, W) {
	if (v.d > u.d + W(u, v))
		v.d = u.d + W(u, v)
		v.pi = u
}
```

## Algoritmo di Bellman-Ford
L'algoritmo di Bellman-Ford lavora con pesi negativi e con cicli negativi: restituisce i cammini minimi ed i loro pesi se non ci sono cicli negativi raggiungibili dalla sorgente e **false** altrimenti.

``` Pseudocodice
proc Bellman-Ford(G, s) {
	InizializeSingleSource(G, s)
	for i=1 to |G.V| - 1
		for ((u, v) in G.E) Relax(u, v, W)
	for ((u, v) in G.E)
		if (v.d > u.d + W(u, v))
			return false
	return true
}
```

### Correttezza e complessità di Bellman-Ford 
La **complessità** di `Bellman-Ford()` e' $\Theta(|V| \cdot |E|)$, cioe' $O(|V|^3)$ ($\Theta(|V|^3)$ nel caso dei grafi densi).
La **terminazione** non presenta particolari difficolta'.
Dimostrazione **correttezza**:
- Sia $G$ un grafo diretto e pesato, $s$ uno dei suoi vertici e assumiamo che `Bellman-Ford()` sia stato eseguito su $G$ con sorgente $s$.
- Vogliamo dimostrare che $G$ non contiene cicli negativi, allora per ogni $v$ raggiungibile da $s$ si ha che $v.d = \delta(s, v)$, il grafo $G_\pi$ (che corrisponde ad un albero di radice $s$) e' precisamente l'albero dei cammini minimi e l'algoritmo ritorna **true**; se $G$ contiene un ciclo negativo raggiungibile da $s$ allora l'algoritmo ritorna **false**.

Assumiamo che $G$ non contenga cicli negativi raggiungibili dalla sorgente.
Per un determinato vertice $v$, succede che il cammino minimo $s = v_0, v_1, v_2, \ldots, v_k = v$
e' definito.
Mostriamo la seguente **invariante**: dopo $i$ esecuzioni del ciclo piu' esterno, $v_i.d = \delta(s, v_i)$. 
- Il **caso base** e' ovvio: dopo 0 iterazioni, $v_0 = s$ e' tale che $v_0.d = 0 = \delta(s, v_0)$ (conseguenza di `InizializeSingleSource()`)
- **Caso induttivo**: 
	- supponiamo che dopo $i-1$ iterazioni si abbia $v_{i-1}.d = \delta(s, v_{i-1})$.
	- poiche' nell'iterazione $i$-esima si rilassano **tutti gli archi**, tra questi si rilassera' anche l'arco $(v_{i-1}, v_i)$;
	- dopo questo rilassamento, chiaramente $v_i.d = \delta(s, v_i)$ (poiche' l'arco $(v_{i-1}, v_i)$ e' parte di un percorso minimo).
	- inoltre, in ogni rilassamento successivo di qualunque arco, compreso $(v_{i-1}, v_i)$, il valore di $v_i.d$ non puo' cambiare perche' e' gia' minimo.
	  
	- Supponiamo adesso che $G$ contenga un ciclo negativo raggiungibile dalla sorgente: $v_0, v_1, \ldots, v_k = v_0$, tale che: $$ \sum^k_{i=1} W(v_{i-1}, v_i) <0 $$
	- Il ragionamento precedente ci porta a concludere che, dopo il primo ciclo, per ogni vertice $v$ il valore di $v.d$ e' il peso del cammino minimo da $s$ a $v$ con non piu' di $|V|-1$ archi.
	- Se per assurdo, dopo il secondo ciclo `Bellman-Ford()` restituisse **true**, allora per $i = 1, 2, \ldots, k$ succederebbe che $v_i.d \le v_{i-1}.d + W(v_{i-1}, v_i)$.
	- Sommando: $$ \begin{align} 
	\sum^k_{i=1} v_i.d & \le \sum^k_{i=1}(v_{i-1} + W(v_{i-1}, v_i)) \qquad && \text{somma del ciclo} \\
	& = \sum^k_{i=1} v_{i-1}.d + \sum^k_{i=1} W(v_{i-1}, v_i) && \text{proprieta' di } \sum \\
	0 & \le \sum^k_{i=1} W(v_{i-1}, v_i)
	\end{align}
	$$
	- perche' $\sum^k_{i=1} v_{i-1}.d = \sum^k_{i=1} v_i.d$. $\rightarrow$ contraddice l'ipotesi assurda
	- $\Rightarrow$ `Bellman-Ford()` restituisce **false**.

## Percorsi minimi nei grafi aciclici
Nel caso particolare in cui possiamo assumere che $G$ sia aciclico, trovare i percorsi minimi da una sorgente $s$, anche in presenza di pesi negativi, e' piu' semplice. 
Un grafo aciclico puo' essere ordinato topologicamente:
- se $u$ precede $v$ in un percorso minimo $\rightarrow$ $u$ precede $v$ nell'ordine topologico.
Possiamo operare un ordinamento topologico prima e poi una serie di rilassamenti in ordine, risparmiando quindi molti passi di rilassamento.
L'algoritmo che noi chiamiamo `DAGShortestPath()` e' in realta' `Bellman-Ford()` ottimizzato per il caso aciclico.

``` Pseudocodice
proc DAGShortestPath(G, s) {
	TopologicalSort(G)
	InizializeSingleSource(G, s)
	for (u in G.V - in order)
		for (v in G.Adj[u]) Relax(u, v, W)
}
```

### Correttezza e complessità di `DAGShortestPath()`
Con l'analisi aggregata vediamo che la **complessità** di `DAGShortestPath()` e' $\Theta(|V| + |E|)$ e la sua **terminazione** e' ovvia.
Dimostriamo la **correttezza**. 
- L'**invariante** che usiamo e': assumendo che $v$ sia raggiungibile da $s$ per un percorso minimo qualsiasi $s = v_0, v_1, \ldots, v_k = v$, dopo l'$i$-esima esecuzione del ciclo, si ha che $v_i.d = \delta(s, v_i)$.
- **Caso base**: 
	- $s = v, \space v.d = 0$ (`InizializeSingleSource()`) e poiche' $s$ non puo' essere raggiungibile da se stesso, nessun rilassamento modifica $s.d$, che rimane 0.
- **Caso induttivo**:
	- consideriamo un percorso di lunghezza superiore a 0 da $s$ a $v$: $s = v_0, v_1, \ldots, v_k = v$.
	- Giacche' i vertici vengono considerati in ordine topologico, gli archi $(v_0, v_1), (v_1, v_2), \ldots$ vengono considerati precisamente in questo ordine.
	- Quindi se e' vero per ipotesi induttiva che $v_j.d = \delta(s, v_j)$ quando viene rilassato l'arco $(v_j, v_{j+1})$ succede che $v_{j+1}.d$ viene aggiornato al valore corretto $\delta(s, v_{j+1})$.
- E' ovvio che $G_\pi$ sia l'albero dei percorsi minimi a fine esecuzione.

## Algoritmo di Dijkstra
L'algoritmo di Dijkstra migliora le prestazioni di `Bellman-Ford()` aggiungendo l'ipotesi che tutti gli archi hanno peso positivo o 0, utilizzando una tecnica che ricorda l'algoritmo di Prim.
Come in `MSTPrim()` abbiamo bisogno di una coda di priorita'. La chiave sara' il valore $v.d$ di ogni vertice.

``` Pseudocodice
proc Dijkstra(G, s) {
	InizializeSingleSource(G, s)
	S = 0
	Q = G.V
	while (Q != 0) 
		u = ExtractMin(Q)
		S = S cup {u}
		for (v in G.Adj[u])
			if (v in Q)
				Relax(u, v, W)
}
```

Come per `MSTPrim()`, l'opreazione di cambio di valore di una chiave contenuta in `Relax()` contiene un'operazione di decremento di una chiave nella coda di priorita'. Pertanto si applica solo ai vertici ancora nella coda di priorita'.
Dopo `InizializeSingleSource()` il valore $s.d = 0$; pertanto, $s$ si trova in cima alla coda di priorita' e sara' estratto per primo (tutti gli elementi hanno valore $\infty$).
Nessuno inserisce elementi in $Q$, pertanto il ciclo **while** viene eseguito precisamente $|V|$ volte.
Per mostrare la correttezza, notiamo come la coda FIFO di `BreadthFirstSearch()` viene sostituita dalla coda di priorita' di `Dijkstra()`.

### Correttezza e complessità dell' algoritmo di Dijkstra
Dimostriamo che l'algoritmo e' **corretto**.
- L'**invariante** e': all'inizio di ogni iterazione del ciclo **while**, $v.d = \delta(s, v)$ per ogni $v \in S$:
	- **Caso base**: quando $S = 0$, l'invariante e' triviale vera
	- **Caso induttivo**.
		- ragioniamo per contraddizione
		- Sia $v$ il primo vertice in $S$ tale che $v.d \ne \delta(s,v)$ e sia $u$ il vertice estratto immediatamente prima di $v$.
		- Chiaramente $v \ne s$ e $\delta(s, v) \ne \infty$
		- Supponiamo che $v.d < \delta(s, v)$. Naturalmente $u.d = \delta(s, u)$; poiche' $v.d = u.d + W(u, v)$ si ha che $\delta(s.v) > \delta(s, u) + W(u, v)$ che viola la disuguaglianza triangolare.
		- Se $v.d > \delta(s, v)$, allora il percorso minimo da $s$ a $v$ passa per un certo vertice $x$, diverso da $u$.
		- Ma allora deve succedere $\delta(s, x) < \delta(s, u) + W(u, v)$, perche' tutti gli archi sono non negativi.
		- Dunque, $x$ viene estratto prima di $v$ e allora $v.d$ non puo' avere una stima definitiva a questo punto e questa e' una contraddizione.

**Complessità**: 
- Esattamente come per `MSTPrim()` possiamo implementare la coda di priorita' con un array senza ordine oppure con una heap binaria e dobbiamo considerare separatamente il caso dei grafi densi e di quelli sparsi.
- Se il grafo e denso, usiamo una coda senza struttura per ottenere:
	- $\Theta(|V|)$ (inizializzazione)
	- $\Theta(|V|)$ (costruzione della coda)
	- $\Theta(|V|^2)$ (estrazione del minimo)
	- $\Theta(|E|) = \Theta(|V|^2)$ (per i decrementi)
	- Totale: $\Theta(|V|^2)$
- Se il grafo e' sparso usiamo una heap binaria e avremo:
	- $\Theta(|V|)$ (inizializzazione)
	- $\Theta(|V|)$ (costruzione della coda)
	- $\Theta(|V| \cdot log(|V|))$ (estrazioni del minimo)
	- $\Theta(|E| \cdot log(|V|))$ (per i decrementi)
	- Totale: $\Theta(|E| \cdot log(|V|))$