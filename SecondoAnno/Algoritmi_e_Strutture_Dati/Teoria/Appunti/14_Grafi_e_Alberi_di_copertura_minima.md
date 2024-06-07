## Alberi di copertura minimi
Ci concentriamo sui grafi pesati indiretti connessi. Sia quindi $G = (V, E, W)$. Un grafo indiretto pesato puo' essere una buona rappresentazione di situazioni reali, come ad esempio una rete di connessioni informatiche tra computer, dove il peso di ogni arco rappresenta il costo della connessione. In una situazione come quella descritta possiamo domandarci qual'e' il costo di visitare ogni vertice, ed in particolare se c'e' una scelta di archi ottima, che minimizza il costo.

Definiamo un **alberi di copertura minimo** (o **MST**) come un sottoinsieme di archi che forma un albero, copre tutti i vertici e la cui somma dei pesi e' minima.

> MST $\rightarrow$ **minimum spanning tree** 

La strategia generale per risolvere questo problema e' di tipo **greedy**. Questo significa che ad ogni passo faremo la scelta **localmente** migliore e vogliamo in questo modo ottenere il risultato **globalmente** migliore. 

Osserviamo che un MST $A$ e' un insieme di archi tali che formano un albero (indiretto) che tocca tutti i nodi del grafo. Quindi, eliminando qualunque arco da un albero di copertura si ottengono due alberi connessi.
Qualunque partizione di $V$ in due sottoinsiemi $S$  e $V \backslash S$ viene chiamato **taglio del grafo** (semplicemente, **taglio**). Costruire un MST $T$ significa considerare un taglio ed aggiungere progressivamente un arco, partendo dal taglio piu' semplice che comprende un solo nodo di $S$ .

![[taglioVS.png]]

Consideriamo un taglio qualsiasi $(S, V \backslash S)$ tale che per una certa coppia di nodi $a, \space b, \space a \in S \space \text{e} \space b \notin S$. Poiche' vogliamo costruire un MST, dovremo scegliere un arco che connetta qualche nodo di $S$ con qualche nodo di $V \backslash S$: se non lo facessimo non potremo mai ottenere un MST.
Immaginiamo adesso che tra tutti gli archi con questa proprieta', l'arco $(a, b)$ sia quello di peso minimo. Vogliamo mostrare che **l'arco** $(a, b)$ **deve essere scelto**. 

Immaginiamo di non scegliere $(a, b)$ nella costruzione di un MST. Per la proprieta' detta precedentemente, a fine operazioni ci sara' certamente un arco $(u, v)$ che connette qualche nodo di $S$ con qualche nodo di $V/S$:

![[taglioVS2.png]]

Sia $T$ l'MST ottenuto scegliendo $(u, v)$ invece di $(a, b)$. Possiamo costruire un nuovo albero $T'$ cosi': $T' = (T \backslash \{(u, v)\}) \cup \{(a, b)\}$. Chiaramente, se $T$ era un albero d copertura, lo e' anche $T'$; inoltre $T'$ pesa meno di $T$. Pertanto $T'$ **non poteva essere un** MST.
Abbiamo dimostrato la seguente proprieta':
- considerata qualunque situazione di un MST, il prossimo passo per la costruzione e' segliere sempre l'arco di peso minimo che lo attraversa.
Chiamiamo questo arco **sicuro**.

La proprieta' vista prima e' un algoritmo di costruzione di un MST, che non rimane che codificare. Ci permette di fare le seguenti considerazioni. Se tutti i pesi di archi di $G$ sono diversi tra loro, allora l'MST e' unico: per ogni taglio ci sarebbe sempre una sola scelta. 
L'algoritmo di Prim e' un modo efficiente per vedere quanto visto.

### Alberi di copertura minimi: algoritmi di Prim
L'idea di base dell'algoritmo di Prim e' quella di partire da un vertice qualsiasi e, ad ogni passo, aggiungere un arco in modo che l'arco aggiunto sia sicuro. La corretta struttura dati in questo caso deve permettere di mantenere un insieme di vertici in maniera da poter facilmente individuare ed estrarre, tra questi, quello che comporta una spesa minima in termini di peso dell'arco che viene scelto per raggiungere quel vertice.

Ogni vertice $G$ viene arricchito con due campi:
- `v.key` $\rightarrow$ il peso minimo, inizialmente $\infty$, tra gli archi che connettono qualche vertice $T$ con $v$ 
- `v.pi` $\rightarrow$ il padre di $v$, inizialmente `Nil`, nel MST risultante.
Inizialmente tutti i vertici si trovano nella coda di priorita' $Q$ semi-ordinata su `v.key`, dove inizialmente tutti gli elementi sono $\infty$. La radice $r$ e' data esplicitamente e si cerca un MST radicato in $r$. Ad ogni scelta, i pesi degli elementi in $Q$ vengono modificati in base al principio che abbiamo spiegato, il vertice estratto da $Q$ e inserito in $T$ e ripetiamo finche' $Q$ si svuota.

``` Pseudocodice
proc MST-Prim(G, w, r) {
	for (v in G.V)
		v.key = infty
		v.pi = Nil
	r.key = 0
	Q = G.V
	while (Q != 0)
		u = ExtractMin(Q)
		for (v in G.Adj[u])
			if ((v in Q) and (W(u, v) < v.key))
				v.pi = u
				v.key = W(u, v)
}
```

### Correttezza di `MST-Prim`
Per mostrare che `MST-Prim` e' **corretto**, definiamo $T$ come l'insieme di tutte le coppie $(v.pi, v)$ tali che `v.pi` e' definito e $v \notin Q$.
Mostriamo che vale la seguente **invariante**: $T$ e' sempre sottoinsieme di qualche MST.
- Nel **caso base**, $T$ e' vuoto e l'invariante e' rispettata in maniera triviale.
- Supponiamo che l'invariante valga per $T$ ad un certo punto della computazione (**caso induttivo**) e consideriamo l'insieme $T'$ ottenuto dopo una esecuzione del ciclo **while**. Succede che $T' = T \cup \{(v.\pi, v)\}$, e $v$ e' il vertice tale che `v.key` e' il minore tra quelli ancora nella coda $Q$. Vogliamo mostrare che $T'$ è ancora sottoinsieme di qualche MST. Sia $S$ l'insieme di tutti e soli i vertici coperti da archi di $T$. Chiaramente, ($S, V / S$) è un taglio di $G$, e chiaramente, l'arco $(v.π, v)$ è un arco sicuro per il taglio. Pertanto, $T'$ è ancora sottoinsieme di qualche MST.

#### Complessità di `MST-Prim`
In alternativa possiamo usare una **heap binaria** per implementare la coda, il che ci permette di avere un vantaggio in caso di grafi sparsi. Avremo che la costruzione della coda costa sempre $\Theta(|V|)$, l'estrazione del minimo costa $\Theta(log(|V|))$ e il decremento costa $\Theta(log(|V|))$.
Con un grafo sparso avremo:
- $\Theta(|V|)$ $\rightarrow$ inizializzazione
- $\Theta(|V|)$ $\rightarrow$ costruzione della coda
- $\Theta(V \cdot log(|V|))$ $\rightarrow$ estrazione del minimo
- $\Theta(E \cdot log(|V|))$ $\rightarrow$ decrementi
- **totale** $\rightarrow$ $\Theta(E \cdot log(|V|))$

Il caso peggiore si verifica con un grafo denso e per minimizzare il danno si preferisce l'implementazione con una coda senza struttura. I grafi si considerano non densi anche quando $|E|$ cresce rispetto a $|V|$ ma non si avvicina asintoticamente a $|V|^2$; anche in questo caso, la seconda scelta e' migliore della prima.

### Alberi di copertura minimi: algoritmo di Kruskal
Una valida alternativa a `MST-Prim` è l'algoritmo noto come algoritmo di Kruskal, che utilizza una generalizzazione del concetto di taglio e del concetto di arco sicuro per il taglio al ne di ottenere un albero di copertura minimo.

L'idea di `MST-Kruskal` e' che possiamo ordinare gli archi in ordine crescente di peso e, analizzandoli uno a uno in questo ordine, stabilire se inserirlo come parte dell'albero di copertura minimo oppure no.
Quale sarebbe la ragione di non farlo a un certo punto dell'esecuzione? Semplicemente, un arco $(u, v)$ **non** e' parte di nessun MST se $u$ e $v$ sono gia' connessi da qualche altro arco precedentemente scelto.
Sia $T$ l'insieme di archi che abbiamo scelto fino ad un certo punto. $T$ a differenza del caso di `MST-Prim` non e' necessariamente un albero ad ogni momento, ma lo e' direttamente alla fine della computazione. Dati gli archi di $T$ e dato l'insieme $V$ di vertici, diciamo che un sottoinsieme $S$ di $V$ e' T-**connesso** se, considerando solo archi in $T$, e' un albero ed e' massimale.

Dato $T$ ad un certo punto della computazione, identifichiamo tutte le componenti `T-connesse` di $V$: $S_1, S_2, ..., S_n$. La tupla $(S_1, S_2, ..., S_n)$ e' certamente una partizione di $V$ e generalizza il concetto di taglio visto prima. Lo chiamiamo **taglio generalizzato**. Possiamo affermare che **un arco di peso minimo tra quelli non ancora considerati** che attraversa un taglio generalizzato e' un arco sicuro.

Come facciamo a garantire che un arco scelto attraversi il taglio? La definizione e' semplice:
- un arco $(u, v)$ attraversa un taglio generalizzato se $u$ e $v$ appartengono a diverse componenti `T-connesse`.
Quindi abbiamo bisogno di una struttura dati per insiemi disgiunti.

``` Pseudocodice
proc MST-Kruskal(G, w) {
	T = 0
	for (v in G.V)
		MakeSet(v)
	SortNotDecreasing(G.E)
	for ((u, v) in G.E - in order)
		if (FindSet(u) != FindSet(v))
			T = T \cup {(u, v)}
			Union(u, v)
	return T
}
```

#### Correttezza di `MST-Kruskal`
Per mostrare che `MST-Kruskal` e' **corretto**, osserviamo che tutti gli insiemi $S_1, S_2, ..., S_{|V|}$ che vengono creati da `MakeSet` nella prima operazione costituiscono un taglio generalizzato, considerando che $T$ e' vuoto.
Dimostriamo la seguente **invariante**: ogni arco che viene aggiunto nel ciclo non e' sicuro.
- Poiché gli archi vengono ordinati in maniera crescente, l'invariante è vera all'inizio (**caso base**): il primo arco scelto è un arco di peso minimo, e certamente attraversa il taglio.
- Supponiamo adesso che l'invariante sia vero fino ad una certa esecuzione (**caso induttivo**). Sia $(S_1, . . . , S_n$), con $n ≤ |V|$, il taglio generalizzato corrente, e sia $(u, v)$ l'arco considerato. Per definizione di `FindSet`, $u$ e $v$ appartengono a due diverse componenti T-connesse, quindi attraversa il taglio. Ogni arco $(u', v')$ di peso minore di $(u, v)$ è gia' stato considerato prima di $(u, v)$, e attraversava il taglio quando era stato considerato (dunque, dopo Union, non lo attraversa piu') oppure non lo attraversava. Quindi $(u', v')$ non attraversa il taglio, e $(u, v)$ è un arco di peso minimo che attraversa il taglio.

#### Complessità di `MST-Kruskal`
Per calcolare la **complessità** di `MST-Kruskal` dobbiamo distinguere tra grafi sparsi e grafi densi. 
Se il grafo e' denso allora abbiamo:
- inizializzazione $\rightarrow (O(1))$
- ordinamento $\rightarrow (\Theta(|E| \cdot log(|E|)) = \Theta(|V|^2 \cdot log(|E|)) = \Theta(|V|^2 \cdot log(|V|)))$
- piu' $O(|V| + |E|)$ diverse operazioni su insiemi, di cui $O(|V|)$ sono `MakeSet`
- **totale** $\rightarrow \Theta(|V|^2 \cdot log(|V|))$.

Se il grafo e' sparso abbiamo:
- inizializzazione $\rightarrow O(1)$
- ordinamento $\rightarrow \Theta(|E| \cdot log(|E|))$ 
- $O(|V| + |E|)$ diverse operazioni di cui $O(|V|)$ sono `MakeSet`
- **totale** $\rightarrow \Theta(|E| \cdot log(|E|))$.