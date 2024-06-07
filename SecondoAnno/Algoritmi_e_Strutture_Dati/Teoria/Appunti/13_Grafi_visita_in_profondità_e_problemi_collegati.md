## Grafi: visita in profondità
Consideriamo un grafo $G$. E' indifferente se $G$ e' o no pesato, o se $G$ e' o no diretto (come nelle visite in ampiezza). Ma se $G = (V, E)$, e' diretto. 
Ci proponiamo di risolvere tre problemi:
- stabilire se $G$ e' **ciclico** $\rightarrow$ stabilire se contiene almeno un ciclo;
- costruire un **ordinamento topologico** di $G$ $\rightarrow$ elencare tutti i suoi vertici in un ordine qualsiasi tale che ogni vertice $v$ e' elencato solo se tutti i vertici **dai quali** $v$ si puo' raggiungere sono stati elencati prima;
- conoscere ed enumerare tutte le **componenti fortemente connesse** di $G$ $\rightarrow$ elencare tutti i sottoinsiemi massimali di $V$ tale che, ogni vertice in ogni sottoinsieme raggiunge ogni altro vertice di quel sottoinsieme.
La soluzione in comune per questi tre problemi e' la visita in profondita'.

Il proposito della visita in profondita' e' quello di scoprire tutti i vertici raggiungibili da ogni potenziale sorgente $s$. La differenza tra le due visite e; che i vertici nella visita in profondita' vengono scoperti il **prima possibile** a partire da quelli gia' scoperti: la visita in profondita' e' ricorsiva. Anche `DepthFirstSearch` usa un sistema di colorazione per ricordare i vertici ancora da scoprire e quelli gia' scoperti. `DepthFirsSearch` assume che $G$ sia rappresentato con liste di adiacenza e riempie un campo $v.\pi$ per generare un **albero di visa in profondità** come risultato della visita; la differenza qui e' che invece di un solo albero, si produce una **foresta** di alberi, uno per ogni sorgente.
`DepthFirstSearch` riempie anche dei campi $v.d$ (**momento** della scoperta) e $v.f$ (momento nel quale il vertice viene abbandonato). I campi $v.d$ e $v.f$ sono interi tra $1$ e $2 \cdot |V|$. Vengono generalmente chiamati **tempi**. Per ogni vertice $u$, abbiamo $u.d < u.f$.

``` Pseudocodice
proc DepthFirstSearch(G) {
	for (u in G.V)
		u.color = WHITE
		u.pi = Nil
	time = 0
	for (u in G.V)
		if (u.color = WHITE) then 
			DepthVisit(G, u)
}

proc DepthVisit(G, u) {
	time = time + 1
	u.d = time
	u.color = GREY
	for (v in G.Adj[u])
		if (v.color = WHITE) then 
			v.pi = u
			DepthVisit(G, u)
		u.color = BLACK
		time = time + 1
		u.f = time
}
```

> `time` $\rightarrow$ variabile globale

Ogni volta che `DepthVisit` viene chiamata, si inizia un nuovo albero della foresta degli alberi di visita in profondita'. Se $G$ e' tale che tutti i vertici sono raggiungibili dal primo vertice visitato, allora ci sara' un solo albero, altrimenti ce ne saranno di piu'.
Ogni vertice $u$ sul quale si chiama `DepthVisit` e' inizialmente bianco, il suo tempo di scoperta viene marcato come tempo di inizio ($u.d$) e viene colorato di grigio.
Ogni vertice nella lista di adiacenza di $u$ viene esplorato ricorsivamente; alla fine di questa esplorazione $u$ viene marcato come nero ed il suo tempo finale $u.f$ viene registrato.

### Complessità di `DepthFirstSearch`
L'analisi della **complessità** viene calcolata attraverso la tecnica dell'analisi aggregata.
`BreadthFirstSearc` esegue un ciclo di che costa $|V|$ per la colorazione iniziale e per ogni vertice chiama `DepthVisit`. Ma il massimo numero di chiamate a `DepthVisit` e' $|E|$. Quindi la complessità e' $\Theta(|V| + |E|)$, che nel caso peggiore diventa $\Theta(|V|^2)$ (anche se accettiamo $\Theta(|V| + |E|)$).

## Grafi diretti: cicli
In un grafo diretto, chiamiamo **ciclo** un percorso $v_1, v_2, ..., v_k$ di vertici tali che per ogni $i$ esiste l'arco $(v_i, v_{i+1})$ e che $v_1 = v_k$. Quando un grafo diretto e' privo di cicli, lo chiamiamo `DAG` (Directed Acyclic Graph). 
Definiamo il seguente problema: dato un grafo diretto $G$ stabilire se presenta o no un ciclo. L'algoritmo `CycleDet` che usiamo prima di eseguire `DephtFirstSearch` modificata in maniera da interrompersi se si visita un nodo grigio: al momento di visitare un nodo grigio, siamo certi di aver trovato un ciclo.

``` Pseudocodice
proc CycleDet(G) {
	cycle = False
	for (u n G.V) u.color = WHITE
	for (u in G.V)
		if (u.color = WHITE) 
			then DepthVisitCycle(G, u)
	return cycle
}

proc DepthVisitCycle(G, u) {
	u.color = GREY
	for (v in G.Adj[u])
		if (v.color = WHITE)
			then DepthVisitCycle(G, v)
		if (v.color = GREY)
			then cycle = True
	u.color = Black
}
```

### Correttezza e complessità di `CycleDet`
**Correttezza**: l'algoritmo e' corretto se e solo se restituisce `True` quando $G$ e' ciclico e `False` altrimenti.
La **complessità** del nostro algoritmo e' la stessa della visita in profondita', e la **terminazione** e' ovvia.

## Grafi: ordinamento topologico di grafi diretti
Uno degli usi piu' interessanti dei grafi diretti consiste nel rappresentare un insieme di vincoli rispetto a un insieme finito di compiti (o **task**). Se tutti i task son inseriti in un grafo diretto $G$ dove un arco $(u,v)$ rappresenta che $u$ deve essere posto prima di $v$, una delle possibili soluzioni al problema di stabilire un ordine lineare (possibile) tra i task e' dato dall'**ordinamento topologico**. Il problema dell'ordinamento topologico non ha senso se il grafo diretto e' ciclico.

Il problema dell'ordinamento topologico prende in input un grafo connesso $G$ senza cicli e restituisce una lista collegata $v_1, ..., v_{|V|}$ di vertici topologicamente ordinati; per ogni coppia $v_i, v_j$ di vertici, $v_i$ appare prima nella lista di $v_j$ se e solo se $v_i$ precede topologicamente $v_j$.

``` Pseudocodice
proc TopologicalSort(G) {
	for (u in G.V) u.color = WHITE
	L = 0
	time = 0
	for (u in G.V)
		if (u.color = WHITE)
			then DepthVisitTS(G, u)
	return L
}

proc DepthVisitTS(G, u) {
	time = time + 1 
	u.d = time
	u.color = GREY
	for (v in G.Adj[u])
		if (v.color = WHITE)
			then DepthVisitTS(G, v)
		u.color = BLACK
		time = time + 1
		u.f = time
		ListInsert(L, u)
}
```

> `L` e' una variabile globale

### Correttezza e complessità di `TopologicalSort`
Per la **correttezza** di `TopologicalSort`, dopo l'esecuzione, la lista di uscita e' topologicamente ordinata. La **complessità** e' la stessa della visita in profondita' e la terminazione e' ovvia.

## Grafi: componenti fortemente connessi
Dato un grafo diretto $G$, una **componente fortemente connessa** (SCC) e' un sottoinsieme massimale $V' \subseteq V$ tale che, per ogni $u, v \in V'$, succede che $u ⇝ v$ e che $v ⇝ u$. 
Osserviamo il concetto corrispondente nel caso di grafi indiretti e' quello di **componente connessa**; due termini per indicare un'idea simile in due contesti leggermente diversi.

Un'elemento fondamentale nello studio delle SCC e' il grafo trasposto di $G$. Dato $G$ diretto, il **grafo trasposto** $G^T$ di $G$ e' ottenuto invertendo la direzione di ogni arco. La complessita' e' $\Theta(|V| + |E|)$ quando $G$ e' rappresentato con liste di adiacenza. La proprieta' piu' interessante di $G^T$ e' che $G$ e $G^T$ hanno le stesse SCC.

``` Pseudcodice
proc StronglyConnectedComponents(G) {
	for (u in G.V)
		u.color = WHITE
		u.pi = Nil
	time = 0
	for (u in G.V)
		if (u.color = WHITE) then
			DepthVisit(G, u)
	for (u in G.V)
		u.color = WHITE
		u.pi = Nil
	time = 0
	L = 0
	for (u in G^T.V in rev. finish time order)
		if (u.color = WHITE) then
			DephtVisit(G^T, u)
		ListInsert(L, u)
	return L
}
```

### Correttezza e complessità di `StronglyConnectedComponents`
Mentre il calcolo della **complessità** e' molto semplice e porta a concludere che la complessita' di `StronglyConnectedComponents` e' la stessa della visita in profondita', la dimostrazione della **correttezza** e' piu' complessa.
Osserviamo che da un grafo $G$ possiamo ricavare il suo **grado delle componenti connesse** $G^{SCC}$ semplicemente considerando tutti i vertici di ogni SCC di $G$ come un unico vertice di $G^{SCC}$ e impostando che esiste un arco $(u,v)$ in $G^{SCC}$ se e solo se esiste un arco in $G$ da uno dei nodi simboleggiati da `u` a uno dei nodi simboleggiato da $v$. Se $G^{SCC}$ e' un `DAG` possiamo calcolare il suo ordinamento topologico.

Per un gruppo di vertici $C$ (che potrebbe essere una SCC) chiamiamo $f(C)$ il massimo tempo $u.f$ tra tutti gli $u \in C$.  Osserviamo che la prima esecuzione di `DepthFirstSearch` ci da informazione sulle SCC di G. In poche parole, la componente $C'$ viene scoperta tutta prima della fine della scoperta di $C$. Questo significa che una esecuzione di `DepthFirstSearch` sul grafo $G^T$ ha esattamente la proprieta' contraria.

Ragioniamo adesso per induzione sull'indice $k$ che indica il k-esimo albero di visita in profondità generato dalla **seconda** visita `DepthFirstSearch`, quella effettuata su $G^T$ . Vogliamo mostrare che tutti questi alberi sono, in effetti, SCC di $G$ (e di $G^T$ ) (e questa è l'**invariante** che stiamo cercando).
- Quando $k=0$ (**caso base**) il risultato e' triviale
- Per il **caso induttivo** supponiamo che i primi $k$ alberi restituiti coincidano ancora con $k$ SCC di $G$, e dimostriamo che questo e' ancora vero per il 
  $k+1$-esimo albero. Supponiamo che $u$ sia la radice del $k+1$-esimo albero. Questo vertice e' tale che $u.f = f(C)$ ed e' maggiore di $f(C')$ per ogni SCC $C'$ ancora da visitare. Tutti i vertici in $C$ sono bianchi a questo punto e tutti diventano discendenti di $u$. Quindi, l'albero risultante contiene tutti gli elementi di $C$.

Rimane da mostrare che nessun $v$ che non appartenga a $C$ appare come discendente di $u$.
Infatti, se $v$ è discendente di u, deve succedere in $G^T$ che $u ⇝ v$, e che $v$ era bianco al momento di considerare $u$. Ma questo accade solo se $u /\rightsquigarrow v$ in $G$. Quindi, nella prima visita di $G$, $v$ è ancora bianco quando $u.f$ viene deciso, e quindi, quando v viene visitato (e abbandonato) il suo $v.f$ sarà maggiore di $u.f$ . Pertanto, $v$ è già apparso in qualche albero di visita (radicato in qualche $u'$) e quando viene visto durante la visita a partire da $u$ è già nero, e non inserito nel rispettivo albero di visita. Allora, come volevamo dimostrare, anche l'albero $k$ corrisponde ad una SCC di $G$.