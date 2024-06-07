## Grafi: introduzione
Un **grafo** e' una tripla $G = (V, E, W)$ composta da un insieme di $V$ di **vertici**, un insieme $E \subseteq V * V$  di **archi**, e una funzione $W : E \rightarrow \mathbb{R}$ che assegna un peso ad ogni arco. Il grafo $G$ e' **indiretto** se vale sia $(u, v) \in E \iff (v, u) \in E$ che $W(u, v) = W(v, u)$, e **diretto** altrimenti.
Quando da un vertice $u$ possiamo raggiungere un vertice $v$ usiamo il simbolo $u ⇝ v$.
Il **grado entrante** di un **nodo** di un grafo e' il numero di archi che lo raggiungono, ed il **grado uscente** il numero di archi che lo lasciano. Il grafo $G$ e' **pesato** se $W$ non e' costante, e **non pesato** altrimenti (spesso viene usata la notazione $G = (V, E)$).

Chiamiamo **sparso** un grafo tale che $|E| << |V|^2$, e lo definiamo **denso** altrimenti.
Ci sono due standard per rappresentare un grafo:
- con **liste di adiacenza**;
- con una **matrice di adiacenza**.
In entrambi i casi, e' conveniente pensare che ogni vertice $v \in V$ sia identificabile con un numero naturale da 1 a $|V|$.

Nel caso della rappresentazione a liste di adiacenza, usiamo un array $Adj[1, ..., |V|]$ dove ogni elemento punta ad una lista. Per ogni vertice $v$, la lista puntata da $Adj[v]$ contiene la chiave $u$ se e solo se $(v, u) \in E$ e, nel caso piu' generale, il nodo della lisa contiene anche il peso dell'arco. (per i grafi sparsi si preferisce questa rappresentazione).
![[Grafi_rappr_liste.png]]

Nel caso della matrice di adiacenza, usiamo una matrice $W$ che contiene sia l'informazione sul peso di ogni arco sia quella della sua esistenza. Chiaramente e' una matrice quadrata di lato $|V|$, e si preferisce questo metodo per i grafi densi.
Invece di $W[i, j]$ si usa la scrittura $W_{ij}$.
![[Grafi_rappr_matrici.png]]

I grafi (e gli algoritmi su di essi) sono pensati per essere **statici**.
Ma come ci comportiamo per un grafo dinamico? Esistono almeno due soluzioni per la rappresentazione di questo caso:
- La prima prevede nodi come oggetti che includono puntatori ai nodi (che rappresentano gli archi).
	- Soluzione complessa quando il grafo e' pesato e quando e' ignoto il grado uscente massimo
- La seconda prevede che sia i nodi, sia gli archi siano oggetti, entrambi inseriti in liste doppiamente collegate.

==Un grafo== (per noi) ==e' una struttura dati **statica**, **non basata sull'ordinamento** e rappresentata in maniera **sparsa o compatta** a seconda del caso==.

Per quanto riguarda il trattamento degli algoritmi sui grafi useremo la classica notazione `u.att` per indicare un attributo `att` associato con un vertice $v$. 
Per quanto riguarda la complessita' degli algoritmi sui grafi, dobbiamo tenere conto di entrambe le componenti $E$ e $V$. Un algoritmo **lineare** nel caso peggiore avra' complessita' $\Theta(|V| + |E|)$ o $O(|V| + |E|)$. 

>`att` $\rightarrow$ nome generico di attributo

Le applicazioni dei grafi sono innumerevoli:
- rappresentazione di una rete sociale, dove i nodi sono le persone e gli archi le amicizie o i legami;
- rappresentazione di un programma, dove i nodi sono funzioni e gli archi sono le dipendenze;
- rappresentazione di una mappa, dove i nodi sono i luoghi e gli archi sono le strade che portano da un nodo all'altro.

Poi esistono applicazioni piu' complesse:
- intelligenza artificiale, che usano i grafi ad esempio per rappresentare i testi dai quali si vuole estrarre informazioni.

## Grafi: visita in ampiezza

> Distinguiamo due tipi di visite:
> 	- Search $\rightarrow$ vedo tutti i nodi 
> 		- si dividono a loro volta in ampiezza e profondita'
> 	- Visit (trasversale) $\rightarrow$ non vedo tutti i nodi

Dato un grafo $G = (V, E)$ diretto o indiretto, non pesato, ed un vertice particolare chiamato **sorgente** $s \in V$, vogliamo sapere quanti archi sono necessari a raggiungere qualunque altro vertice (raggiungibile) da $s$. Utilizziamo una visita di $G$ chiamata **in ampiezza**.
`BreadthFirstSearch` esplora sistematicamente gli archi di $G$ e cerca di **scoprire** nuovi vertici raggiungibili da quelli gia' conosciuti. Computa la distanza minima in termini di numero di archi che esiste tra $s$ ed ogni vertice scoperto e produce un **albero di visita in ampiezza** che contiene tutti i vertici raggiungibili. 
Non ha tanto senso utilizzare questa visita per i grafi pesati, anche se funziona. Viene naturale applicare la visita in ampiezza a grafi indiretti.

> visita in ampiezza $\rightarrow$ preferibilmente applicabile a ==grafi indiretti e non pesati==

Utilizzeremo i colori bianco, grigio e nero per colorare i vertici mentre sono scoperti.
Tutti sono bianchi all'inizio: quando un nuovo vertice e' scoperto diventa grigio e quando tutti i vertici adiacente ad esso sono stati scoperti, diventa nero (ed il suo ruolo termina). Solo $s$ e' colorato di grigio all'inizio e durante la scoperta $s$ diventa la radice dell'albero di visita in ampiezza.
Alla scoperta di un nuovo vertice $v$ a partire da un vertice gia' scoperto $u$, l'arco $(u, v)$ diventa parte dell'albero e $u$ viene marcato come **predecessore** di $v$ nell'albero stesso.
Un vertice bianco viene scoperto al massimo una volta.

> attributi:
> 	- `u.color`
> 	- u.$\pi$ $\rightarrow$ rappresenta il suo predecessore

``` Pseudocodice
proc BreadthFirstSearch(G, s) {
	for (u ∈ G.V \ {S})
		u.color = WHITE
		u.d = inf
		u.pi = nil
	s.color = GREY
	s.d = 0
	s.pi = nil
	Q = 0
	Enqueue(Q, s)
	while (Q != 0)
		u = Dequeue(Q)
		for (V ∈ G.Adj[u])
			if (v.color = WHITE) then
				v.color = GREY
				v.f = u.d + 1
				v.pi = u
				Enqueue(Q, v)
		u.color = BLACK
```

> primo ciclo for $\rightarrow$ inizializzazione del grafo
> Q = 0 $\rightarrow$ inizializzazione della coda FIFO

Tutti i nodi ad eccezione di $s$ sono inizializzati di colore bianco, la loro distanza a infinito e il loro padre nell'albero a visita a nullo. I vertici vengono inseriti nella coda e gli adiacenti ai vertici vengono scopeti nell'ordine di inserimento. L'elemento $v.\pi$, per ogni $v$ e' chiamato **puntatore** (al **padre** di $v$ nell'albero di visita in ampiezza). Non e' un puntatore classico, ma contiene il nome del padre di $v$ nell'albero di visita.
## Correttezza e complessità di `BreadthFirstSearch`
Definiamo la **distanza piu' corta** di $v$ dalla sorgente $s$ ($\delta(s,v)$) come il **numero minimo di archi che sono necessari per raggiungere** $v$ da $s$.
Le sue proprieta' sono:
- che sia zero tra un vertice e se stesso $\rightarrow$ $\delta(s,s) = 0$;
- che sia infinito da $s$ a $v$ quando il secondo e' irraggiungibile dal primo $\rightarrow \delta(s,v) = \infty$;
- che sia una distanza $\rightarrow$ disuguaglianza triangolare (per ogni coppia di vertici $v, u$ tali che esiste un arco $(u,v)$, succede che $\delta(s,v) \le \delta(s,u)+1$).

**Correttezza** e **terminazione** di `BreadthFirstSearch`. Mostriamo che dopo l'esecuzione, per ogni $v$ raggiungibile da $s$, $\text{v.d} = \delta(s,v)$ e se $v \ne s$, almeno uno dei percorsi piu' brevi da $s$ a $v$ si ottiene da uno dei percorsi piu' brevi da $s$ a $v.\pi$ con l'arco $(v.\pi, v)$.
Osserviamo che in ogni momento gli elementi di $Q$ sono ordinati per stima, cioe' se $u$ e' prima di $v$ in $Q$ allora $u.d \le v.d$, e che se $\delta(\text{s, u}) \le \delta(\text{s, v})$, $u$ entra in $Q$ prima di $v$.
Dimostriamo la correttezza semplicemente trasformandola in **invariante**: appena un vertice $v$ entra in $Q$, si ha che $\text{v.d} = \delta(s, v)$; ragioniamo per induzione sul momento in cui $v$ entra nella coda.

- **Caso base**: $v=s$. Chiaramente all'entrata in $Q$ $\text{s.d} = 0 = \delta(s, s)$.
- **Caso induttivo**. Consideriamo $v$ scoperto durante l'esplorazione degli archi di $u$ Supponiamo, per assurdo, che $v$ sia il primo vertice che entra nella coda con $\text{v.d} \ne \delta(s, v)$. Siccome $v$ e' entrato, questo e' accaduto perche' il vertice $u$ e' appena uscito. Per ipotesi induttiva, $\text{u.d} = \delta(s, u)$ e viene assegnato $v.d = u.d + 1$, quindi $\text{u.d} = \delta(s, u)$. Se, nell'ipotesi assurda, fosse che $\text{v.d} \le \delta(s, v)$ si avrebbe $\delta(s, v) > \delta(s, u)+1$, che contraddice la disuguaglianza triangolare. Allora l'unica possibilita' e' che $\text{v.d} > \delta(s, v)$, cioe' che esista $W$ tale che $\delta(s, w) < \delta(s, u)$ e $(w,v) \in E$. Ma siccome $v$ e' stato scoperto da $u$, $W$ non puo' mai avere preceduto $u$ in $Q$. Ma questo contraddice il fatto che $\delta(s,w) < \delta(s,u)$. Quindi $\text{v.d} = \delta(s, v)$.

La **complessità** della visita e' semplice da calcolare: e' facile osservare che un nodo entra nell coda al massimo una volta. Per ogni nodo che e' entrato nella coda si analizzano i suoi adiacenti ma il totale degli archi e' comunque $|E|$. 
Questa analisi e' detta **aggregata**, ed e' usata spesso nei grafi.
Con questa analisi, se il grafo e' connesso, allora la complessita' e' $\Theta(|V| + |E|)$ in tutti i casi. Se il grafo e' sconnesso, la complessita' e' $O(|V| + |E|)$. La funzione $|V| + |E|$ **diventa** $|V|^2$ quando il grado e' denso. Quindi nel caso peggiore (grafo connesso e denso) e' $\Theta(|V|^2)$, ma normalmente si accetta $\Theta(|V| + |E|)$.
