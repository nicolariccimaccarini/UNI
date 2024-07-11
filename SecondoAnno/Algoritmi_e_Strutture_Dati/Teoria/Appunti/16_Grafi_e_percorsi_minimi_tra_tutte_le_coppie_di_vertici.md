## Percorsi minimi tra tutte le copie di vertici
Dato un grafo $G = (V, E, W)$ pesato, diretto e connesso, vogliamo calcolare il peso del **percorso minimo per ogni coppia di vertici**.
Osserviamo in primo luogo che se aggiungiamo l'ipotesi di non avere archi negativi, possiamo risolvere questo problema applicando $|V|$ volte l'algoritmo di `Dijkstra`.
- In caso di grafi densi, questo porterebbe ad un algoritmo $\Theta(|V|^3)$
- In caso di grafi sparsi, $\Theta(|E| \cdot |V| \cdot log(|V|))$ 
In caso di archi negativi siamo costretti ad usare `Bellman-Ford`, per una complessita' di $\Theta(|V|^4)$.
Ci domandiamo se possiamo fare meglio di cosi'.

Il primo problema che dobbiamo affrontare e' quello della rappresentazione. Fino ad ora abbiamo usato la rappresentazione a **liste di adiacenza**. Ma il problema dei percorsi minimi tra tutte le coppie e sostanzialmente diverso da quello con sorgente singola, e in un certo modo richiede in maniera naturale una rappresentazione **matriciale** del grafo.
Faremo la seguente assunzione: un grafo $G$ e' rappresentato dalla sola matrice $W$
di pesi.

Cosa ci aspettiamo come risultato di un algoritmo che risolve questo problema?
- Poiche' la rappresentazione e' matriciale, ci aspettiamo due matrici come risultato:
	- in una troveremo un'indcazione sul percorso (**matrice dei predecessori**, denotata con $\Pi$)
	- nell'altra troveremo i pesi calcolati (denotata con $D$)

Senza cicli negativi, rappresentare $G$ con la matrice dei pesi $W$ significa assumere che per ogni coppia $i, j$, 
- $W$ contiene **nil** se non c'e' un arco tra $i$ e $j$
- 0 se $i=j$
- il peso dell'arco tra $i$ e $j$ altrimenti
Osserviamo che potrebbero esserci **self loops** di peso positivo: questi cicli vengono ignorati perche' non hanno alcun ruolo nei cammini minimi.
La matrice $D$ di pesi calcolati si comportera' esattamente nello stesso modo e la matrice $\Pi$ dei predecessori avra' per ogni coppia $i, j$, **nil** se $i=j$ oppure se non esiste un percorso tra $i$ e $j$, e $k$ se per qualche percorso minimo, da $i$ a $j$, $k$ precede su di esso.

## Programmazione dinamica
Questo problema ci permette di introdurre una nuova tecnica, alternativa al dive and conquer e alla strategia greedy, chiamata **programmazione dinamica**. 
La programmazione dinamica e' pensata per risolvere in maniera efficiente un problema di ottimizzazione.
Nel divide and conquer, l'approccio ricorsivo serve a risolvere **sottoproblemi separati tra loro**; nella programmazione dinamica serve a risolvere **sottoproblemi comuni**.
In questo caso, il percorso minimo tra $a$ e $b$ potrebbe passare per $c$; se e' cosi' io devo sapere qual'e' il percorso minimo tra $a$ e $c$ e tra $c$ e $b$. Se il percorso minimo tra $a$ e $d$ passasse per $c$, quello tra $a$ e $c$ mi servirebbe di nuovo.
Una caratteristica importante della programmazione dinamica e' che tutta la complessita' di progettazione di accumula nella progettazione iniziale: le dimostrazioni di correttezza si riducono a osservare che l'algoritmo segue il modello dinamico che e' stato pensato.

## L'algoritmo della moltiplicazione di matrici
Facciamo le seguenti osservazioni:
1. e' conveniente pensare ai vertici come numerati da 1 a $|V|$ (e li indicheremo con indici $i, j, \ldots$);
2. come gia' sappiamo, nessun percorso minimo tra due vertici puo' avere piu' di $|V|-1$ archi (altrimenti sarebbe un ciclo).
In termini di notazione, useremo le matrici cosi': invece di $W[i,j]$ usiamo $W_{ij}$ e lo stesso varra' per $D$ e $\Pi$.

Sappiamo gia' che i percorsi minimi tra i due vertici sono caratterizzati da una sottostruttura ottima. Troveremo che questa e' proprio una condizione essenziale per l'applicazione della programmazione dinamica.

In quale modo possiamo vedere questa sottostruttura perche' sia utile adesso?
Definiamo $L^m_{ij}$ come **i peso minimo del cammino tra il vertice** $i$ **e il vertice** $j$ **che si puo' costruire usando al piu'** $m$ **archi**.
Chiaramente, $L^1_{ij} = W_{ij}$ per ogni coppia di vertici $i$ e $j$, ed il problema che affrontiamo consiste nel capire come trovare $L^m$ a partire da $L^{m-1}$.

Non e' difficile convincersi che:
$$ 
L^m_{ij} = \min\{L^{m-1}_{ij}, \min_k\{L^{m-1}_{ik} + W_{kj}\}\}
$$
Se **mi permetto** di usare un arco in piu', devo stabilire se mi conviene: questo accadra' se e solo se, tra tutti i vertici, ne esiste uno tale che e' migliore di quello attuale come predecessore di $j$.
Siccome $W_{ij}=0$ sempre possiamo semplificare in:
$$
L^m_{ij} = \min_k\{ L^m _{ik} + W_{kj} \}
$$

Questa uguaglianza si puo' applicare in maniera iterativa finche' si ottiene il percorso minimo.
Quando dovremmo fermarci? Sappiamo che in assenza di cicli negativi, ogni percorso minimo non puo' contare con piu' di $|V|-1$ archi. Dunque si ha che $L^{|V|-1}_{ij} = L^{|V|}_{ij} = L^{|V|+1}_{ij} = \ldots$. Questa sara' la condizione di stop.
Applichiamo la formula per calcolare la matrice $L$ di grado $|V|$, esattamente $|V|-1$ volte, partendo dalla matrice iniziale che e' data dal valore 0 sulla diagonale, e $\infty$ altrimenti.
Queste matrici si chiameranno $L^1 = W, L^2, \ldots, L|V| = D$ e l'ultima conterra' i pesi dei cammini minimi.

### L'algoritmo della moltiplicazione di matrici: versione lenta
``` Pseudocodice
proc ExtendShortesPath(L, W) {
	n = L.rows
	let L' be a new matrix
	for i=1 to n
		for j=1 to n
			L'_ij = infty
			for k=1 to n
				L'_ij = min{L'_ij, L_ik + W_kj}
	return L'
}

proc SlowAllPairsMatrix(W) {
	n = L.rows
	L^1 = W
	for m=2 to n-1
		L^m = ExtendedShortestPath(L^m-1, W)
	return L^n-1
}
```

La **correttezza** di questo algoritmo e' immediata perche' dipende dalla formula che abbiamo visto.
L'**invariante** sarebbe:
- dodo $m-1$ esecuzioni del ciclo piu' esterno, $L^m_ij$ contiene il percorso del cammino minimo da $i$ a $j$ che utilizza al massimo $m$ archi.
La sua **complessità** e' facile da calcolare:
- quattro cicli di lunghezza $|V|$, per un totale di $\Theta(|V|^4)$.

### L'algoritmo della moltiplicazione di matrici: versione veloce
L'osservazione principale e': abbiamo bisogno unicamente di $L^{|V|}$ e non di tutte le altre.
Potremmo chiederci qual'e' il costo minimo da $i$ a $j$ con 1 arco ($W$), con 2 archi ($L^2$, che viene combinando $W$ con se stesso), 4 archi ($L^4$, che viene combinando $L^2$ con se stesso), 8 archi e cosi' via.
Non abbiamo bisogno di calcolare $L^3, L^5, L^6, L^7$.

``` Pseudocodice
proc ExtendShortesPath(L, W) {
	n = L.rows
	let L' be a new matrix
	for i=1 to n
		for j=1 to n
			L'_ij = infty
			for k=1 to n
				L'_ij = min{L'_ij, L_ik + W_kj}
	return L'
}

proc FastAllPairsMatrix(W) {
	n = L.rows
	L^1 = W
	m = 1
	while (m < n-1)
		L^2*m = ExtendShortestPath(L^m, L^m)
		m = 2 * m
	return L^m
}
```

La **correttezza** e' ovvia.
La **complessità** diventa $\Theta(|V|^3 \cdot log(|V|))$. Questo si deve al fatto che uno dei quattro cicliche c'erano precedentemente e' stato convertito in una ricerca binaria:
- invece di esplorare tutte le matrici dalla prima alla $|V|$-esima, si procede attraverso i quadrati (1, 2, 4, 8, ...) fino a superare $|V|$; poiche' tutte le matrici di indice superiore al $|V|$-esimo sono uguali, possiamo restituirne una qualsiasi.

## Algoritmo di `FloydWarshall`
Nell'algoritmo di `FloyWarshall`, che migliora i due risultati precedenti, utilizzeremo idee molto simili; la differenza fondamentale e' la caratterizzazione della sottostruttura ottima.

La caratteristica della sottostruttura ottima in questo caso e' fatta attraverso i vertici.
Siano $i$ e $j$ due vertici qualsiasi e consideriamo un percorso minimo $i \rightsquigarrow j$. 
Consideriamo tutti i vertici diversi da $i$ e $j$ che compaiono su questo percorso: li chiameremo **intermediari**.
In $G$, per un percorso minimo $i \rightsquigarrow j$ i vertici intermediari possono essere qualsiasi vertice nell'insieme $\{1, \ldots, |V|\}$. Ma in generale possiamo chiederci qual'e' il percorso minimo tra $i$ e $j$ limitandoci a **pescare** i vertici intermediari in un qualunque sottoinsieme $\{ 1, \ldots, k \}$, con $k \le |V|$.
Usiamo questa notazione: $i \rightsquigarrow_k j$ per indicare un percorso minimo tra $i$ e $j$ dove tutti gli intermediari appartengono all'insieme $\{1, \ldots, k\}$.

Quindi, $i \rightsquigarrow_{|V|} j = i \rightsquigarrow j$. L'idea di `FloydWashall` e' che possiamo arrivare a $i \rightsquigarrow j$ esaminando tutti i percorsi minimi $i \rightsquigarrow_1 j$, $i \rightsquigarrow_2 j$, $\ldots$. 
Infatti, la relazione tra $i \rightsquigarrow_{k-1} j$ e $i \rightsquigarrow_k j$ e' semplice:
- $k$ non e' un intermediario di $i \rightsquigarrow j$, allora $i \rightsquigarrow_{k-1} j = i \rightsquigarrow_k j$
- se $k$ e' un intermediario di $i \rightsquigarrow j$, allora $i \rightsquigarrow_k j$ e' composto da $i \rightsquigarrow_{k-1} k$ e da $k \rightsquigarrow_{k-1} j$.

``` Pseudocodice
proc FloydWarshall(W) {
	n = W.rows
	D^0 = W
	for k=1 to n
		let D^k be a new matrix
		for i=1 to n
			for j=1 to n
				D^k_ij = min{D^k-1_ij, D^k-1_ik + D^k-1_kj}
}
```

La **correttezza** si basa sulle osservazioni precedenti.
La **complessità** e' $\Theta(|V|^3)$ e la sua terminazione e' ovvia.