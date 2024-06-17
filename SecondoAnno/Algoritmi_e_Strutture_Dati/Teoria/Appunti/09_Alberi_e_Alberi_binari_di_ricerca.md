## Alberi
Gli alberi sono ==strutture dati fondamentali dinamiche e sparse==. A seconda degli usi che se ne fanno, ==possono essere basate sull'ordinamento oppure no==.
Da un lato possiamo dire che gli alberi generalizzano le liste:
- se vediamo il puntatore **next** come un successore, allora nelle liste il successore e' unico e negli alberi no.
In questo senso, possiamo dire che i grafi, a loro volta generalizzano gli alberi.

Dal punto di vista delle strutture dati, liste, alberi e grafi sono oggetti molto diversi e con usi diversi; il fatto che uno sia la generalizzazione dell'altro non significa che gli utilizzi si ereditino.
==Attenzione a non confondere i vari concetti==: qui studiamo una struttura dati concreta; le heaps sono array che possono essere convenientemente viste come alberi; un albero di decisione, o un albero di ricorsione, sono strutture concettuali (non strutture dati).
La struttura dati **albero** e' troppo generica e ubiqua per essere associata a uno o piu' nomi specifici.

Un **albero radicato** (semplicemente **albero**) e' un grafo aciclico connesso tale che ogni coppia di vertici e' connessa da al piu' un cammino.
- I vertici vengono chiamato **nodi**. 
- Un'albero e' `k-ario` se ogni nodo ha al piu' $k$ figli distinti.
- Albero **completo**: ogni livello e' completo (tutti i nodi di uno stesso livello hanno esattamente zero o due figli);
- **quasi completo**: ogni livello, tranne eventualmente l'ultimo, e' completo;
- **bilanciato**: tra il percorso semplice piu' breve e' quello semplice piu' lungo la radice ed una foglia esiste la differenza, al massimo, costante;
- **pieno**: ogni nodo ha zero o due figli.

Anche queste definizioni sono soggette a variazioni:
- es: alcuni definiscono un albero bilanciato solo quando la differenza tra due percorsi semplici e' al massimo uno.

Un'albero puo' anche essere:
- **diretto**: sono tali che ogni sottoalbero ha un nodo predeterminato chiamato **radice**, che dota l'albero di un ordinamento topologico privilegiato.
- **indiretto**: la radice viene invece individuata in maniera arbitraria, cosi' come la direzionalita' dei cammini.
Quando esiste una direzione nei cammini, si individuano in maniera naturale le **foglie** (cioe' i nodi senza figli).
L'**altezza** di un albero e' il massimo numero degli archi su un percorso semplice dalla radice alla foglia.

### Alberi: altezza
Gli alberi binari quasi completi con `n` elementi hanno altezza $\Theta(log(n))$ (questo vale anche per gli alberi completi). 
Quando un albero non ha nessuna proprieta' strutturale, la sua altezza e' $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio.

### Alberi: struttura
I nodi di un albero binario possono esser pensati come oggetti che possiedono, almeno, una **chiave** (`x.key`) e tre puntatori:
1. il **padre** (`x.p`)
2. il **figlio destro** (`x.right`)
3. il **figlio sinistro** (`x.left`).
Tutti i puntatori sono **nil** quando non sono definiti.
L'operazione di creazione di un'albero vuoto e' immediata. Le operazioni di inserimento e cancellazione di un elemento dipendono dall'applicazione che abbiamo in mente.

![[AlberiStruttura.png]]

Gli alberi sono la struttura ideale per memorizzare oggetti che hanno una natura ricorsiva (come formule matematiche o formule logiche).
In intelligenza artificiale, un albero decisionale e' un algoritmo fondamentale di ragionamento automatico.
Ogni visita diversa corrisponde a uno scopo diverso e puo' dare informazioni diverse sull'albero. 
I nomi delle visite che vediamo sono originate, in parte, dal loro uso negli alberi semi-ordinati:
- visite **in-order**;
- visite **pre-order**;
- visite **post-order**.

### Alberi: `TreeInOrderTreeWalk`

``` Pseudocodice
proc TreeInOrderTreeWalk(x) {
	if (x != nil) then {
		TreeInOrderTreeWalk(x.left)
		Print(x.key)
		TreeInOrderTreeWalk(x.rght)
	}
}
```

#### Correttezza e complessità di `TreeInOrderTreeWalk`
La **terminazione** di questa procedura si vede immediatamente dalla struttura ricorsiva degli alberi e della procedura.
Lo stesso vale per la **correttezza**, basta vedere che tutti gli elementi sono visitati esattamente una volta.
La **complessità** di una visita non e' direttamente collegata all'altezza dell'albero e pertanto non presenta la separazione in caso medio e peggiore. 
Consideriamo la generica visita di un nodo $x$ tale che ci sono $k$ nodi nel sotto-albero radicato a sinistra il che implica che ci sono $n-k-1$ nodi nel sotto-albero radicato a destra. Possiamo considerare che il costo della visita a $x$ stesso sia costante.

Il costo totale e' dato dalla ricorrenza:
$$
T(n) = T(k) + T(n-k-1) + \Theta(1)
$$

Sospettiamo che $T(n) = \Theta(n)$ e dimostriamo che $T(n)=O(n)$. Ipotizziamo che $T(n) \le c \cdot n$ per qualche $c>0$:
$$
\begin{align}
T(n) &= T(k) + T(n-k-1) + \Theta(1) \\
&\le c \cdot k + c \cdot (n-k-1) + \Theta(1) \qquad &&\text{ipotesi} \\
&= c \cdot k + c \cdot n - c \cdot k - c + \Theta(1) &&\text{calcolo} \\
&= c \cdot n &&\text{calcolo} 
\end{align}
$$

Quindi $T(n)=O(n)$. Poiche' non e' possibile visitare un albero senza aver visitato tutti i nodi, la complessita' di una visita e' certamente $\Omega(n)$.
La complessita' di `TreeInOrderTreeWalk` e' $\Theta(n)$, in tutti i casi.

### Alberi: altre visite
Altre visite possibili si ottengono cambiando l'ordine delle chiamate ricorsive e ottengono risultati diversi.

``` Pseudocodice
proc TreeInOrderTreeWalk(x) {
	if (x != nil) then {
		Print(x.key)
		TreeInOrderTreeWalk(x.left)
		TreeInOrderTreeWalk(x.right)
	}
}

proc TreePostOrderTreeWalk(x) {
	if (x != nil) then {
		TreeInOrderTreeWalk(x.left)
		TreeInOrderTreeWalk(x.right)
		Print(x.key)
	}
}
```

### Alberi: introduzione algoritmica
Considerata l'alta versatilita' degli alberi, questi sono utilizzati in molti contesti diversi per sviluppare e testare l'introduzione algoritmica.
Esempi includono problemi tipici come:
- trovare un algoritmo per stampare tutte e sole le chiavi della **frontiera** di $T$;
- trovare un algoritmo per stampare tutte e sole le chiavi del **costato sinistro** (o destro) di $T$;
- dire quale/quali visite sono necessarie per ricostituire la struttura di $T$;
- arricchire la struttura di $T$ in maniera che ogni nodo punti anche allo **zio** (se esiste), otlre che al padre, e a molti altri.

## Alberi binari di ricerca: introduzione
Gli **alberi binari di ricerca (BST)** sono una struttura:
- dinamica;
- basata sull'ordinamento;
- implementata in maniera sparsa.
Associamo gli alberi binari di ricerca le operazioni di inserimento, cancellazione, ricerca, minimo, massimo, successore e predecessore. Possiamo farlo perche' la struttura e' basata sull'ordinamento delle chiavi.
I nodi di un BST sono nodi di un albero definiti come nel caso generale.

Le regole che un albero binario di ricerca deve rispettare (anche note come **proprieta' BST**), sono:
1. Per ogni nodo $x$, se un nodo $y$ si trova nel **sotto-albero sinistro**, allora `y.key <= x.key`;
2. Per ogni nodo $x$, se un nodo $y$ s trova nel **sotto-albero destro**, allora `y.key > x.key`.
Dunque si puo' dire che un BST e' **parzialmente ordinato**.

![[BST.png]]

### Alberi binari di ricerca: creazione e visita di un albero
I BST si creano vuoti come nel caso generale ed hanno esattamente la stessa struttura. Tutte le visite che abbiamo visto negli alberi generici hanno senso anche negli alberi binari di ricerca.
In alcuni casi (come quello della visita in order) restituiscono un risultato ancora piu' naturale.
Se un albero e' un BST ordinato, il risultato della sua visita in order e' l'insieme delle chiavi ordinato.

### Alberi binari di ricerca: ricerca, minimo e massimo
Vogliamo una struttura che dati `x, k` ritorna un puntatore al nodo che contiene 
`k`, se esiste, e ritorna **nil** altrimenti.

``` Pseudocodice
proc BSTTreeSearch(x, k) {
	if ((x = nil) or (x.key = k))
		then return x
	if (k <= x.key)
		then return BSTTreeSearch(x.left, k)
	else return BSTTreeSearch(x.right, k)
}
```

#### Correttezza e complessità di `BSTTreeSearch`
La **correttezza** di `BSTTreeSearch` e' triviale: se la chiave esiste, questa viene trovata sicuramente, perche' tutti i nodi nella parte dell'albero dove la chiave deve essere. 
La **complessità** di `BSTTreeSearch` e' direttamente proporzionale alla sua altezza. Nel caso peggiore, l'albero assume l'aspetto di una lista e la ricerca di una chiave che non esiste prende tempo $\Theta(n)$. Nel caso medio l'albero ha altezza logaritmica, e la ricerca di una chiave ha tempo $\Theta(log(n))$.

#### `BSTTreeMinimum`, `BSTTreeMaximum`, correttezza e complessità

``` Pseudocodice
proc BSTTreeMinimum(x) {
	if (x.left = nil)
		then return x
	return BSTTreeMinimum(x.left)
}
```

Il nodo che contiene la chiave **minima** di un BST si trova sempre sull'ultimo nodo del ramo che dalla radice percorre sempre rami sinistri, mentre quello che contiene la chiave **massima** sull'ultimo nodo del ramo della radice percorre sempre rami destri.
La **correttezza** di `BSTTreeMinimum` e `BSTTreeMaximum` si ricava immediatamente da questa osservazione e la loro **complessità** e' proporzionale all'altezza dell'albero, cioe' $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio.

### Alberi binari di ricerca: successore e predecessore
Problema:
- dato un nodo $x$ in un BST, trovare il nodo $y$, se esiste, tale che `y.key` e' il **successore immediato** di `x.key` nell'ordinamento naturale delle chiavi.

![[BSTSuccessorePredecessore.png]]

``` Pseudocodice
proc BSTTreeSuccessor(x) {
	if (x.right != nil)
		then return BSTTreeMinimum(x.right)
	y = x.p
	while ((y != nil) and (x = y.right)) {
		x = y
		y = y.p
	}
	return y
}
```

Andiamo per casi:
- se $x$ ha figlio destro $\rightarrow$ il successore immediato e' il **minimo** del sottoalbero destro di $x$;
- se $x$ non ha figlio destro $\rightarrow$ il successore immediato si trova tra i suoi antenati: bisogna risalire finche' la relazione padre-figlio e' di tipo padre-figlio **sinistro**

#### Correttezza e complessità di `BSTTreeSuccessor`
La **correttezza** di `BSTTreeSuccessor` e' evidente, considerato che implementa esattamente la strategia vista prima. La **complessità** di `BSTTreeSuccessor` e' proporzionale all'altezza dell'albero, cioe' $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio.

### Alberi binari di ricerca: inserimento
L'operazione di **inserimento** di un nodo e' quella che ci permette di costruire e modificare un dato BST. Stabiliamo che l'inserimento opera su un BST (possibilmente vuoto) denotato da $T$, tale che `T.root = nil` quando l'albero e' vuoto, e punta alla radice di $T$ in caso contrario. Si inserisce in $T$ un nodo $z$ in maniera che `z.key` contiene la chiave da inserire, `z.left = z.right = nil`; si noti che il nuovo nodo inserito finisce sempre per essere una nuova foglia di $T$.

``` Pseudocodice
proc BSTTreeInsert(T, z) {
	y = nil
	x = T.root
	while (x != nil) {
		y = x
		if (z.key <= x.key)	
			then x = x.left
			else x = x.right
	}
	z.p = y
	if (y != nil)
		then T.root  = z
	if ((y != nil) and (z.key <= y.key))
		then y.left = z
	if ((y  != nil) and (z.key > y.key))
		then y.right = z
}
```

#### Correttezza e complessità di `BSTTreeInsert`
Vogliamo mostrare che `BSTTreeInsert` e' **corretta**, cioe' se $T$ e' un BST e $T'$ e' il risultato di inserimento, allora $T'$ e' un BST. Se $T$ e' vuoto, il ciclo **while** non si segue e tra le istruzioni restanti si segue solo la prima, mettendo $z$ come radice di $T'$, che diventa un albero con un solo nodo e quindi corretto. Sia quindi $T$ un BST corretto non vuoto. Vogliamo mostrare che l'**invariante** del ciclo e': la posizione corretta di $z$ e' nel sottoalbero radicato in $x$ e $y$ ne mantiene il padre.
Questa e':
- vera all'inizio (**caso base**), perche' `x = T.root`;
- vera anche dopo l'i-esima esecuzione del ciclo (**caso induttivo**): assumendola vera dopo la ($i-1$)-esima esecuzione, $z$ viene confrontato con $x$ (che non e' ancora una foglia) e $x$ viene spostato correttamente, cosi' come $y$, mantenendo vera la proprieta'.
Alla fine del ciclo, `x = nil`, ed e' precisamente la posizione di $z$: poiche' si e' persa la relazione padre-figlio tra $y$ e $x$, le ultime due istruzioni recuperano questa relazione per ottenere la posizione corretta.

Osserviamo che e' l'operazione di inserimento che si occupa di decidere se le chiavi uguali vanno a sinistra o a destra. Tutte e due le scelte sono buone.
La **complessità** di `BSTTreeInsert` e' proporzionale all'alteza dell'albero e pertanto e' $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio.

### Alberi binari di ricerca: eliminazione
Eliminare un elemento da un BST dato e' un'operazione leggermente piu' difficile delle altre. 
Considerando un nodo $z$ qualsiasi:
- se $z$ e' foglia, si puo' eliminare semplicemente;
- se $z$ ha un solo figlio, allora l'operazione di eliminazione coincide con l'operazione di eliminazione in una lista;
- se $z$ ha due figli, allora dobbiamo trovare il modo di ricostruire l'albero dopo l'eliminazione.
Se $z$ ha due figli, il nodo che contiene la chiave successore di quella contenuta in $z$ certamente non ha mai figlio sinistro. Questa osservazione e' importante perche' ci permette di ridurre il caso difficile ad uno piu' semplice.

Procediamo cosi':
- se $z$ non ha figli sinistri, o e' una foglia, allora **trapiantiamo** il sotto-albero `z.right` al posto di $z$ (anche se `z.right` e' **nil**: caso senza figli).
- se $z$ ha figlio sinistro, ma non destro, allora **trapiantiamo** il sotto-albero `z.left` al posto di $z$ (`z.left` non puo' essere **nil**, altrimenti saremmo nel caso anteriore).
- se $z$ ha due figli, allora andiamo a prendere il suo successore immediato $y$, che si trova nel sotto-albero destro di $z$ e non ha **al piu' un figlio**.
  Il nodo $y$ va a prendere il posto di $z$ e se $y$ e' figlio immediato di $z$ allora il figlio destro di $z$ diventa il figlio destro di $y$ e il resto rimane invariato, altrimenti ($y$ e' nel sotto-albero destro di $z$ a non e' suo figlio immediato) allora prima rimpiazziamo $y$ con il suo figlio destro, e poi rimpiazziamo $z$ con $y$.

Ad esempio, eliminiamo 5:
![[BSTEliminazione.png]]

``` Pseudocodice
proc BSTTreeDelete(T, z) {
	if (z.left = nil)
		then BSTTranslpant(T, z, z.right)
	if ((z.left != nil) and (z.right = nil))
		then BSTTransplant(T, z, z.left)
	if ((z.left != nil) and (z.right != nil)) then {
		BSTTransplant(T, y, y.right)
		y.right = z.right
		y.right.p = y
	}
	BSTTransplant(T, z, y)
	y.left = z.left
	y.left.p = y
}

proc BSTTreeTransplant(T, u, v) {
	if (u.p = nil)
		then T.root = v
	if ((u.p != nil) and (u = u.p.left))
		then u.p.left = v
	if ((u.p != nil) and (u = u.p.right))
		then u.p.right = v
	if (v != nil)
		then v.p = u.p
}
```

#### Correttezza e complessità di `BSTTreeDelete`
La **correttezza** di `BSTTreeDelete` e' implicita nell'algoritmo stesso, la cui casistica, riflette i passi necessari. Quindi consideriamo `BSTTreeDelete` corretto per progettazione.
La **complessità** di `BSTTreeDelete`, che non contiene cicli, e' nuovamente $\Theta(n)$ nel caso peggiore e ==$\Theta(log(n))$== nel caso medio; questo si deve naturalmente alla presenza di una chiamata a `BSTTreeMinimum`.

## Alberi binari di ricerca e liste: confronto

|               | Liste       | BST (c. medio)   | BST (c.peggiore) |
| ------------- | ----------- | ---------------- | ---------------- |
| Inserimento   | $\Theta(1)$ | $\Theta(log(n))$ | $\Theta(n)$      |
| Cancellazione | $\Theta(1)$ | $\Theta(log(n))$ | $\Theta(n)$      |
| Visita        | $\Theta(n)$ | $\Theta(n)$      | $\Theta(n)$      |
| Ricerca       | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$      |
| Successore    | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$      |
| Precedente    | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$      |
| Massimo       | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$<br>  |
| Minimo        | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$<br>  |
## Conclusione
Gli alberi sono una struttura dati molto versatile. Gli alberi binari di ricerca sono una delle specializzazioni di questa struttura dati piu' comuni ed utilizzate. Oltre al loro uso naturale, queste sono utilizzate come base per strutture piu' complesse.