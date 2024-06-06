Sono una ==struttura dati astratta, parzialmente dinamica, parzialmente sparsa e non basata sull'ordinamento==.
La caratteristica principale di un insieme disgiunto e' che le operazioni ad esso associate sono tipicamente:
- `MakeSet` $\rightarrow$ costruisce un nuovo insieme disgiunto
- `Union` $\rightarrow$ unisce due insiemi disgiunti in uno solo
- `FindSet` $\rightarrow$ trova il rappresentante dell'insieme al quale appartiene l'elemento.
Ogni insieme e' dotato di un elemento rappresentativo.
Gli insiemi crescono solo in due modi:
- quando vengono creati (e contengono esattamente un elemento)
- quando vengono uniti due insiemi in uno solo che contiene gli elementi di entrambi

Immaginiamo di avere i seguenti insiemi: $S_1 = \{5, 12, 20\}, \space S_2 = \{7\}, \space S_3=\{13, 2\}$. Ognuno di essi puo' essere rappresentato da uno qualsiasi dei suoi elementi e cio' che dobbiamo mantenere e' l'informazione dell'insieme stesso, il quale non ha una vera struttura interna.
Indipendentemente dall'implementazione scelta, possiamo immaginare che $S$  abbia almeno un array che contiene tutte queste chiavi e, per ognuna di esse, un informazione aggiuntiva che ci permetta di ricostruire tutta la struttura.

La particolare scelta delle operazioni che si vogliono rendere disponibili influenza la struttura dati, che tendenzialmente e' ottimizzata per quelle operazioni. Se si volesse offrire un'altra operazione diversa da quelle originali, questo risulterebbe impossibile o troppo costoso.
Gli insiemi disgiunti hanno una applicazione fondamentale in uno degli algoritmi su grafi che vedremo piu' avanti (per il calcolo dell'albero di copertura minimo).

Consideriamo questo scenario: abbiamo una rete molto grande di criminali e tutti usano molti **alias** diversi tra loro. I nostri informatori riescono, di tanto in tanto, a scoprire che due **alias** sono la stessa persona. 

### Insiemi disgiunti: liste
Qual è la struttura dati ottima per mantenere questa informazione?
- L'implementazione piu' intuitiva per gestire $S$ passa attraverso l'uso delle liste collegate. L'elemento $S \in \mathcal{S}$ e' quindi una lista dotata degli attributi `S.head` (che punta al primo elemento) e `S.tail` (che punta all'ultimo elemento). Ogni elemento $x$ e' dotato di `x.next` e `x.head` che punta all'insieme $S$ che lo contiene.
  ![[InsiemiDisgiunti.png]]
  In questa versione l'informazione aggiuntiva che contiene ogni $S[i]$ e' un puntatore all'elemento $i$ in memoria, cioe' alla casella $x$ che contiene la chiave $i$. Lo chiamiamo per esempio `S[i].set`.
  ![[InsiemiDisgiunti2.png]]
  In questa maniera, implementare `MakeSet(x)` e' banale: crea un nuovo oggetto $S$ tale che `S.head = S.tail = x`. Se poi decidiamo che il rappresentante di ogni $S$ e' precisamente l'elemento puntato da `S.head`, allora implementare `FindSet(x)` e' altrettanto banale: dato $x$ cerchiamo prima `x.head` poi `x.head.head` per arrivare al suo rappresentante.
  Entrambe le operazioni costano $O(1)$. 
  Tra le sue caratteristiche:
	- E' commutativa
	- Viene eseguita a partire da elementi dei due insiemi uniti
	- Distrugge il secondo insieme 
In ogni operazione, gli elementi sono ipotizzati gia' **creati e nella memoria principale**.
Creare un nuovo insieme (`MakeSet()`) significa:
  - creare un oggetto $S$, creare un oggetto $x$ con la chiave che vogliamo e collegarli.
 La differenza con una lista 'normale' e' che l'oggetto 'lista' e' in memoria principale e non nello stack.
 L'operazione di `FindSet`, dato un oggetto $x$, che contiene una chiave della quale vogliamo conoscere il rappresentante, consiste nel seguire il puntatore `head` di $x$ per arrivare ad $S$ e poi nuovamente il puntatore `head`.
 L'operazione di `Union` di $x$ e $y$ consiste nel trovare $S_1$ ed $S_2$ e, se sono diversi, aggiornare $S_1.tail.next$ e $S_2.head$ e per ogni $z$ tale che $z.head = S_2$, impostare $z.head = S_1$. 

Vediamo il codice delle operazioni:
``` Pseudocodice
proc MakeSet(S, S, x, i) {
	S[i].set = x
	S.head = x
	S.tail = x
}

proc Union(x, y) {
	S1 = x.head
	S2 = y.head
	if (S1 != S2) then {
		S1.tail.next = S2.head
		z = S2.head
		while (z != nil) {
			z.head = S1
			z = z.next
		}
		S1.tail = S2.tail
	}
}

proc FindSet(x) {
	return x.head.head
}
```

Queste operazioni cono chiaramente **corrette** e **terminanti**.
Per calcolare la **complessità** dobbiamo fare ricorso ad un tipo leggermente diverso di analisi, chiamata **analisi ammortizzata**. 
- Si tratta di calcolare il costo **medio** di una operazione qualsiasi nel contesto di un gruppo di operazioni, piuttosto che il costo per operazioni


> Nell'implementazione con liste il caso peggiore si ha quando le operazioni iniziano con `n` `Makeset` seguite da `n - m Union` nel peggior ordine possibile
> 1. non ci sono `FindSet`
> 2. tutte le `Union` costano $n-1 \rightarrow m = 2 \cdot n-1$ 


Per poterlo calcolare, diciamo che $m$ denota il numero di operazioni qualsiasi fatte su $S$, ed $n \le m$ denota il numero di `MakeSet` tra le $m$ operazioni.
Consideriamo il caso peggiore:
- Non e' difficile definire una situazione in cui abbiamo gli oggetti $x_1, ..., x_n$ ed ognuno costituisce il suo proprio insieme. Quindi abbiamo $n$ operazioni `MakeSet` seguite da `n - 1 Union` in maniera che $m=2 \cdot n-1$. Spendiamo $\Theta(n)$ per generare gli insiemi. Nel caso peggiore, spendiamo 1 per la prima unione, 2 per la seconda e cosi' via fino all'ultima unione che costa $n$. Il totale e' $\Theta(n^2)$
  Le operazioni `FindSet` non contribuiscono a cambiare la struttura di $S$ e hanno un costo costante; pertanto le abbiamo escluse dall'analisi.

> Sapendo che $m$ operazioni di cui $n$ sono `MakeSet` e danno il caso peggiore nella situazione vista prima e che in quella situazione $2 \cdot n+1 = \Theta(n)$, il costo medio ammortizzato di un'operazione e' $\frac{\Theta n^2}{\Theta(n)} = \Theta(n)$ 

Qual'e' la differenza tra l'analisi ammortizzata e quella del caso medio?
- ==Nell'analisi ammortizzata non ci sono considerazioni probabilistiche==: si calcola il costo medio di ogni operazione nei casi ottimo, medio e pessimo, in maniera da tenere conto dell'influenza mutua tra operazioni.
Perche' non abbiamo avuto occasione di utilizzarla prima?
- Perche' altre strutture dati, come le liste, non sono tali che eseguire un operazione influenza in maniera evidente il costo di eseguire altre operazioni.

### Insiemi disgiunti: liste con unione pesata
Una prima strategia che possiamo usare per migliorare la situazione e' chiamata **unione pesata**.
Il principio sul quale si basa e' semplice:
- se manteniamo in ogni insieme $S$ anche il numero degli elementi dell'insieme, allora possiamo implementare `Union` in maniera che gli aggiornamenti dei puntatori si facciano sempre sull'insieme piu' piccolo

``` Pseudocodice
proc MakeSet(calS, S, x, i) {
	calS[i].set = x
	S.head = x
	S.tail = x
	S.rank = x
}

proc Union(x, y) {
	S1 = x.head
	S2 = y.head
	if (S1 != S2) then {
		if (S2.rank > S1.rank)
			then SwapVariable(S1, S2) 
		S1.tail.next = S2.head
		z = S2.head
		while (z != nil) {
			z.head = S1
			z = z.next
		}	
		S1.tail = S2.tail
		S1.rank = S1.rank + S2.rank
	}	
}
```

Concentriamoci sulla **complessità**.
Adesso il caso peggiore accade quando tutti gli $S$ sono di dimensione uguale, L'analisi ammortizzata ci dira' che esiste un risparmio in media di tempo.
Mettiamoci nelle stesse condizioni di prima: $m$ operazioni generiche di cui $n$ `MakeSet`. Come nel caso precedente, ci dovremmo fermare quando avremo raggiunto un solo insieme con tutti gli elementi. Le operazioni `FindSet` vengono inizialmente escluse dal ragionamento.
Dati $n$ insiemi tutti di un solo elemento, la situazione peggiore si verifica effettuando $\frac{n}{2}$ unioni: infatti, se non facessimo cosi', arriveremo ad avere un insieme con $n$ elementi dopo solamente $n-1$ passi e non avremmo la situazione peggiore.
Se, per comodita', ipotizziamo $n = 2^k$ per qualche $k$, allora possiamo proseguire ragionando nello stesso modo: $\frac{n}{2}$ unioni la prima volta, seguite da $\frac{n}{4}$ unioni e cosi' via, precisamente $log(n)$ volte.

Quanto e' il costo totale delle $m$ operazioni?
- Ogni unione costa: 1 per la prima volta, 2 per la seconda, 4 per la terza e cosi' via.
- Il costo totale di tutte le unioni che possiamo fare prima di arrivare all'insieme con tutti gli elementi e' $\Theta(n \cdot log(n))$. 
- Nel caso peggiore (forzare che le $m$ operazioni siano `n MakeSet` seguite da tutte le `Union` possibili) ci da un costo totale di $\Theta(n \cdot log(n))$.
- In questo caso, aggiungere qualche `FindSet` nel gruppo di $m$ operazioni puo' solo migliorare la complessita', ed e' per questo che le escludiamo dall'analisi del caso peggiore.
Possiamo migliorare queste prestazioni?
- La strategia implementativa che ci permette una ulteriore miglioria consiste in tre passi:
	1. cambiare la rappresentazione
	2. adattare l'unione pesata alla nuova rappresentazione
	3. modificare l'operazione `FindSet` per renderla **attiva**

### Insiemi disgiunti: foreste di alberi
Il modo piu' efficiente per trattare gli insiemi disgiunti sono le **foreste di alberi**.

La rappresentazione e' basata in **alberi** piuttosto che liste. Un nodo $x$ contiene le seguenti informazioni:
- `x.p` (il padre)
- `x.rank` (un limite superiore all'altezza del sotto-albero radicato in `x`)
Gli alberi sono `k-ari` e formano una **foresta** di $\mathcal{S}$.
L'operazione di `MakeSet` e' la stessa:
- si crea un nuovo albero di altezza massima 0 tale che il padre dell'unico nodo $x$ e' $x$ stesso.
==Attenzione: questi alberi sono liste particolari e non vanno confusi con alberi e grafi.==

Un nodo di un albero `k-ario` **non ha, in generale, nessun puntatore ai figli**. Infatti, non abbiamo un limite superiore a quanti figli posso avere e non ci interessa agli scopi di questa struttura dati.
Il rango **non** e' la misura dell'altezza ma un suo limite superiore. Questo significa che un albero in questa struttura puo' avere altezza $h$ e rango qualsiasi ($\ge h$).

![[InsiemiDisgiunti3.png]]

L'operazione di unione, in due fasi, consiste nel trovare rappresentanti degli elementi utilizzati come indici; se le due radici sono $x$ e $y$, si sceglie quello il cui rango sia inferiore e si aggiorna **solo il padre**, rendendolo uguale all'altro elemento. Con il criterio **unione per rango** (il corrispondente dell'unione pesata nella versione con le liste), il rango dell'insieme risultante cambia **solo se i ranghi dei due componenti erano uguali** e rimane invariato negli altri casi.
L'operazione `FindSet` diventa attiva. Non solo si restituisce il rappresentante, ma, mentre scorre i puntatori verso l'alto alla sua ricerca, li aggiorna **appiattendo** il ramo al quale appartiene. Chiamiamo **compressione del percorso questa strategia**.

Un'esempio di `FindSet`; a destra, l'effetto di chiamare `FindSet(h)`:
![[InsiemiDisgiunti4.png]]

``` Pseudocodice
proc MakeSet(x) {
	x.p = x
	x.rank = 0
}

proc Union(x, y) {
	x = FindSet(x)
	y = FindSet(y)
	if (x.rank > y.rank)
		then y.p = x
	if (x.rank <= y.rank) then {
		x.p = y
		if (x.rank = y.rank)
			then y.rank = y.rank + 1
	}
}

proc FindSet(x) {
	if (x != x.p)
		then x.p = FindSet(x.p)
	return x.p
}
```

Nuovamente, **correttezza** e **terminazione** di queste procedure non sono in discussione.
Il calcolo della **complessità** di $m$ operazioni in questa implementazione e' molto difficile.
La ragione per quale l'unione per rango sommata alla compressione del percorso migliora le prestazioni globali sono chiare. In sostanza, l'unione effettua sempre al massimo un aggiornamento sui puntatori. L'operazione `FindSet` aggiorna un certo numero di puntatori, ma questi, una volta aggiornati, non vengono toccati mai piu' e il prossimo `FindSet` su elementi del percorso che e' gia' stato compresso costera' un tempo costante. 
Sia $\alpha$ una certa funzione da $\mathbb{N}$ a $\mathbb{N}$ che cresce approssimativamente come l'inverso della funzione di Ackermann (cioe' cresce in maniera **estremamente** lenta). Nel caso concreto in questione abbiamo che $\alpha(n)$ e' minore o uguale a 4 per $n \le 10^{80}$.
Una corretta analisi di $m$ operazioni darebbe che il costo totale e' $O(m \cdot \alpha(n))$, che, in ogni situazione pratica, e' lo stesso che $O(m)$. 

## Conclusione
Gli insiemi disgiunti sono un esempio di struttura dati non intuitiva. E' un esempio di struttura dati che fornisce idee non banali a chi la studia, che possono essere riutilizzate in altri contesti. 