## Problemi sui grafi
Alcuni problemi che abbiamo risolto sui grafi:
- Calcolare il percorso minimo tra due vertici di un grafo non pesato.
- Calcolare il percorso minimo tra due vertici di un grafo pesato.
- Stabilire la presenza di un ciclo di peso negativo in un grafo diretto pesato.
- Stabilire il numero di componenti fortemente connesse in un grafo diretto.
- ...

Cosa accomuna tutti questi problemi? 
- Il fatto di poter essere risolti con un algoritmo che, nel caso peggiore, termina in un tempo che e' **polinomico** nella dimensione di input.
Questo vale per **tutti** i problemi che abbiamo visto, non solo per quelli sui grafi.

## Problemi decisionali
Per rendere piu' semplice il trattamento generale dei problemi, una semplificazione che si fa sempre e' quella di considerare solo i problemi decisionali.
Un problema e' **decisionale** se il suo output e' semplicemente {si, no}.
I problemi della lista di prima hanno un loro corrispondente decisionale:
- Stabilire se il peso del percorso minimo tra due vertici di un grafo non pesato e' inferiore a $k$.
- Stabilire se il peso del percorso minimo tra due vertici di un grafo pesato e' inferiore a $k$.
- Stabilire se la presenza di un ciclo di peso negativo in un grafo diretto pesato.
- Stabilire se il numero di componenti fortemente connesse in un grado diretto e' inferiore a k
- ...

Nella teoria della complessità (e anche in quella della calcolabilita') ci occupiamo di versioni decisionali di problemi, in un contesto in cui, in generale, la difficolta' di risolvere la versione non decisionale di un problema e' almeno tanto alta quanto quella del suo corrispondente decisionale.
Supponiamo che stabilire qual'e' il percorso minimo tra due vertici in un grafo indiretto non pesato abbia un certo costo: 
- stabilire se questo percorso minimo ha costo inferiore a un certo $k$ ha lo stesso cost, perche' basta calcolare il percorso e confrontare il suo peso con $k$.
Ragionando al contrario, un risultato **negativo** di costo sulla versione decisionale di un problema ha effetto immediato sulla versione non decisionale, che presenta almeno lo stesso costo.

## La classe P
Nel contesto dei soli problemi decisionali, emerge in forma naturale una classe, cioe' un insieme, ben definito di problemi: 
$$
P = \{\text{problemi che possono essere risolti in tempo polinomiale}\}
$$
Nel dare questa definizione stiamo utilizzando un certo grado di approssimazione rispetto al concetto di 'poter essere risolto', intendendo, 'da un algoritmo, e su un modello di computazione ragionevole'.
Appartengono tutti i problemi alla classe P?

### Il problema della $k$-clique
Consideriamo un grafo indiretto non pesato $G = (V, E)$ e definiamo $k$-**clique** un sottoinsieme dei vertici di cardinalita' $k$ tale che, per ogni coppia $u, v$ in questo sottoinsieme, esiste l'arco $(u, v)$.
Osserviamo che le 1-clique sono triviali (tutti i vertici sono una 1-clique) e che lo stesso vale per le 2clique.
Non tutte le triple di vertici pero' sono 3-clique, e in generale e' sempre piu' difficile trovare una $k$-clique in un grafo man mano che $k$ cresce.
Possiamo definire il problema di trovare la massima clique n un grafo indiretto non pesato, cosi' come la sua versione decisionale:
- stabilire se un grafo presenta un $k$-clique o superiore, per un dato $k$.
Immaginiamo che $G$ sia rappresentato come una matrice di adiacenza.

``` Pseudocodice
proc kClique(G, k) {
	for (A subseteq G.V, |A| = k)
		Clique = true
		for (u, v in A)
			if ((u, v) notin G.E)
				Clique = false
			if (Clique = true)
				return true
	return false
}
```

La **correttezza** di `kClique()` e' immediata.
**Complessità**:
- L'input e' dato da due elementi, il grafo e il numero $k$
- L'input puo' crescere solo se cresce la dimensione del grafo, dunque la dimensione dell'input **e' la dimensione del grafo**.
- Dati $|V|$ vertici, abbiamo nel caso peggiore $\frac{|V|!}{(|V|-k)!k!} = \binom{|V|}{k}$ $k$-uple di vertici.
- Succede che $O \binom{|V|}{k} = O (2^{|V|})$ $\rightarrow$ complessita' **esponenziale**.

Avere un algoritmo stabilisce che il problema e' risolvibile.
Usando questa informazione, **non possiamo dire** che il problema della $k$-clique stia nella classe P.

## La classe NP
Se avessimo in mano un insieme di vertici di $G$, quanto sarebbe difficile stabilire che si tratta di una $k$-clique?
In altre parole, se risolvere il problema e' troppo difficile, quanto difficile e' stabilire che una certa soluzione e' in effetti una soluzione?
Osserviamo che se quest'ultimo sottoproblema fosse gia' molto difficile, non avremo speranze di risolvere efficientemente il problema decisionale.

Nuovamente nel contesto dei problemi decisionali, l'osservazione precedente fa emergere un'altra classe ben definita di problemi:
$$
NP = \{ \text{problemi la cui soluzione puo' essere controllata in tempo polinomiale} \}
$$
Dunque NP nella nomenclatura classica non significa 'non polinomiale'.
Osserviamo che: 
$$ 
P \subseteq NP 
$$
perche' se posso efficientemente trovare soluzione ad un problema, evidentemente posso efficientemente controllare che una soluzione data sia una soluzione.

### Il problema delle $k$-clique
Il problema delle $k$-clique e' un esempio perfetto di problema nella classe NP.
Supponendo di avere in mano un insieme di vertici in un grafo dato, e' immediato stabilire se questo insieme e' in effetti di cardinalita' $k$ e, se e' cosi', stabilire se tutte le coppie di vertici dell'insieme sono in effetti collegate tra loro da un arco.

``` Pseudocodice
proc kCliqueCheck(G, A, k) {
	if (|A| != k)
		return false
	Clique = true
	for (u, v in A)
		if ((u, v) notin G.E)
			Clique = false
	if (Clique = true)
		return true
	return false
}
```

Nel caso peggiore il costo di eseguire `kCliqueCheck()` (che e' **corretto** per la stessa ragione di `kClique()`) e' di $O(k^2)$, al crescere dell'input (che cresce sempre con la dimensione del grafo), la **complessità** dell'algoritmo e' **polinomiale** in essa.
Possiamo dire che $k$-clique si trova nella classe NP.
La relazione tra la classe P e NP e' ignota; non sappiamo se esiste oppure no un problema che si trova nella classe NP e certamente non si trova nella classe P ($k$-clique **non** e' un esempio: non conosciamo un algoritmo polinomiale per risolverlo, ma non abbiamo dimostrato che questo non esiste.)

## Oltre la classe NP
Forse potremmo pensare che tutti i problemi si trovano, almeno, nella classe NP; cioe' che se pur per qualche problema **trovare** la soluzione e' difficile, deve succedere che per tutti i problemi verificarla e' facile. ==Invece non e' cosi'==.
Nono solo non tutti i problemi che stanno nella classe NP, ma sono infinitamente di piu' di quelli che non vi appartengono di quelli che vi appartengono.
Sono infinitamente di piu' i problemi che non appartengono a nessuna classe di complessita', perche' sono irrisolvibili algoritmicamente.
Concentriamoci su un problema che non appartiene alla classe NP.

### Il problema della geografia generalizzata
Dato un grafo $G = (V, E)$, diretto, non pesato ed un vertice $v \in V$ e' possibile definire un gioco come segue:
- due giocatori (I e II) si alternano al gioco, che comincia I, e consiste, ad ogni turno, nello scegliere un vertice non ancora usato e connesso a quello attuale; Il giocatore che rimane senza mosse possibili perde e l'altro vince.
Il problema interessante e' trovare una **strategia vincente** per un giocatore.
Cos'e' la strategia?
- E' una sequenza di mosse, cioe' una sequenza di vertici.
- Una strategia e' vincente se, seguendola, il giocatore vince **qualunque cosa faccia l'altro**.
Per questo problema conviene supporre che $G$ sia rappresentato con liste di adiacenza.

``` Pseudocodice
proc HasIWinningStrategy(G, p, v) {
	if (p = I)
		Wins = false
		for (u in Adj[v])
			if (u.mark = false)
				u.mark = true
				Wins = Wins or HasIWinningStrategy(G, II, u)
				u.mark = false
		return Wins
	Wins = true
	for (u in Adj[v])
		if (u.marl = false)
			u.mark = true
			Wins = Wins and HasIWinningStrategy(G, I, u)
			u.mark = false
	return Wins
}
```

L'algoritmo `HasIWinningStrategy()` funziona in modo tale che, se chiamato un certo vertice $v$ di un certo grafo $G$ con $p=I$, stabilisce se $I$ ha una strategia vincente su $G$ a partire da $v$.
**Correttezza**:
- **Invariante**: al termine di ogni chiamata ricorsiva sul grafo $G$, giocatore $P$, vertice $v$, `HasIWinningStrategy()` restituisce **true** se e solo se, $p=I$ e $p$ ha una strategia vincente su $G$ a partire da $v$, oppure $p=II$ e $p$ non ha una strategia vincente su $G$ a partire da $v$. 
- **Caso base**: 
	- non ci sono chiamate ricorsive.
	- Questo avviene se non ci sono i successori del vertice corrente oppure questi sono tutti marcati;
	- se $p=I$ restituisce **false** perche' $I$ non puo' vincere passando dal vertice corrente $v$;
	- se $p=II$ si restituisce **true** perche' $I$ ha vinto
	- questo e' in linea con l'invariante $\rightarrow$ $II$ non ha una strategia vincente
- **Caso induttivo** (alla $j$-esima chiamata ricorsiva):
	- Se $p=I$ si effettua una chiamata su ogni successore non marcato, marcandolo, e si preleva il risultato;
	- questo viene da una chiamata con $p=II$, dunque se restituisce **true**, per ipotesi induttiva, $II$ non puo' vincere partendo da quel successore.
	- Se anche **una sola** di queste chiamate ha successo, allora $I$ ha una strategia vincente e dunque si restituisce la loro disgiunzione logica.
Quando $P=II$ si procede in maniera simmetrica: si restituisce **true** se **tutte** le chiamate ricorsive hanno successo.

**Complessità**:
- dipende dal numero totale di cammini semplici che esistono in un grafo diretto $G = (V, E)$.
- questo e' limitato dall'alto da $|V|!$ e dunque il tempo di esecuzione e' esponenziale: $O(2^{|V|})$.

Ci troviamo nello stesso problema di prima: il problema non e' nella classe $P$.
Possiamo dire, come nel caso precedente, che appartiene alla classe $NP$? 
Per questo dovremmo almeno essere capaci di dimostrare che una soluzione a questo problema e', in se', polinomiale.

Come denotiamo tale soluzione?
- Essa deve avere una serie di suggerimenti del tipo 'percorso fino qui - prossima mossa'.
- Deve cioe' essere un manuale che $I$ segue alla lettera e vince sempre.
- Pertanto, essa stessa e' esponenziale per natura, perche' esiste un numero esponenziale di regole che potenzialmente potrebbe contenere.

## La classe PSPACE
Il problema della geografia generalizzata si trova nella classe di quei problemi la cui soluzione puo' essere trovata/verificata usando al massimo **spazio polinomiale** per la computazione.
Questa classe include (non sappiamo se strettamente) la classe $NP$
$$
PSPACE = \{ \text{problemi la cui soluzione puo' essere trovata/verificata in spazio polinomiale} \}
$$
Quando misuriamo lo spazio, invece del tempo, distinguere tra trovare e verificare una soluzione non e' piu' rilevante.

## Lo zoo della complessità e della calcolabilità
Esistono decine di classi di problemi da studiare; alcune di queste sono incluse in $P$, tutte le altre sono incomparabili con $P$ oppure che non la contengono.
Nella maggioranza dei casi, le relazioni esatte sono ignote, si conoscono solo le relazioni di contenimento non strette. Classi di complessità sempre superiori portano a classi di problemi la cui stessa risolvibilità inizia a vacillare per così dire, e nalmente si arriva ad una classicazione anche della calcolabilità di un
problema, cioè della nostra possibilità di risolverlo pur senza limiti di tempo/spazio di computazione. Sorprende sapere che la grandissima maggioranza dei problemi **non può essere risolta** algoritmicamente.

![[complessita_calcolabilita.png]]