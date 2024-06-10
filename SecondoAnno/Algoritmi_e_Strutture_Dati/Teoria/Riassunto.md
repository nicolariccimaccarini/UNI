## Terminazione, Correttezza, Complessità
- **correttezza**: l'algoritmo restituisce sempre la risposta corretta
- **completezza**: ogni risposta corretta è restituita dall'algoritmo
- **terminazione**: l'algoritmo termina sempre (non entra in loop infiniti)
- **complessità**: l'algoritmo termina in un tempo ragionevole

### `InsertionSort` - caso iterativo
- **invariante** $\rightarrow$ $A[1, \ldots, j-1]$ e' sempre ordinato in maniera non decrescente (invariante del ciclo piu' esterno con inizialmente $j=2$)

### `RecursiveBinarySearch` - caso ricorsivo
- **invariante induttiva**: dopo ogni chiamata ricorsiva, se $k$ e' in $A$, allora si trova in $A[low, \ldots, high]$

### Notazione asintotica
- $O()$
	- $f(n)$ e' limitata da $g(n)$ $\leftrightarrow \exists \space c > 0 : \space 0 \le f(n) \le c \cdot g(n) \qquad \text{con} \space n \ge n_0$ 
- $\Omega(n)$
	- $f(n)$ e' limitata dal basso da $g(n)$ $\leftrightarrow \exists \space c > 0 : \space 0 \le  c \cdot g(n) \le f(n) \qquad \text{con} \space n \ge n_0$ 
- $\Theta()$
	- $f(n)$ e' dello stesso ordine di $g(n)$ $\leftrightarrow \exists \space c_1, c_2>0: \space 0 \le c_1 \cdot g(n) \le f(n) \le c_2 \cdot g(n) \qquad \text{con} \space n \ge n_0$ 
- $o()$
	- $$ f(n)=o(g(n)) \leftrightarrow \forall \space c>0: \space 
	  \left\{\begin{align} 
	  & 0 \le f(n) \le c \cdot g(n) \\
	  &\lim_{n \rightarrow \infty} \frac{f(n)}{g(n)} = 0
	  \end{align}\right. $$
- $\omega()$
	- $$ f(n)=\omega(g(n)) \leftrightarrow \forall \space c>0: \space  
	  \left\{\begin{align}
	  & 0 \le c \cdot g(n) \le f(n) \\
	  & \lim_{n \rightarrow \infty} \frac{f(n)}{g(n)} = \infty
	  \end{align}\right.
	  $$
---
## Ricorrenze

### Sviluppo in serie
- $T(n)$ = numero delle foglie + somme dei costi di ogni livello

### Master Theorem
Dimostrazione:
$$
T(n) = 
\left\{\begin{align}
& \Theta(n^{\log_b(a)}) \qquad &&\text{se} \quad n^{\log_b(a)} \space \text{ e' di ordine maggiore di } f(n)  \\
& \Theta(n^{\log_b(a)} \cdot \log^{k+1}(n)) &&\text{se} \quad  n^{\log_b(a)} \cdot \log^{k+1}(n) \space \text{ e' dello stesso ordine di } f(n) \\
& \Theta(f(n)) &&\text{se} \quad f(n) \text{ e' di ordine maggiore di } n^{\log_b(a)} \text{ e se esiste }  \\ 
&\space && c>1 : a \cdot f(\frac{a}{b}) \le c \cdot f(n) \quad \text{per ogni } n \ge n_0
\end{align}\right.
$$

**Dimostrazione Master Theorem**:
Si basa sull'analisi del comportamento della ricorrenza attraverso l'uso di tecniche come l'albero di ricorsione o l'induzione 
1. **Albero di ricorsione**: si costruisce un albero di ricorsione in cui ogni nodo rappresenta un sottoproblema. Il costo di ogni livello dell'albero viene analizzato e si sommano i costi di tutti i livelli per determinare il costo totale
	- Esempio: $T(n) = 2T(\frac{n}{2}) + n$
		- $a = 2, b = 2$ e $f(n) = n$
		- $n^{log_22} = n \rightarrow$ siamo nel caso 2
		- $T(n)) = \Theta(n \cdot \log(n))$
1. **Induzione**: si assume una soluzione di forma $\Theta$ e si utilizza l'induzione per verificare che la soluzione ipotizzata soddisfi la ricorrenza data
	- Esempio: $T(n) = T(\frac{n}{2}) + 1$
		- $a=1, b=2$ e $f(n)=1$
		- $n^{log_21} = n^0 = 1 \rightarrow$ siamo nel caso 2
		- $T(n) = \Theta(\log(n))$

---
## Algoritmi di ordinamento

### Selection Sort
- E' un algoritmo elementare per l'ordinamento come `InsertionSort`

### Merge Sort
- E' un algoritmo non elementare per l'ordinamento
- Sia `Merge()` che `MergeSort()` non sono in place $\rightarrow$ piu' grande e' l'input e maggiore e' la quantita' di spazio utilizzata da L e R, pertanto non e' costante.
- `Merge()`
	- **invariante**: $A[p, \ldots, k-1]$ contiene i $k-p$ elementi piu' piccoli di $L[1, \ldots, n_1]$ e $R[1, \ldots, n_2]$
	- **complessità**: $\Theta(n)$
- `MergeSort()`
	- **invariante induttiva**: dopo ogni chiamata ricorsiva `MergeSort(a, p, r)`, $A[1, \ldots, r]$ e' ordinato
	- **complessità**: $\Theta(n \cdot \log(n))$

### Quick Sort
- E' un algoritmo non elementare per l'ordinamento
- `Partition()`
	- **invariante**: all'inizio di ogni iterazione, per ogni indice $k$
		- $A[k] \le x$ se $p \le k \le i$
		- $A[k] > x$ se $i+1 \le k \le j-1$
		- $A[k] = x$ se $k=r$
		- dove $x$ e' il pivot
	- **complessità**: $\Theta(n)$
- `QuickSort()`
	- **invariante induttiva**: al termine di ogni chiamata ricorsiva con indici $p$ ed $r$ $A[p, \ldots, r]$
	- **complessità**:
		- $\Theta(n^2)$ nel caso peggiore (partizione sbilanciata)
		- $\Theta(n \cdot \log(n))$ nel caso medio e migliore

### Randomized Quick Sort
- Per minimizzare la possibilita' di avere partizioni sbilanciate

### Counting Sort
- **invariante**: 
	- al $j$-esimo passo dell'ultimo ciclo for (con $j$ che va da $n$ a 1), $C[A[j]]$ e' la posizione corrente di $A[j]$ in $B$
- **complessità**: $\Theta(n + k)$

### Radix Sort
- **invariante**: dopo l'$i$-esima esecuzione del ciclo piu' interno, gli elementi formati dalle ultime $i$ colonne sono correttamente ordinati
- **complessità**: $\Theta(d \cdot f(n))$ 

---
## Tassonomia
- Statica o dinamica:
	- **statica** $\rightarrow$ se la dimensione e' fissata a priori
	- **dinamica** $\rightarrow$ se la dimensione puo' variare durante l'esecuzione
- compatta o sparsa:
	- **sparsa** $\rightarrow$ gli elementi sono virtualmente vicini ma fisicamente non sappiamo dove si trovano
	- **compatta** $\rightarrow$ gli elementi si trovano in posizioni fisiche di memoria contigue
- basata o non basata su ordinamento:
	- **basata su ordinamento** $\rightarrow$ se gli elementi sono ordinati secondo una chiave
	- **non basata su ordinamento** $\rightarrow$ se non c'e' alcuna relazione con una chiave

--- 
## Liste
- Struttura dati dinamica, sparsa e non basata sull'ordinamento
- E' una struttura dati concreta (o fondamentale)
- Ogni elemento ha un campo dati e un campo puntatore al successivo
- Associamo le operazioni di 
	- inserimento $\rightarrow$ $\Theta(1)$
	- cancellazione $\rightarrow$ $\Theta(1)$
	- ricerca $\rightarrow$ $\Theta(n)$ (nel caso peggiore)

---
## Pile
- Struttura dati astratta, dinamica, non basata sull'ordinamento, sparsa (basata su liste) o compatta (basata su array)
- Implementa la politica LIFO
- Accesso agli elementi vincolato ad una politica per far risparmiare dettagli implementativi, assicurando un certo ordine di inserimento ed estrazione

### Pile su Array
- Si puo' pensare come un oggetto con i suoi relativi metodi
- Sia $S$ array di interi $\rightarrow$ dotato di parametri `S.top` e `S.max`
- Le operazioni di inserimento di un elemento nelle pile prendono il nome di *Push* e *Pop*.

### Pile su liste
- L'operazione di inserimento in testa di una lista e' la *Push* e la *Pop* e' una semplificazione dell'operazione di delete.

--- 
## Code
- Struttura dati astratta, dinamica, non basata sull'ordinamento, sparsa (basata su liste) o compatta (basata su array)
- Implementa la politica FIFO
- Accesso agli elementi vincolato ad una politica per far risparmiare dettagli implementativi, assicurando un certo ordine di inserimento ed estrazione
- Permette di accedere ai dati attraverso inserimento ed eliminazione che prendono normalmente i nomi di `Enqueue` e `Dequeue`.

### Code su Array
- Usate per implementare uno stack
- Sia $Q$ una coda $\rightarrow$ dotata di parametri `Q.head` e `Q.tail` inizializzati entrambi a $1$

### Code su liste
- Campo `Q.tail` in aggiunta a `Q.head` per implementare `Enqueue` chiamando l'inserimento in una lista e `Dequeue` chiamando l'eliminazione di un elemento in una lista

---
## Heap
- Struttura dati astratta, parzialmente basata sull'ordimanento e non necessariamente compatta 
- Caratteristica principale $\rightarrow$ mantiene le chiavi semi-ordinate

### Heap binarie su array
- Distinguiamo due tipi di heap: 
	- max-heap $\rightarrow$ il massimo elemento si trova nella radice
		- **Complessità** $\rightarrow$ `BuildMaxHeap` $\Theta(n)$ + `MaxHeapify` $\Theta(\log(n))$ = $\Theta(n \cdot \log(n))$
	- min-heap $\rightarrow$ il minimo elemento si trova nella radice
		- **Correttezza**: se i figli sono una min-heap, dopo la chiamata ricorsiva il il padre con i figli è una min-heap
		- **Complessità** min-heap $\rightarrow$ $h = \Theta(log(n))$
		- **Caso sbilanciato** $\rightarrow$ situazione in cui uno dei sottoalberi di un nodo e' significativamente piu' grande dell'altro. 
			- il peggior sbilanciamento possibile e' $\frac{2}{3}$. Questo caso succede quando la heap sulla quale la procedura e' chiamata tende ad essere sbilanciata, forzando piu' chiamate ricorsive. Risolvendo la ricorrenza $T(n) \le T(\frac{2}{3}n) + \Theta(1)$ (Master Theorem, caso 2) si ottiene che $T(n) = \Theta(log(n))$. 
- In generale una heap di altezza $h$ contiene **al minimo** $2^h$ elementi ed **al massimo** $2^{h+1} -1$ elementi

---
## Code di priorità
- Struttura dati astratta, basata sull'ordinamento e necessariamente compatta.
- Si possono costruire basandosi sulle min-heap
- Associa ad ogni chiave la priorita' che serve $\rightarrow$ estrae l'elemento di priorita' piu' bassa

### Code di priorita' su heap binarie
- Correttezza $\rightarrow$ immediata
- Complessità $\rightarrow$ $\Theta(log(n))$ nel caso pessimo

### Code di priorita' su array
- Implementate direttamente su array senza passare dalle heap
- Complessita' non comparabile $\rightarrow$ migliore in qualche caso e peggiore in qualche caso
- Conviene esplicitare le chiavi associate
- Sia $Q$ un array che contiene $\rightarrow$ $i$, `Q[i]` e `Q[i].empty`
	- costo inizializzazione $\rightarrow$ $\Theta(n)$
- Correttezza e terminazione $\rightarrow$ immediate
- Estrazione del minimo $\rightarrow$ $O(n)$
- Decremento $\rightarrow$ $\Theta(1)$

---
## Tabelle Hash
- Struttura dati astratta, dinamica, parzialmente compatta e non basata su ordinamento
- Uso: dato un numero di oggetti **relativamente piccolo**, ognuno dei quali è denotato da una chiave con universo **relativamente grande**, si vuole memorizzare in maniera dinamica gli oggetti e implementare operazioni di inserimento, cancellazione, ricerca
- La tabella hash ad accesso diretto (uso di array) ha ottime complessità ($\Theta(1)$ per tutte le operazioni) ma solo se l'universo è piccolo $\rightarrow$ si cercano soluzioni migliori
- $T$ $\rightarrow$ array di $m$ posizioni
	- problema: memorizzare una chiave $k > m$ 
		- soluzione: funzione di hash che indirizza l'elemento $k$ alla posizione $h(k)$
	- problema: conflitti $\rightarrow$ quando $k_1 \ne k_2$ ma $h(k_1) = h(k_2)$
		- soluzioni: chaining (in ogni cella della tabella c'è un puntatore alla testa di una lista) e open hashing (provare più posizioni finché non se ne trova una libera o se la tabella è piena) 

### Tabelle hash con chaining
- Tecnica che risolve i conflitti di una tabella hash usando un puntatore alla testa di una lista in ogni cella (testa memorizzata in `T[h(k)]`, quando è vuota punta a `nil`)
- Operazioni:
	- `HashInsert` $\rightarrow$ complessita' $\Theta(1)$
	- `HashSearch` $\rightarrow$ complessita' $\Theta(n)$ $\leftarrow$ `HashDelete` 

### Funzioni di hash con il chaining
Perché una funzione di hash sia buona, deve distribuire le chiavi in maniera uniforme sulle $m$ posizioni della tabella. Per farlo esistono diversi metodi:
1. **Divisione**: $h(k) = (k \cdot mod(m)) + 1)$ $\rightarrow$ resto della divisione tra $k$ e $m+1$
	- chiavi naturali
	- $m$ numero primo
	- $m$ lontano da una potenza di 2
2. **Moltiplicazione**: $h(k) = \lfloor m \cdot (k \cdot A - \lfloor k \cdot A) \rfloor ) \rfloor +1$ 
	- chiavi naturali
	- $m = 2^p$ per qualche $p$ $\rightarrow$ $h(k)$ dipende dagli ultimi $p$ bit di $k$ 
	- si sceglie $A$ costante tra 0 e 1
3. **Addizione**: $((a_1 \cdot B + a_2) \cdot B + a_3) \cdot B + ... a_d$  
	- chiavi sono stringhe o insiemi di stringhe o oggetti complessi
	- $m$ numero primo
	- $m$ lontano da potenze di 2
	- problema della dimensione $\rightarrow$ risolvo calcolando il modulo (non si memorizza mai il $k$)
	- $d$ = numero di caratteri della stringa
	- $B$ = cardinalità dell'alfabeto (es. 26 per alfabeto inglese)
	- $a_1a_2a_3...a_d$ = stringa

### Tabelle Hash con Open Hashing
- Tecnica che risolve i conflitti di una tabella hash provocando piu' posizioni finche' non se ne trova una libera o se la tabella e' piena.
- Si ipotizza che il numero di chiavi e' minore al numero di celle della tabella
	- $n \le m$
- **Caratteristiche**:
	- elimina le liste $\rightarrow$ una tabella hash di $m$ elementi potra' tenere al massimo $m$ elementi
	- eliminando le liste non si potrà avere la funzione di cancellazione
- Partendo da una funzione di hash qualsiasi si verifica se la cella è libera:
	- se è libera si inserisce la chiave
	- se non è libera si cerca una nuova cella
- Sequenza di probing $\rightarrow$ sequenza di tentativi di inserimento di una chiave in una tabella hash

---
## Insiemi disgiunti
- Struttura dati astratta, parzialmente dinamica, parzialmente sparsa e non basata sull'ordinamento
- La caratteristica principale di un insieme disgiunto e' che le operazioni ad esso associate sono tipicamente:
	- `MakeSet` $\rightarrow$ costruisce un nuovo insieme disgiunto
	- `Union` $\rightarrow$ unisce due insiemi disgiunti in uno solo
	- `FindSet` $\rightarrow$ trova il rappresentante dell'insieme al quale appartiene l'elemento.
- Ogni insieme e' dotato di un elemento rappresentativo.
- Gli insiemi crescono solo in due modi:
	- quando vengono creati (e contengono esattamente un elemento)
	- quando vengono uniti due insiemi in uno solo che contiene gli elementi di entrambi

### Insiemi disgiunti: liste
- Uso delle liste collegate per gestire $S$($\mathcal{S}$)
- $S \in \mathcal{S}$ e' una lista con:
	- `S.head` $\rightarrow$ punta al primo elemento
	- `S.tail` $\rightarrow$ punta all'ultimo elemento
- Ogni elemento $x$ e' dotato di:
	- `x.next` $\rightarrow$ punta all'elemento successivo
	- `x.head` $\rightarrow$ punta all'insieme $S$ che lo contiene 
	- `x.key` $\rightarrow$ chiave dell'elemento
- L'informazione aggiuntiva che contiene ogni $S[i]$ e' un puntatore all'elemento $i$ in memoria, cioe' alla casella $x$ che contiene la chiave $i$. Lo chiamiamo per esempio `calS[i].set`.
- `MakeSet(x)` $\rightarrow$ crea nuovo oggetto $S$ (`S.head = S.tail = x`) quindi costa $O(1)$
- Rappresentante di ogni $S$ $\rightarrow$ `S.head`
- `FindSet(x)` $\rightarrow$ dato $x$, si cerca prima `x.head` poi `x.head.head` (restituisce il rappresentante dell'insieme di $x$) $\rightarrow$ $O(1)$
- `Union(x, y)` $\rightarrow$ unisce due insiemi in uno già esistente ed eliminando l'altro $\rightarrow$  $\Theta(n)$ 
- Operazioni corrette e terminanti
- **Complessità** 
	- Analisi ammortizzata $\rightarrow$ calcolare il costo **medio** di una operazione qualsiasi nel contesto di un gruppo di operazioni, piuttosto che il costo per operazioni
		- Nell'analisi ammortizzata non ci sono considerazioni probabilistiche
	- il costo medio ammortizzato di un'operazione e' $\frac{\Theta n^2}{\Theta(n)} = \Theta(n)$ 

### Insiemi disgiunti: liste con unione pesata
- **Euristica** $\rightarrow$ strategia migliorativa e non un'implementazione diversa, ha un effetto soprattutto dal punto di vista pratico  
- **Algoritmo Euristico** $\rightarrow$ algoritmo che da una soluzione non ottimale si trova un problema la cui soluzione ottimale esiste, lo si fa per scelta per evitare il costo computazionale che avrebbe avuto quella esecuzione
- Strategia per migliorare la situazione $\rightarrow$ **unione pesata**
	- scriviamo in ogni insieme $S$ il numero di elementi (`S.rank`) e con le `Union()` si fanno gli aggiornamenti dei puntatori sempre sull'insieme piu' piccolo
- **Complessità**
	- si ha il caso peggiore quando tutti gli $s$ sono di dimensione uguale
	- costo totale delle $\Theta(n)$ unioni $\rightarrow$ $\Theta(n \cdot log(n))$ 

### Insiemi disgiunti: foreste di alberi
- Metodo piu' efficiente per implementare gli insiemi disgiunti
- In questa rappresentazione gli elementi vivono come prima, nelle strutture $S$, e sono puntati in un albero, mentre i rank sono i limiti superiori dell'altezza dell'albero.
- **Rappresentazione**:
	- nodo `x` (elemento che non ha puntatore ai figli) contiene:
    - `x.p` $\rightarrow$ padre
    - `x.rank` $\rightarrow$ rango
    - `x.key` $\rightarrow$ chiave
- Attenzione: questi alberi sono liste particolari e non vanno confusi con alberi e grafi
- **Operazioni**:
	- `Makeset` $\rightarrow$ crea un nuovo albero di altezza massima 0 con solo il nodo $x$
	- `Union` $\rightarrow$ unisce due alberi in uno solo:
		1. si trovano i rappresentanti degli elementi $(x, y)$ usati come indici
		2. si sceglie l'elemento di rango inferiore e si aggiorna solo il padre (si fa puntare il rappresentante con rango inferiore al rappresentante con rango superiore)
	- `FindSet` $\rightarrow$ scorrendo i puntatori verso l'alto li aggiorna appiattendo il ramo al quale appartiene il rappresentante e lo restituisce
- **Complessità**:
	- La complessita' di $m$ operazioni si vede con $O(m \cdot \alpha(n))$ con $\alpha(n)$ funzione inversa di Ackermann che cresce lentamente (tanto da essere costante)
	- costo totale $\rightarrow$ $\Theta(m)$ 

--- 
## Alberi
- Strutture dati fondamentali dinamiche e sparse, possono essere basate sull'ordinamento oppure no
- Generalizzano le liste ma se ne fa un uso diverso
- **Albero** (o **albero radicato**) $\rightarrow$ grafo aciclico connesso tale che ogni coppia di vertici e' connessa da al piu' un cammino
	- **Nodi** $\rightarrow$ vertici dell'albero.  
	- **Foglie** $\rightarrow$ nodi senza figli.  
- **Albero k-ario** $\rightarrow$ ogni nodo ha al massimo k figli.  
- **Altezza** $\rightarrow$ massimo numero di archi su un percorso semplice dalla radice ad una foglia (l'altezza è legata al numero di nodi in maniera non triviale).
	- BST $\rightarrow$ $\Theta(n \cdot \log(n))$ 
	- nessuna struttura:
		- caso medio $\rightarrow$ $\Theta(\log(n))$
		- caso peggiore $\rightarrow$ $\Theta(n)$
- **Albero diretto** $\rightarrow$ ogni sottoalbero ha un nodo predeterminato chiamato radice, che dota l'albero di un ordinamento topologico privilegiato
- **Albero indiretto** $\rightarrow$ la radice viene invece individuata in maniera arbitraria, cosi' come la direzionalita' dei cammini

- I nodi di un albero binario possono esser pensati come oggetti che possiedono, almeno, una **chiave** (`x.key`) e tre puntatori:
	1. il **padre** (`x.p`)
	2. il **figlio destro** (`x.right`)
	3. il **figlio sinistro** (`x.left`).
- Tutti i puntatori sono **nil** quando non sono definiti.
- Tipi di visite:
	- visite **in-order**;
	- visite **pre-order**;
	- visite **post-order**.

### Alberi binari di ricerca (BST)
- Sono una struttura dati dinamica, basata sull'ordinamento e implementata in maniera sparsa
- Essendo basata sull'ordinamento possiamo fare le operazioni di inserimento, cancellazione, ricerca, minimo, massimo, successore e predecessore

- Le regole che un albero binario di ricerca deve rispettare (anche note come **proprieta' BST**), sono:
	1. Per ogni nodo $x$, se un nodo $y$ si trova nel **sotto-albero sinistro**, allora `y.key <= x.key`;
	2. Per ogni nodo $x$, se un nodo $y$ s trova nel **sotto-albero destro**, allora `y.key > x.key`.
- Dunque si puo' dire che un BST e' **parzialmente ordinato**.

- I BST si creano vuoti come nel caso generale ed hanno esattamente la stessa struttura
- Sui BST ha senso effettuare la visita in order per restituire l'insieme di chiavi ordinate, ma non ha senso usare gli altri due tipi di visite.

- **Operazioni**:
	- Ricerca:
		- complessita' direttamente proporzionale all'albero
			- casso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$ 
	- Minimo e massimo 
		- il nodo minimo e' il nodo piu' a sinistra dell'albero mentre il massimo e' quello piu' a destra
		- complessita' direttamente proporzionale all'albero
			- casso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$ 
	- Successore e predecessore:
		- se $x$ ha figlio destro $\rightarrow$ il successore e' il minimo del sottoalbero destro di $x$
		- se $x$ non ha figlio destro $\rightarrow$ il successore è tra i suoi antenati ovvero bisogna risalire finché la relazione padre-figlio diventa padre-figlio sinistro
		- complessita' direttamente proporzionale all'albero
			- casso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$ 
	- Inserimento:
		- inserisce un nuovo elemento come foglia nell'albero, questo permette di rispettare la proprietà BST ed essere efficiente.
		- Correttezza: si vuole dimostrare che il risultato di un inserimento sia un BST (T = BST, T' = risultato di un inserimento)
			- se $T$ è vuoto $\rightarrow$ while non eseguito $\rightarrow$ $z$ è la radice di $T'$ $\rightarrow$ $T'$ è un BST
			- se $T$ non è vuoto $\rightarrow$ invariante del ciclo = posizione corretta di $z$ è nel sotto-albero di x e y ne mantiene il padre:
			    - caso base: $x$ = `T.root`
			    - caso induttivo: dopo la `i-1` esecuzione, $z$ è confrontato con $x$ e $y$, mantenendo la proprietà
			    - alla fine del ciclo `x = nil` ed è nella posizione corretta di $z$
		- Complessita': direttamente proporzionale all'altezza dell'albero
			- caso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$
	- Eliminazione: 
		- si distinguono 3 casi:
			1. se $z$ è foglia (non ha figli) -> si elimina $z$
			2. se $z$ ha un solo figlio -> si elimina $z$ facendo puntare il padre di $z$ al figlio di $z$ (come eliminare un nodo in una lista)
			3. se $z$ ha due figli -> si cerca il successore di $z$ (es. $y$) e si scambia $z$ con $y$, poi si elimina $y$
		- Complessita':
			- caso peggiore $\rightarrow$ $\Theta(n)$
			- caso medio $\rightarrow$ $\Theta(\log(n))$

### Alberi Red-Black (RBT)
- E' un BST bilanciato per costruzione
- Ha tutte le caratteristiche di un BST ma la sua altezza e' sempre $\Theta(\log(n))$ 
  ($n =$ numero di nodi)
- E' una struttura dati dinamica, sparsa e basata sull'ordinamento
- Ogni nodo in un RBT ha delle informazioni in piu' rispetto a un nodo di BST:
	- colore (`x.color`) $\rightarrow$ rosso o nero (per convenzione)
	- ogni foglia possiede due figli virtuali che non contengono chiave e sono sempre di colore nero
	- definita una sentinella `T.nil` $\rightarrow$ campo aggiuntivo di $T$ (colore fissato a nero per il ruolo di foglia esterna)
- I nodi si differenziano tra 
	- interni $\rightarrow$ ha sempre due figli e una chiave
	- esterni $\rightarrow$ nodo nero senza chiave con proprieta' uguali agli altri nodi
- Le regole che ogni RBT deve rispettare sono:
	1. Ogni nodo e' rosso o nero;
	2. La radice e' nera;
	3. Ogni foglia (esterna, **nil**) e' nera;
	4. Se un nodo e' rosso, entrambi i suoi figli sono neri;
	5. Per ogni nodo, tutti i percorsi semplici da lui alle sue foglie, contengono lo steso numero di nodi neri.

- Le proprieta' garantiscono il bilanciamento dell'albero
- Il bilanciamento e' una proprieta' dinamica $\rightarrow$ relazione tra numero di nodi e altezza
- Altezza nera (`bh(x)`) di un nodo $x$ in $T$ $\rightarrow$ numero di nodi neri su un qualsiasi percorso semplice da $x$ ad una foglia esterna 
- Altezza nera dell'albero $T$ $\rightarrow$ altezza nera della root `bh(T.root)`
- numero $n$ di nodi in $T$ $\rightarrow$ $2^{\text{bh(T.root)}}-1$, cioe' $n \ge 2^{\frac{h}{2}}-1$
- $h \le 2 \cdot \log(n+1)$ $\rightarrow$ l'altezza nel caso peggiore è di 2 volte il logaritmo del numero di nodi interni più 1 quindi è $\Theta(\log(n))$
- $h = \Theta(\log(n)) \rightarrow$ bilanciato

- Le operazioni di inserimento e cancellazione potrebbero violare le proprietà di un RBT quindi implementando queste operazioni si modifica la struttura dell'albero per ripristinare le proprietà.
- Per ripristinare la struttura si usa la **rotazione** $\rightarrow$ operazione per mantenere le proprietà di un BST (non di un RBT).
- Complessità $\rightarrow$ $O(1)$ 

- Usando l'algoritmo di inserimento dei BST, le proprieta' dei BST sono rispettate
- Se il nodo inserito e' rosso si avra':
	- proprieta' 1 $\rightarrow$ rispettata
	- proprieta' 2 $\rightarrow$ violata ($z$ diventa la root che dovrebbe essere nera)
	- proprieta' 3 $\rightarrow$ rispettata
	- proprieta' 4 $\rightarrow$ violata ($z$ diventa figlio di un nodo rosso che dovrebbe essere nero)
	- proprieta' 5 $\rightarrow$ rispettata
- Per mantenere le proprieta' si usa `RBTreeInsertFixup`
- **Correttezza** $\rightarrow$ se esiste il problema dopo l'inserimento (proprieta' 2 e 4 violata) si spinge verso l'alto con il caso 1 finche' possibile. Quando non e' piu' possibile si passa al caso 2 o 3. Una rotazione risolve il problema e esce dal ciclo
- **Invariante**:
	1. $z$ e' rosso
	2. se $z.p$ e' la root $\rightarrow$ $z.p$ e' nero
	3. se $T$ viola qualche proprieta' $\rightarrow$ viola una sola proprieta'
		- se viola la proprieta' 4 $\rightarrow$ $z$ e $z.p$ sono rossi
		- se viola la proprieta' 2 $\rightarrow$ $z$ e' la root rossa
- **Complessità** $\rightarrow$ nel caso peggiore costa tanto quanto risalire l'albero fino alla root quindi $\Theta(h) = \Theta(\log(n))$ 

### Alberi B (BT = Balanced Tree)
- Struttura dati dinamica, sparsa e basata sull'ordinamento
- Generalizza un RBT $\rightarrow$ mantiene le proprieta' di bilanciamento ma ha fini diversi
- E' sempre completo $\rightarrow$ il nodo stesso deve contenere piu' chiavi
- Ottimizzati per minimizzare gli accessi al disco (memoria secondaria)
- Un BT si caratterizza per:
	1. possedere un'arietà (branching factor) superiore ad un parametro, il grado minimo = $t$, che in questo caso è 2 (spesso dell'ordine delle migliaia)
	2. un'altezza proporzionale al logaritmo in base $n$ (con $n$ numero di chiavi)
	3. avere nodi con molte chiavi ordinate tra loro
	4. crescere verso l'alto, non verso il basso (un nodo inizia da radice poi diventa un nodo interno e il nuovo nodo è radice, ...)
- Complessita' proporzionale all'altezza
- Un nodo di un BT e' composto da:
	- `x.p` $\rightarrow$ padre
	- `x.n` $\rightarrow$ numero di chiavi nel nodo
	- `x.leaf` $\rightarrow$ booleano, indica se il nodo e' foglia
	- `x.c_1, ..., x.c_x.n+1` $\rightarrow$ puntatori ai figli definiti se 
	- `x.key_1, ..., x.key_x.n` $\rightarrow$ $n$ chiavi ordinate
- Le proprieta' di un albero B sono:
	1. Ogni nodo, tranne la radice, ha almeno $t-1$ chiavi;
	2. Ogni nodo puo' contenere al massimo $2 \cdot t-1$ chiavi;
	3. Per ogni nodo $x$, `x.key_1 <= x.key_2 <= ... <= x.key_x.n`;
	4. Per ogni nodo $x$, se un nodo $y$ contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono minori o uguali a `x.key_i`
	5. Per ogni nodo $x$ , se un nodo $y$ e' contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono maggiori di `x.key_i-1`
- Altezza massima $\rightarrow$ $h \le log_t(\frac{n+1}{2}) = O(log_t(n))$ 

- **Operazioni**
	- Convenzioni:
		- usiamo esplicitamente le operazioni `DiskRead` e `DiskWrite` per tenere conto degli accessori al disco; 
		- diciamo che la radice `T.root` e' sempre in memoria principale
		- rispetto a un nodo passato come parametro, assumiamo che sia gia' stata eseguita `DiskRead` su di esso.
		- calcoliamo la complessita' sia in termini di uso della CPU sia in termini di numero di accessi al disco
	- Ricerca (`BTreeSearch`):
		- si sceglie tra $x.n+1$ possibili figli
		- prende in input un nodo $x$ di $T$ e una chiave $k$
		- restituisce
			- un puntatore a un nodo $y$
			- un indice $i$ se ricerca ha successo altrimenti nil
		- Correttezza immediata
		- Complessita':
			- numero di accessi al disco: $O(h) = O(\log_t(n))$
			- uso di CPU: $\Theta(t \cdot h) = \Theta(t \cdot log_t(n))$ 
	- Inserimento (`BTreeCreate`):
		- Prima di fare l'inserimento l'albero deve esistere
		- `Allocate()` $\rightarrow$ funzione che crea e occupa uno spazio sufficiente per un nodo di memoria
		- Split del figlio per l'inserimento:
			- Funzionamento:
				1. riempire un nodo fino a quando diventa pieno ($2t-1$ chiavi)
				2. dividere il nodo in due nodi di $t-1$ chiavi l'uno
				3. inserire la nuova chiave del nodo padre
				4. se il nodo padre e' pieno, ripetere l'operazione un livello piu' in alto
			- Complessita':
				- numero di accessi al disco $\rightarrow$ $\Theta(1)$
				- uso di CPU $\rightarrow$ $\Theta(t)$
		- Inserimento di una chiave:
			- Usa `BTreeInsertNonFull` $\rightarrow$ inserisce ricorsivamente una chiave in un nodo non pieno
			- La chiave non va sempre inserita in una foglia:
				- se il nodo considerato e' foglia $\rightarrow$ si inserisce la chiave
				- se il nodo considerato non e' foglia $\rightarrow$ si inserisce ricorsivamente in un nodo e se e' pieno si procede ricorsivamente
			- Il pre-emptive split permette di effettuare split su ogni nodo pieno che si trova sul percorso di discesa fino alla foglia in cui si deve andare.
			- In questo modo si minimizza il numero di accessi al disco e l'uso di CPU.
			- Correttezza:
				- **caso base:** all'inizio di ogni esecuzione di `BTreeInsertNonFull`, il nodo $x$ è non pieno e la chiave $k$ viene inserita nel sotto-albero di $x$, in uno dei suoi figli.
				- **caso induttivo:** si trova il posto giusto per $k$ e, poiché $x$ non è foglia, carica il giusto figlio. Essendo il nodo non pieno per ipotesi (se nodo pieno $\rightarrow$ split) si richiama `BTreeInsertNonFul`
			- Complessita':
				- numero di accessi al disco $\rightarrow$ $\Theta(h) = \Theta(\log_t(n))$
				- uso di CPU $\rightarrow$ $\Theta(h \cdot t) = \Theta(t \cdot log_t(n))$

---
## Grafi
- Strutture dati statiche, sparse o compatte e non basate sull'ordinamento
- Il fatto di essere statiche comporta che non ci siano operazioni di inserimento o cancellazione di elementi
- Un grafo e' una tripla $G = (V, E, W)$ dove:
	- $V$ $\rightarrow$ insieme di vertici
	- $E \subseteq V * V$ $\rightarrow$ insieme di archi 
	- $W: E \rightarrow \mathbb{R}$ $\rightarrow$ funzione che assegna un peso ad ogni arco
- $u \rightsquigarrow v$ $\rightarrow$ quando da un vertice $u$ possiamo raggiungere un vertice $v$
- Gradi di un grafo:
	- **grado entrante** $\rightarrow$ numero di archi entranti in un vertice
	- **grado uscente** $\rightarrow$ numero di archi uscenti da un vertice
- Proprieta' statiche di un grafo:
	- **Diretto** $\rightarrow$ gli archi sono orientati
	- **Indiretto** $\rightarrow$ gli archi non sono orientati
	- **Pesato** $\rightarrow$ gli archi hanno un peso ($W$ non costante)
	- **Non pesato** $\rightarrow$ gli archi non hanno un peso ($W$ costante $\rightarrow$ $G = (E, V)$)
- Proprieta' dinamiche di un grafo:
	- **Sparso**: $|E| << |V|^2$ $\rightarrow$ numero di archi molto minore rispetto al numero di vertici
	- **Denso**: $|E| \approx |V|^2$ $\rightarrow$ numero di archi simile al numero di vertici
- Rappresentazione di un grafo:
	- **liste di adiacenza** $\rightarrow$ si usa un array $Adj[1, \ldots, |V|]$ dove ogni elemento punta ad una lista (preferibile nei grafi sparsi)
	- **matrice di adiacenza** $\rightarrow$ si usa una matrice $A[1, \ldots, |V| \space * \space 1, \ldots, |V|]$ dove $A[u, v] = w$ e $w$ e' il peso dell'arco (preferibile per i grafi densi)
- `u.att` $\rightarrow$ indica un attributo `att` associato con un vertice $v$.

### Grafi: visita in ampiezza (BFS)
- È dato un grafo $G = (V, E)$ diretto o indiretto, non pesato, ed un vertice sorgente $s \in V$   
- **Obiettivo**: conoscere quanti archi sono necessari per raggiungere qualunque altro vertice (raggiungibile) da $s$  
- **Soluzione**: si usa una visita di $G$ in ampiezza (BFS - *Breadth First Search*)

**BFS**:
- Esplorazione degli archi di $G$ e scoperta di nuovi vertici raggiungibili da quelli sconosciuti
- Si usano i colori:
	- **bianco** $\rightarrow$ vertice non ancora scoperto, tutti i vertici sono bianchi all'inizio tranne $s$ che e' grigio
	- **grigio** $\rightarrow$ vertice scoperto
	- **nero** $\rightarrow$ quando tutti i vertici adiacenti sono stati scoperti
- Attributi:
	- `u.color` $\rightarrow$ colore del vertice
	- `u.pi` $\rightarrow$ predecessore di $u$ nel cammino minimo da $s$ a $u$
	- `u.d` $\rightarrow$ distanza minima in numero di archi da $s$ a $u$
- L'algoritmo computa la distanza minima in numero di archi tra $s$ ed ogni vertice scoperto e produce un albero di visita in ampiezza con tutti i vertici raggiungibili da $s$
- $\delta(s,v) \rightarrow$ **distanza piu' corta** da $v$ a $s$ (numero minimo di archi che sono necessari per raggiungere $v$ da $s$)
- Proprieta':
	- che sia zero tra un vertice e se stesso $\rightarrow$ $\delta(s,s) = 0$;
	- che sia infinito da $s$ a $v$ quando il secondo e' irraggiungibile dal primo $\rightarrow \delta(s,v) = \infty$;
	- che sia una distanza $\rightarrow$ disuguaglianza triangolare (per ogni coppia di vertici $v, u$ tali che esiste un arco $(u,v)$, succede che $\delta(s,v) \le \delta(s,u)+1$).
- **Correttezza**: dopo l'esecuzione, per ogni $v$ raggiungibile da $s$, $\text{v.d} = \delta(s,v)$ e se $v \ne s$, almeno uno dei percorsi piu' brevi da $s$ a $v$ si ottiene da uno dei percorsi piu' brevi da $s$ a $v.\pi$ con l'arco $(v.\pi, v)$.
	- **caso base**: $v=s \rightarrow$ all'entrata in $Q$ $s.d = 0 = \delta(s,s)$
	- **caso induttivo**: $v$ scoperto da $u$ quindi gia' in $Q \rightarrow u.d \ge \delta(s,u)$ quindi $v.d = u.d + 1$ e $v.d \ge \delta(s,u)$
- **Complessità**: per algoritmi sui grafi si usa l'analisi aggregata (si guarda il numero di elementi/archi)
	- **grafo connesso** $\rightarrow$ $\Theta(|V| + |E|)$
	- **grafo non connesso** $\rightarrow$ $O(|V| + |E|)$
	- **grafo denso** $\rightarrow$ $\Theta(|V|^2)$ ma si accetta anche $\Theta(|V| + |E|)$ 

### Grafi: visita in profondita' (DFS)
- Consideriamo un grafo $G = (V,E)$, se questo e' diretto di puo' visitare in maniera diversa per avere informazioni piu' utili
- Con la DFS si possono risolvere 3 problemi:
	1. stabilire se $G$ e' **ciclico** $\rightarrow$ stabilire se contiene almeno un ciclo
	2. costruire un **ordinamento topologico** di $G$ $\rightarrow$ elencare tutti i suoi vertici in un ordine qualsiasi tale che ogni vertice $v$ e' elencato solo se tutti i vertici **dai quali** $v$ si puo' raggiungere sono stati elencati prima
	3. conoscere ed enumerare tutte le **componenti fortemente connesse** di $G$ $\rightarrow$ elencare tutti i sottoinsiemi massimali di $V$ tale che, ogni vertice in ogni sottoinsieme raggiunge ogni altro vertice di quel sottoinsieme

**DFS**:
- Obiettivo $\rightarrow$ scoprire tutti i vertici raggiungibili da ogni potenziale sorgente $s$
- Output $\rightarrow$ foresta di alberi (uno per ogni sorgente)
- **Differenza con BFS** $\rightarrow$ i vertici vengono scoperti il prima possibile a partire da quelli gia' scoperti (e' ricorsiva)
- Assunzioni:
	- $G$ e' rappresentato con liste di adiacenza
	- `u.color` $\rightarrow$ colori per distinguere i vertici ancora da scoprire e quelli gia' scoperti
	- `v.pi` $\rightarrow$ predecessore di $v$
	- `v.d` $\rightarrow$ momento della scoperta di $v$
	- `v.f` $\rightarrow$ momento di abbandono di $v$ (tutto il sotto-grafo e' stato scoperto)
	- campi `v.d` e `v.f`:
		- interi tra $1$ e $2 \cdot |V|$
		- generalmente chiamati tempi (di scoperta e di abbandono)
		- Per ogni vertice $u$, abbiamo $u.d < u.f$.
- Ogni volta che `DepthVisit(G, u)` viene chiamata in `DepthFirstSearch(G)` si scopre un nuovo albero della foresta.
- **Complessità**:
	- `for (u in G.V)` $\rightarrow$ $\Theta(|V|)$
	- `for (u in G.V)` con `DepthVisit(G, u)` $\rightarrow$ $\Theta(|V| + |E|)$
	- `for (v in G.Adj[u])` $\rightarrow$ $\Theta(|E|)$
	- Totale $\rightarrow$ $\Theta(|V| + |E|)$

**Cycle Detection**:
- Ciclo: percorso $v_1, v_2, ..., v_k$ di vertici tali che per ogni $i$ esiste l'arco $(v_i, v_{i+1})$ e che $v_1 = v_k$
- Quando un grafo diretto e' privo di cicli, lo chiamiamo `DAG` (Directed Acyclic Graph)
- Dato un grafo diretto $G$ stabilire se presenta o no un ciclo
	1. si esegue la DFS modificato in modo che si interrompa se visita un nodo grigio
	2. restituisce `True` se visita un nodo grigio, `False` altrimenti
- **Correttezza**: l'algoritmo e' corretto se e solo se restituisce `True` quando $G$ e' ciclico e `False` altrimenti
	- caso $G$ ciclico: qualunque nodo si prenda (per ipotesi si visitano tutti i nodi), se c'è un ciclo viene trovato dall'algoritmo e deve restituire `True`
	- caso $G$ aciclico: non ci sono cicli quindi è impossibile incontrare un nodo grigio e l'algoritmo deve restituire `False`
- **Complessità** $\rightarrow$ identica a DFS

**Topological Sort**:
- Prende in input un grafo connesso $G$ senza cicli
- Restituisce una lista collegata $v_1, ..., v_{|V|}$ di vertici topologicamente ordinati
- Per ogni coppia $v_i, v_j$ di vertici, $v_i$ appare prima nella lista di $v_j$ se e solo se $v_i$ precede topologicamente $v_j$
- `TopologicalSort(G)`
	- Si chiama la DFS per calcolare i tempi di scoperta `v.d` e abbandono `v.f` $\forall \space v \in G.V$  
	- Ogni nodo finito $v$ (`v.color = BLACK`) viene inserito alla testa di una lista collegata
	- Restituisce la lista collegata
- **Correttezza** $\rightarrow$ un arco $(u, v)$ è tale che $u$ precede $v$ in $G$
- **Complessità** $\rightarrow$ stessa della DFS

**SCC (Strongly Connected Components)**:
- Dato un grafo diretto $G$, una **componente fortemente connessa** (SCC) e' un sottoinsieme massimale $V' \subseteq V$ tale che, per ogni $u, v \in V'$, succede che $u ⇝ v$ e che $v ⇝ u$. 
- **Grafo trasposto** $\rightarrow$ $G^T$ di $G$ e' ottenuto invertendo la direzione di ogni arco (complessita' $\Theta(|V| + |E|)$)
- La proprieta' piu' interessante di $G^T$ e' che $G$ e $G^T$ hanno le stesse SCC.
- **Complessità** $\rightarrow$ stessa della DFS

### Grafi per copertura minima
- Ci concentriamo sui grafi pesati indiretti connessi $\rightarrow$ $G = (V, E, w)$
- **Obiettivo**: trovare un percorso che minimizzi il costo
- **Alberi di copertura minimo** (o **MST**) $\rightarrow$ sottoinsieme di archi che forma un albero, copre tutti i vertici e la cui somma dei pesi e' minima
- **Strategia**: algoritmi greedy $\rightarrow$fare ad ogni passo la scelta localmente migliore in modo da ottenere una soluzione globalmente migliore
- **Caratteristiche alberi indiretti**: 
	- tutti i nodi possono essere radice
	- tutti i nodi di un solo arco insistente possono essere foglie
	- una volta determinata la radice si determinano le foglie
- **Proprietà' MST**:
	- e' un insieme di archi tali che formano un albero (indiretto) che tocca tutti i nodi del grafo
	- eliminando qualunque arco da un albero di copertura si ottengono due alberi connessi
	- se ci sono archi di peso uguale si possono avere piu' MST, altrimenti e' unico
- **Correttezza del taglio di un arco**:
	1. Si prende un grafo e si partiziona $S$ e $V \backslash S$ dove si ha l'arco di peso minimo $(a,b)$ che collega le due partizioni
	2. Si prende poi un grafo ma con un arco qualsiasi $(u,v)$ che anch'esso collega le due posizioni
	3. L'albero che si crea con $(u,v)$ pesa di piu' di quello con $(a,b)$ e quindi non e' un MST
	4. L'arco $(a,b)$ e' chiamato arco sicuro di taglio $\rightarrow$ arco di peso minimo che attraversa due partizioni

**Prim**:
- Parte da un vertice qualsiasi e ad ogni passo aggiunge un arco ed un vertice in modo che l'arco raggiunto sia un arco sicuro
- **Struttura dati necessaria** $\rightarrow$ coda di priorita' per mantenere gli archi in ordine di peso
- Ogni vertice $G$ viene arricchito con due campi:
	- `v.key` $\rightarrow$ il peso minimo, inizialmente $\infty$, tra gli archi che connettono qualche vertice $T$ con $v$ 
	- `v.pi` $\rightarrow$ il padre di $v$, inizialmente `Nil`, nel MST risultante.
- Inizialmente tutti i vertici si trovano nella coda di priorita' $Q$ semi-ordinata su `v.key` tranne $r$ che ha chiave 0
- **Correttezza**: si definisce $T$ come l'insieme di tutte le coppie $(v.\pi, v)$ con $v.\pi$ definito e $v \notin Q$.
	- **Invariante**: $T$ è sempre sottoinsieme di qualche MST
	- **caso base**: $T$ e' vuoto $\rightarrow$ insieme vuoto e' sottoinsieme di ogni insieme quindi anche di un MST di $G$
	- **caso induttivo**: si considera $T$ ad un certo punto e $T'=T \cup \{(v.\pi,v)\}$ insieme ottenuto dopo una esecuzione del ciclo while:
		1. $S$ = insieme di tutti i vertici coperti da $T$
		2. $(S, V \backslash S)$ e' un taglio di $G$
		3. l'arco $(v.\pi, v)$ e' un arco sicuro di questo taglio
- **Complessità**: dipende dal costo delle operazioni della coda $Q$ (`ExtractMin` e `DecreaseKey`) la quale si puo' implementare in due modi:
	1. uso di un array senza ordine
		- costruzione coda: $\Theta(|V|)$
		- estrazione del minimo: $\Theta(|V|)$
		- decremento: $\Theta(1)$
		- per i grafi densi si ha:
			- inizializzazione: $\Theta(|V|)$
			- costruzione coda: $\Theta(|V|)$
			- estrazione del minimo: $\Theta(|V|^2)$
			- decremento: $\Theta(|V|^2)$
			- totale: $\Theta(|V|^2)$
	1. uso di heap binaria:
		- costruzione coda: $\Theta(|V|)$
		- estrazione del minimo: $\Theta(\log(|V|))$ 
		- decremento: $\Theta(\log(|V|))$
		- per i grafi sparsa con heap binaria:
			- inizializzazione: $\Theta(|V|)$
			- costruzione coda: $\Theta(|V|)$
			- estrazione del minimo: $\Theta(|V| \cdot \log(|V|))$
			- decremento: $\Theta(|E| \cdot \log(|V|))$
			- totale: $\Theta(|E| \cdot \log(|V|))$
		- per i grafi densi con heap binaria si ha:
			- estrazione: $\Theta(|V|^2 \cdot \log(|V|))$
			- resto uguale
- Soluzione migliore per la complessità: il caso peggiore si ha con un grafo denso e per minimizzare il danno si usa una coda senza struttura però la scelta heap binaria è migliore di array senza ordine

**Kruskal**:
- Utilizza una generalizzazione del concetto di taglio e del concetto di arco sicuro per il taglio al ne di ottenere un albero di copertura minimo
- Idea algoritmo:
	- possiamo ordinare gli archi in ordine crescente di peso
	- analizzarli uno a uno in questo ordine
	- stabilire se inserirli o meno nell'MST
- $T$ = insieme di archi scelti fino ad un certo punto
- A differenza di MST-Prim, in MST-Kruskal $T$ non è obbligatoriamente un albero in ogni momento ma lo sarà sicuramente alla fine
- Un sottoinsieme $S$ di $V$ e' $T$-connesso se:
	- e' un albero
	- e' massimale
- **Taglio generalizzato**:
	- identifichiamo tutte le componenti $T$-connesse di $V$: $S_1, S_2, ..., S_n$
	- la tupla $(S_1, S_2, ..., S_n)$ e' certamente una partizione di $V$ e generalizza il concetto di taglio
	- un arco $(u,v)$ attraversa il taglio $(S_1, S_2, ..., S_n)$ se $u \in S_i$ e $v \in S_j$ con $i \ne j$ quindi se $u$ e $v$ appartengono a componenti diverse $T$-connesse
- **Struttura dati necessaria**: insiemi disgiunti per garantire che un arco scelto attraversi il taglio
- Operazioni:
	- `MakeSet` $\rightarrow$ costruisce un nuovo insieme (componente $T$-connessa)
	- `FindSet` $\rightarrow$ stabilisce se due elementi appartengono allo stesso insieme (se appartengono alla stessa componente $T$-connessa)
	- `Union` $\rightarrow$ unisce due insiemi in uno solo (unisce due componenti $T$-connesse in una sola $\rightarrow$ conseguenza di avere scelto un arco)
- **Correttezza**: con il primo `MakeSet` vengono creati gli insiemi $S_1, S_2, \ldots, S_{|V|}$ e costruiscono un taglio generalizzato, poi si dimostra l'invariante
	- **invariante**: ogni arco che viene raggiunto nel ciclo e' un arco sicuro
	- **caso base**: poiche' gli archi sono prima ordinati, il primo arco scelto è un arco di peso minimo, e certamente attraversa il taglio
	- **caso induttivo**: si considerano due vertici $(u,v)$ con rappresentanti diversi
		- se `FindSet` dei due vertici è diverso, essi devono avere il rappresentante diverso per creare l'arco
		- se `FindSet` dei due vertici è uguale, i due vertici sono nello stesso insieme e l'arco non viene creato
- **Complessità**: si distingue tra grafi densi e grafi sparsi e molto dipende dalle operazioni di insiemi disgiunti
	- Se il grafo e' denso allora abbiamo:
		- inizializzazione $\rightarrow (O(1))$
		- ordinamento $\rightarrow (\Theta(|E| \cdot log(|E|)) = \Theta(|V|^2 \cdot log(|E|)) = \Theta(|V|^2 \cdot log(|V|)))$
		- piu' $O(|V| + |E|)$ diverse operazioni su insiemi, di cui $O(|V|)$ sono `MakeSet`
		- **totale** $\rightarrow \Theta(|V|^2 \cdot log(|V|))$.
	- Se il grafo e' sparso abbiamo:
		- inizializzazione $\rightarrow O(1)$
		- ordinamento $\rightarrow \Theta(|E| \cdot log(|E|))$ 
		- $O(|V| + |E|)$ diverse operazioni di cui $O(|V|)$ sono `MakeSet`
		- **totale** $\rightarrow \Theta(|E| \cdot log(|E|))$.

### Grafi per cammini minimi con sorgente singola
- Ci concertiamo su grafi diretti, pesati e connessi
- Dato $G = (V, E, W)$ e dato un vertice $s$, ci proponiamo di trovare per ogni $v \in V$ il percorso di peso minimo che porta da $s$ a $v$. 
- Questo problema e' da considerarsi una generalizzazione di `BreadthFirstSearch`
- Dato un percorso minimo, tutti i suoi sotto-percorsi sono anch'essi minimi
- Se ci sono pesi negativi:
	- se non ci sono cicli negativi raggiungibili da $s$, allora i percorsi minimi da $s$ sono ancora ben definiti
	- se da $s$ c'e' un ciclo negativo raggiungibile, allora non ha senso definire un percorso di peso minimo da $s$ (ogni nuovo passaggio attraverso quel ciclo diminuirebbe il peso )
- **Ruolo dei cicli sui percorsi minimi**:
	- se $s \rightsquigarrow v$ e' un percorso minimo da $s$ a $v$ che contiene un ciclo positivo, allora eliminando il ciclo si ottiene un nuovo percorso minimo; pertanto $s \rightsquigarrow v$ non contiene un percorso minimo.
	- se il ciclo fosse negativo, il peso del percorso minimo sarebbe $-\infty$.
	- se il ciclo ha peso 0, allora eliminandolo si ottiene un nuovo percorso minimo dello stesso peso. 
- Indipendentemente dalla presenza di pesi negativi, possiamo sempre dire che un percorso minimo e' aciclico e quindi ci limitiamo a studiare percorsi minimi di lunghezza $|V|-1$ al massimo
- Output di ogni algoritmo per grafi diretti, pesati e senza cicli negativi $\rightarrow$ albero dei cammini di peso minimo (creazione dei percorsi dai da $v.\pi$ per ogni vertice $v \in V\{S\}$)
- **Tecnica di rilassamento**:
	- Rilassare un arco $(u, v)$ consiste nel testare se la stima su $v.d$ puo' essere migliorata grazie all'arco $(u, v)$
	- La stima su `v.d` deve prima essere inizializzata
	- Esempio: se il processo minimo e' $v.d=10$ e l'arco $W(u,v)$ ha peso 5, allora $v.d=5$ e $v.\pi = u$ 

**Bellman-Ford**:
- Lavora con pesi negativi e cicli negativi
- Restituisce:
	- se non ci sono cicli negativi raggiungibili da $s$ $\rightarrow$ i cammini minimi ed i loro pesi
	- se $s$ nel raggiungere un vertice trova un ciclo negativo $\rightarrow$ restituisce `False`
- **Correttezza** $\rightarrow$ si vuole dimostrare che:
	- se $G$ non ha cicli negativi $\rightarrow$ $\forall \space v$ raggiungibile da $s$ si ha che $v.d = \delta(s,v)$ (peso "reale" di cammino minimo tra $s$ e $v$) e l'algoritmo restituisce `True`
		- **invariante**: dopo $i$ esecuzioni del ciclo piu' esterno $v[i].d = \delta(s, v[i])$ 
		- **caso base**: dopo 0 tentativi, $v[0]=s$ t.c. $v[0].d = 0 = \delta(s,v[0])$ $\rightarrow$ conseguenza di `InizializeSingleSource`
		- **caso induttivo**: dopo $i-1$ iterazioni, si ha come ipotesi induttiva $v[i-1].d = \delta(s, v[i-1])$. Poiche' all'i-esima iterazione si rilassano tutti gli archi, si rilassa anche l'arco $(v[i-1], v[i])$ ma dopo ogni rilassamento il valore di $v[i].d$ non puo' cambiare perche' e' gia' minimo
	- Se $G$ ha un ciclo negativo raggiungibile da $s$ $\rightarrow$ l'algoritmo restituisce `False`
- **Complessità** $\rightarrow$ $\Theta(|V| \cdot |E|)$, cioe' $O(|V|^3)$ ($\Theta(|V|^3)$ nel caso dei grafi densi)

**Percorsi minimi nei grafi aciclici**:
- In questo caso si può usare un'ottimizzazione dell'algoritmo di `Bellman-Ford` per i grafi aciclici ovvero `DAGShortestPath`.  
- Questo prevede che un grafo aciclico può essere ordinato topologicamente (se precede $v$ in un percorso minimo allora u precede v topologicamente
- **Correttezza**: 
	- **invariante**: assumendo che $v$ sia raggiungibile da $s$ per un percorso minimo qualsiasi $s = v_0, v_1, \ldots, v_k = v$, dopo l'$i$-esima esecuzione del ciclo, si ha che $v_i.d = \delta(s, v_i)$.
	- **caso base**: $s = v, \space v.d = 0$ $\rightarrow$ $s$ non puo' raggiungere se stesso e quindi nessun arco da risalire
	- **caso induttivo**: siccome i vertici sono in ordine topologico, si ha che $v[i]$ precede $v[j]$ topologicamente se $i<j$. Quindi dopo il rilassamento di tutti gli archi uscenti da $v[i]$ si ha che $v[j].d = \delta(s, v[j])$ perche' $v[i]$ precede $v[j]$ topologicamente
- **Complessità**:
	- ordinamento topologico: $\Theta(|V| + |E|)$
	- rilassamento: $\Theta(|V| + |E|) \rightarrow$ $V$ volte perche' il primo for viene eseguito $|V|$ mentre $E$ volte perche' nel secondo for si scorrono al massimo tutti gli archi
	- totale: $\Theta(|V| + |E|)$

**Dijkstra**:
- L'algoritmo di Dijkstra migliora le prestazioni di `Bellman-Ford()` aggiungendo l'ipotesi che tutti gli archi hanno peso positivo o 0, utilizzando una tecnica che ricorda l'algoritmo di Prim.
- Come in `MSTPrim()` abbiamo bisogno di una coda di priorita'. La chiave sara' il valore $v.d$ di ogni vertice
- Come in `MSTPrim`, l'operazione di `Relax(u, v, W)` in realtà "nasconde" un `DecreaseKey(Q, v)` durante il cambio del valore di una chiave `v.d`
- Questa operazione si applica solo ai vertici ancora nella cosa $Q$
- Dopo `InizializeSingleSource` il vertice $s$ e' il primo ad essere estratto dalla coda $Q$ poiche' $s.d = 0$
- Nessuno inserisce elementi in $Q$, pertanto il ciclo **while** viene eseguito precisamente $|V|$ volte
- **Correttezza**:
	- **invariante**: all'inizio di ogni iterazione del ciclo **while**, $v.d = \delta(s, v)$ per ogni $v \in S$
	- **caso base**: quando $S = 0$, l'invariante e' triviale vera
	- **caso induttivo**: l'ipotesi di non avere pesi negativi permette di affermare che se si ha un percorso minimo tra $s$ e $u$ e si aggiunge l'arco $(u, v)$ allora si ha un percorso minimo tra $s$ e $v$ poiché tutti gli elementi precedenti sono già stati rilassati
- **Complessità**: come per `MSTPrim`, si può implementare con array o heap binarie e si possono considerare i due casi di grafi densi e sparsi
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

### Grafi per cammini minimi con sorgente multipla
- Dato un grafo $G = (V, E, W)$ pesato, diretto e connesso
- **Obiettivo**: calcolare il peso del percorso minimo per ogni coppia di vertici
- **Rappresentazione**: utilizzo della matrice delle distanze del grafo
	- Il grafo $G$ e' rappresentato dalla matrice $W$ di pesi
- **Risultato**: 
- $D \rightarrow$ matrice dei pesi calcolati
- $\Pi \rightarrow$ matrice dei predecessori (indicazioni sul percorso minimo)

**Programmazione dinamica**:
- Risolve in maniera efficiente un problema di ottimizzazione suddividendo dei problemi in sottoproblemi comuni  
- **Caratteristica della programmazione dinamica**: tutta la complessità di progettazione si accumula nella progettazione iniziale

**Algoritmo della moltiplicazione di matrici**
- Caratterizzazione della sottostruttura ottima basata su archi:
	- $L^m_{ij}$ $\rightarrow$ i peso minimo del cammino tra il vertice $i$ e il vertice $j$ che si puo' costruire usando al piu' $m$ archi
	- $L^m_{ij} = \min\{L^{m-1}_{ij}, \min_k\{L^{m-1}_{ik} + W_{kj}\}\}$ 
	- struttura percorso minimo: $i \rightarrow k \rightarrow j$ 
	- $\min_k\{ L^m _{ik} + W_{kj} \}$ $\rightarrow$ soluzione piu' conveniente usando $k$ come ultimo predecessore
- **Versione lenta**:
	- L'idea e' calcolare la matrice $L$ per ogni numero fino a $n-1$ (numero di vertici - 1) e restituire la matrice $L(n-1)$ che contiene i pesi dei cammini mini tra tutte le copie di vertici
	- Si guarda il numero di vertici:
		1. si prende il grafo e si guarda ogni vertice
		2. per ogni vertice si guardano i vertici adiacenti a distanza di $i$ archi dove $i$ e' l'$i$-esima esecuzione
	- **Correttezza**: immediata
		- **invariante**: dodo $m-1$ esecuzioni del ciclo piu' esterno, $L^m_ij$ contiene il percorso del cammino minimo da $i$ a $j$ che utilizza al massimo $m$ archi
	- **Complessità**: quattro cicli di lunghezza $|V|$, per un totale di $\Theta(|V|^4)$
- **Versione veloce**:
	- L'ide e' di calcolare la matrice $L$ per ogni potenza di 2 fino a $n-1$ e restituire la matrice $L(n-1)$ che contiene i pesi dei cammini minimi tra tutte le copie di vertici
	- **Correttezza**: ovvia
	- **Complessità**: $\Theta(|V|^3 \cdot log(|V|))$.
		- Questo si deve al fatto che uno dei quattro cicliche c'erano precedentemente e' stato convertito in una ricerca binaria

**Floyd-Warshall** 
- La caratteristica della sottostruttura ottima in questo caso e' fatta attraverso i vertici. Siano $i$ e $j$ due vertici qualsiasi e consideriamo un percorso minimo $i \rightsquigarrow j$. 
	- Consideriamo tutti i vertici diversi da $i$ e $j$ che compaiono su questo percorso: li chiameremo **intermediari**.
- Nel cambiare il focus da numero di archi usabili a quali intermediari usare, si ha un algoritmo più efficiente:
	- mi ricordo che i vertici sono ordinati $1, 2, \ldots, n$
	- caratterizzo il risultato parziale con "usando fino al vertice $k$-esimo"
- **Notazione**: $i \rightsquigarrow_1 j$ oppure $i \rightsquigarrow_2 j$ $\rightarrow$ cammino minimo passando, se serve, dai vertici 1 o 2
- **Osservazione**:
	- il cammino minimo da $i$ a $k$ e quello da $k$ a $j$ non usano $k$ come intermediario
	- il numero dei vertici e' minore del numero di archi quindi l'utilizzo Floyd-Warshall e' piu' efficiente perche' usa come sottostruttura ottima i vertici
- **Complessità**: $\Theta(|V|^3)$ 

---
## Teoria della complessità
- Si considerano solo dei **problemi decisionali** (se output è {si; no}) e non generali

**Classe P**:
- $P = \{\text{problemi che possono essere risolti in tempo polinomiale}\}$ 
- Appartengono tutti i problemi alla classe P?
- Si considera:
	- un grafo indiretto non pesato $G = (V, E)$
	- $k$-clique $\rightarrow$ sottoinsieme di vertici di cardinalita' $k$ t.c. per ogni coppia di vertici esiste un arco che li collega
- Si puo' dunque definire il problema di trovare la massima clique.
- **Correttezza**: immediata
- **Complessità**: $O (2^{|V|})$ $\rightarrow$ esponenziale
- **Non possiamo dire** che il problema della $k$-clique stia nella classe P

**Classe NP**:
- Se avessimo in mano un insieme di vertici di $G$, quanto sarebbe difficile stabilire che si tratta di una $k$-clique?
- $NP = \{ \text{problemi la cui soluzione puo' essere controllata in tempo polinomiale} \}$ 
- $P \subseteq NP$ 
- **Complessità**: nel caso peggiore e' di $O(k^2)$
	- la complessita' dell'algoritmo e' **polinomiale** in essa
- Possiamo dire che $k$-clique si trova nella classe NP.
- Sono infinitamente di piu' i problemi che non appartengono alla classe NP di quelli che vi appartengono
- Un problema in particolare che non appartiene alla classe NP e' quello della geografia generalizzata

**Problema della geografia generalizzata**:
- Dato un grafo $G = (V, E)$, diretto, non pesato ed un vertice $v \in V$ e' possibile definire un gioco come segue:
	- due giocatori (I e II) si alternano al gioco, che comincia I, e consiste, ad ogni turno, nello scegliere un vertice non ancora usato e connesso a quello attuale; 
	- Il giocatore che rimane senza mosse possibili perde e l'altro vince.
- Il problema interessante in questo contesto è trovare una strategia vincente per un giocatore, diciamo I
- Cos'e' la strategia?
	- E' una sequenza di mosse, cioe' una sequenza di vertici.
	- Una strategia e' vincente se, seguendola, il giocatore vince **qualunque cosa faccia l'altro**.
- Per questo problema conviene supporre che $G$ sia rappresentato con liste di adiacenza
- L'algoritmo `HasIWinningStrategy()` funziona in modo tale che, se chiamato un certo vertice $v$ di un certo grafo $G$ con $p=I$, stabilisce se $I$ ha una strategia vincente su $G$ a partire da $v$.
- **Complessità**: $(|V|^2)$ $\rightarrow$ dipende dal numero totale di cammini semplici che esistono in un grafo diretto

**Classe PSPACE**:
- Il problema della geografia generalizzata si trova nella classe di quei problemi la cui soluzione puo' essere trovata/verificata usando al massimo **spazio polinomiale** per la computazione.
- $PSPACE = \{ \text{problemi la cui soluzione puo' essere trovata/verificata in spazio polinomiale} \}$
- $NP \subseteq PSPACE$ 