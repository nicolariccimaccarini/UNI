# INDICE
1. [[#Introduzione e regole]] 
2. [[#Correttezza, complessita' e notazione asintotica]]  
3. [[#Soluzioni di ricorrenze]] 
4. [[#MergeSort e QuickSort]] 
5. [[#CountingSort e RadixSort]] 
6. [[#Liste, pile e code]] 
7. [[#Heaps, heapsort e code di priorità]]
8. [[#Tabelle Hash]] 
9. [[#Strutture dati per insiemi disgiunti]] 
10. [[#Alberi e Alberi binari di ricerca]] 
11. [[#Alberi red-black (RBT)]] 
12. [[#Alberi B (BT)]] 
13. [[#Grafi visita in ampiezza e problemi collegati]] 
14. [[#Grafi visita in profondità e problemi collegati]] 
15. [[#Grafi e alberi di copertura minima]] 
16. [[#Grafi e percorsi minimi con sorgente singola]] 
17. [[#Grafi e percorsi minimi tra tutte le copie di vertici]] 
18. [[#Teoria della complessità accenni ed esempi]] 

--- 

# Introduzione e regole
## Algoritmi e problemi
Le prime tracce degli **algoritmi** risalgono ai Babilonesi, nel VI secolo A.C.
La parola algoritmo deriva dal nome di un matematico persiano del nono secolo, Abu Abd Allah Muhammad ibn Musa al-Khwarizmi. 

Un **algoritmo**, per noi, e' una qualsiasi procedura computazionale che da un **input** produce un **output**. E' necessario un algoritmo per **comunicare** la soluzione a un problema in una maniera formale, verificabile, riproducibile ed implementabile. Inoltre, la relazione tra problema e algoritmo non e' uno a uno! Ci possono essere problemi risolvibili da tanti algoritmi diversi, e problemi per i quali non esiste un algoritmo che lo risolve.

Noi diciamo che un **problema** e' cio' che dobbiamo risolvere, e chiameremo **problema risolvibile** un problema per il quale esiste un algoritmo (sotto certe condizioni) che lo risolve. Un **istanza** e' un particolare input da un problema e una **soluzione** e' l'output che corrisponde ad un particolare input.

## Scrivere algoritmi
In **linguistica** si distinguono tre aspetti di un linguaggio: 
- La **sintassi** --> come strutturo una frase;
- La **semantica** --> cosa significa una frase;
- La **pragmatica** --> lo studio di qual'e' il miglior modo di esprimere un concetto.

Gli algoritmi non sono programmi. I programmi **descrivono** gli algoritmi, nascondendo, di fatto, l'intuizione che soggiace all'algoritmo stesso.
**Tutti i linguaggi di programmazione sufficientemente espressivi sono ugualmente espressivi.** Questo significa che possiamo scegliere un linguaggio o un altro e non perdere nulla in termini di capacita' di risolvere il problema.

Facciamo le seguenti ipotesi. Le istruzioni semplici si eseguono in tempo costante (una unità di tempo): operazioni algebriche sui numeri interi e non, assegnamenti, controlli di condizioni logiche, movimenti semplici in memoria. Tutto il resto non è costante, in particolare i cicli e le chiamate ricorsive o a funzioni da noi precedentemente definite o assunte. I numeri sono rappresentati in base binaria, per cui il numero n occupa c · log(n) bit di memoria per qualche costante c - la parola di memoria è di lunghezza costante. La dimensione dell'input si misura, in generale, come il numero di bit che l'input occupa. Il tempo di computazione è il numero di passi semplici che si impiegano espresso in termini della dimensione dell'input, e tiene conto di costanti che nascondono i dettagli implementativi. Infine, gli array cominciano dalla posizione 1 e non 0,
se non esplicitamente detto il contrario.

## Caratteristiche degli algoritmi
L'esistenza di un algoritmo per un problema ci da un tetto alla complessita' del problema stesso. Per esempio, un algoritmo di ordinamento potrebbe avere complessita' di tempo n * log(n): cio' significa che se una particolare istanza e' lunga *n*, ci si mette circa n * log(n) passi **elementari** per risolverlo


---
# Correttezza, complessita' e notazione asintotica

## Analizzare gli algoritmi
Ci sono quattro caratteristiche fondamentali che ci interessano di un algoritmo:
- **correttezza** --> affermare che esso restituisce sempre una risposta corretta;
- **completezza** --> affermare che ogni risposta corretta e', prima o poi, effettivamente restituita
- **terminazione** --> assicurare che per ogni input la computazione finisca
- **complessità** 

Molti problemi che vedremo sono di tipo **funzionale**: trova il minimo, il massimo, restituisci l'input ordinato, ... In questi casi, distinguere tra le caratteristiche viste prima non e' sempre conveniente ne' naturale. In generale, noi ci riferiremo semplicemente alla correttezza di un algoritmo funzionale, e diremo appunto che esso e' **corretto** solo se restituisce tutte e solamente le risposte giuste, e termina sempre.

### Analizzare gli algoritmi: correttezza nel caso iterativo
Per comprendere formalmente la correttezza degli algoritmi iterativi, partiamo dall'algoritmo di ordinamento di numeri interi, cioe' InsertionSort. 

``` Pseudocodice
proc InsertionSort(A) {
	for (j=2 to A.lenght) {
		key = A[j]
		i = j - 1
		while ((i > 0) and (A[i] > key)) {
			A[i + 1] = A[i]
			i = i + 1
		}
		A[i + 1] = key
	}
}
```

Vogliamo dimostrare la correttezza dell'algoritmo. Per la **terminazione**, osserviamo che il ciclo **for** termina quando `j > A.lenght`, e il ciclo **while** e' sempre vincolato tra j - 1 e 0, e che per ogni j, la variabile i, che inizia da un valore positivo, si decrementa sempre. Dunque la terminazione e' garantita sia a livello del ciclo piu' esterno che di quello piu' interno.
Per la **correttezza** possiamo usare la tecnica dell'**invariante**, che consiste nello stabilire una proprieta' del ciclo principale (o di un ciclo dell'algoritmo) che sia vera dalla prima esecuzione, durante ogni esecuzione, e dopo l'ultima esecuzione, e che implichi la correttezza. Nel nostro caso una **invariante** del ciclo piu' esterno che che `A[1, ..., j-1]` e' sempre ordinato in maniera non decrescente; si noti che quando `j = A.lenght + 1`, l'algoritmo termina, verificando che 
`A[1, ..., n]` e' ordinato in maniera non decrescente.

### Analizzare gli algoritmi: complessita' nel caso iterativo
Passiamo allo studio della **complessità** di InsertionSort,
![[InsertionSortComplexity.png]]

c1, c2, ... sono costanti; n e' la dimensione dell'input; $t_j$ va da 2 a *n* e dipende da istanza a istanza. **Nel caso migliore** $t_j = 1$ per ogni $j = 2, ..., n$ e questo caso corrisponde all'input gia' ordinato in partenza. **Nel caso peggiore** $t_j = j$ per ogni $j = 2, ..., n$ e corrisponde all'input **ordinato in ordine inverso** in partenza. 

Nel caso migliore quindi:
		$T(n) = c_1 * n + c_2 * (n-1) + c_3 * (n-1) + c_7 * (n-1) + c_4 * (n-1)$ 
Quindi $T(n) = a * n + b$ e' una funzione lineare per qualche costante a, b.

Nel caso peggiore invece, succede quanto segue. L'istruzione di controllo del **while** si esegue, ad ogni istanza del ciclo piu' esterno, esattamente $j$ volte; le due istruzioni interne, esattamente $j-1$ volte. Succede che: 
$$ \sum_{j=2}^n j = \frac{n · (n+1)}{2}-1 $$
e che 
$$ \sum_{j=2}^n (j-1) = \frac{n · (n-1)}{2}$$

e pertanto
	$T(n) = C + c_4 · (\frac{n · (n+1)}{2} - 1) + c_5 · (\frac{n · (n-1)}{2}) + c_6 (\frac{n · (n-1)}2)$  

dove la parte $C$ e' rimasta come nel caso migliore.
Il risultato e' quindi una funzione *quadratica* $T(n) = a · n^2 + b · c$ per qualche a, b, c.

Il nostro scopo era calcolare $T(n)$ cioe' il numero di operazioni semplici effettuate per l'entrata di dimensione *n*, e lo abbiamo fatto **a meno di qualche costante**. Il caso migliore non e' mai una buona scelta. Lo faremo sempre nel **caso peggiore** o nel **caso medio**, il quale a sua volta coincide nella maggior parte dei casi nella speranza che il caso medio presenti un comportamento migliore.

### Analizzare gli algoritmi: correttezza nel caso ricorsivo
Per introdurre l'analisi della complessita' di un algoritmo ricorsivo, utilizziamo l'algoritmo di ricerca binaria, *RecursiveBinarySearch* 
``` Pseudocodice
proc RecursiveBinarySerach(A, low, high, k) {
	if (low > high)
		return nil       // OVVERO NULL
	mid = (high + low)/2
	if (A[mid] = k)
		return mid
	if (A[mid] < k)
		return RecursiveBinarySerach(A, mid + 1, high, k)
	if (A[mid] > k)
		return RecursiveBinarySerach(A, low, mid - 1, k)
}
```

Questo algoritmo serve per risolvere il seguente problema: dato un'array, orinato, ed una chiave *k*, restituisce, se esiste, l'indice *k* nell'array, altrimenti restituisce **nil**. *RecursiveBinarySearch* e' un algoritmo interessante perche' molto piu' efficiente dell'idea naïve di scorrere l'array in cerca di *k*.

Come nel caso iterativo, vogliamo assicurare la **terminazione** dell'algoritmo: ad ogni chiamata ricorsiva, la differenza tra high e low diminuisce. La condizione per effettuare un'altra chiamata ricorsiva e' che **low < high**, cosa che, prima o poi, deve necessariamente essere falsa, il che implica che ad un certo punto non ci saranno piu' chiamate.
Per la **correttezza**, utilizziamo la tecnica dell'**invariante introduttiva**. Invece di concentrarci su un ciclo, ci concentriamo su cio' che accade **dopo ogni chiamata ricorsiva**.

Le invarianti ricorsive si mostrano vere attraverso l'induzione:
- Nel **caso base**, cioe' prima della chiamata ricorsiva, $low = 1$ e $high=n$; quindi se *k* e' in A, e' chiaramente in $A[low, ..., high]$.
- Nel **caso induttivo** si assume che *k*, se esiste, sia in $A[low, ..., high]$ (prima della chiamata ricorsiva); poiche' A e' ordinato, e mid è l'indice mediano, se k esiste è $A[mid]$, oppure è alla sua sinistra (se $A[mid] > k$), oppure è alla sua destra (se $A[mid] < k$), e la successiva chiamata ricorsiva, se si effettua, cambia low o high in maniera da rispettare proprio questo principio. Dunque k, se esiste, si trova di nuovo in $A[low, . . . , high]$ all'inizio della seguente chiamata ricorsiva, dove low o high è stato opportunamente cambiato.

**Complessità**. Operare in come *InsertionSort* per operare la complessita' di *RecursiveBinarySearch* non porta a nulla: tutte le operazioni semplici hanno complessita' costante e non ci sono cicli. Ci sono, pero', due chiamate ricorsive che rendono difficile formalizzare il costo totale. $T(n)$, cioe' la nostra funzione di complessita', e' un **incognita**, della quale sappiamo le seguenti cose: costa una costante *c* **prima di dichiararsi**, che possiamo approssimare a 1, e nel **caso peggiore** si richiama esattamente una volta; poiché la variabile mid assume il valore dell'indice centrale dell'array arrotondato, eventualmente, all'intero inferiore, quando la procedura si richiama lo fa, al massimo, sulla metà degli elementi originali.
Formalizzando
$$ T(n) = T(\frac{n}{2}) + 1 $$
Diciamo che l'espressione che descrive $T(n)$ e' una **ricorrenza**, e che questa si risolve trovando l'aspetto esplicito di $T(n)$. 

### Notazione asintotica: principi
Informalmente, data una funzione $T(n)$, diremo che $T(n) = Θ(f(n))$ se e solo se si ottiene da $f(n)$ **eliminando tutti i termini di ordine inferiore al maggiore e tutte le sue costanti**.
E' piu' facile confrontare differenti algoritmi secondo il **loro comportamento nel caso peggiore**.

**Quali sono le operazioni che davvero contano quando si va a calcolare la complessità?**
Osserviamo che la logica di un algoritmo e' data appunto dalle operazioni **logiche**. Quindi, e' naturale contare queste, invece che tutte le operazioni, perche' queste ultime danno un rapporto costante, o comunque limitato dalle operazioni logiche sotto le quali sono inserite.
A seconda del contesto, ci chiederemo quanti sono i **confronti**, o piu' in generale le operazioni logiche di un certo algoritmo. In particolare, quando calcoliamo la complessità asintotica contiamo tutte le operazioni, e quando facciamo una analisi piu' specifica ci concentriamo sulle operazioni logiche.

A questo punto possiamo studiare con maggiore dettaglio il concetto di **ordine di grandezza** di una funzione sui numeri naturali.

Nel caso di *InsertionSort* abbiamo detto che:
$$ T(n) = a·n^2 + b·n + c = Θ(n^2) $$Ma cos'e' $Θ(f(n))$?
Per una funzione $f(n)$ diremo, che $f(n)$ e' **limitata da** (o e' un **o grande** di). $g(n)$ ($f (n) = O(g(n)))$) se e solo se **esiste una costante** $c > 0$ tale che **per tutti gli** $n ≥ n_0$ e' il caso che:
$$ 0 ≤ f (n) ≤ c · g(n) $$
In maniera simile, per una funzione $f(n)$ diremo che $f(n)$ e' **limitata dal basso da** (o e' un **omega grande** di) $g(n)$ ($f (n) = Ω(g(n))$) se e solo se **esiste una costante** $c>0$ tale che **per tutti gli** $n ≥ n_0$ e' il caso che:
$$ 0 ≤ c · g(n) ≤ f (n) $$   
Nel caso piu' semplice, abbiamo, ad esempio, che $f(n) = a · n^k$ e' tale che $f(n) = O(n^k)$, perche', per ogni $n ≥ 0$, si ha che esiste una costante *c* (che e' proprio a) tale che $f(n) ≤ c · n^k$. In questi semplicissimo esempio, abbiamo anche $f(n) = Ω(n^k)$, per la stessa ragione.
Generalizzando, possiamo dire che:
$$ \sum_{i=0}{k} a^i · n^i = Ω(n^k) $$

Naturalmente, $f(n) = O(g(n))$ e $f(n) = Ω(g(n))$ **non** sono definizioni equivalenti. Per esempio, se prendiamo il caso di $f(n) = log(n)$ (come sempre, non specificando altrimenti, usiamo logaritmi in base 2). Non e' vero che $log(n) ≥ c · n$ per $n ≥ n_0$ per nessuna scelta di *c* e di $n_0$. Quindi $log(n) =/  Ω(n)$.

Arriviamo adesso alla nozione che avevamo al principio. Per una funzione $f(n)$, diremo che $f(n)$ e' **dello stesso ordine** di $g(n)$ se e solo se **esistono due costanti** $c_1, c_2 > 0$  tali che, per qualche $n_0$, **per tutti gli** $n ≥ n_0$ e' il caso che:
$$ 0 ≤ c_1 · g(n) ≤ f (n) ≤ c2 · g(n) $$ In altre parole l'insieme $Θ(g(n))$ contiene tutte e solo quelle funzioni $f(n)4 tali che, a partire da un certo momento in poi (per tutti gli $n ≥ n_0$), il valore di $f(n)$ e' descrittivo al valore $g(n)$ a meno di una costante. Quindi, asintoticamente, $f(n)$ si comporta come $g(n)$.

Chiaramente:
$$ f(n) = Θ(g(n)) $$
se e solo se:
$$ f (n) = O(g(n)) e f (n) = Ω(g(n)) $$
![[NotazioneAsintoticaGrafici.png]]

Per confrontare gli ordini di grandezza di funzioni diverse, usiamo la notazione $o()$ e $ω()$ (**o piccolo** e **omega piccolo**).
Diremo quindi che $f(n) = o(g(n))$ (**e' di un ordine inferiore a**) se e sollo se per ogni costante $c > 0$, esiste una costante $n_0$ tale che, per ogni $n > n_0$ si verifica che $0 ≤ f (n) ≤ c · g(n)$ . Questo si puo' vedere in maniera piu' semplice: $lim_n→∞ f(n)/g(n) = 0$.
Analogamente, $f(n) = ω(g(n))$ (**e' di un ordine superiore a**) se e solo se per ogni costante $c > 0$, esiste una costante $n_0$ tale che, per ogni $n > n_0$ si verifica che $0 ≤ c · g(n) ≤ f (n)$. Piu' semplicemente: $lim_n→∞ f(n)/g(n) = ∞$.  

--- 
# Soluzioni di ricorrenze
Per ogni $T(n)$ **esplicita** (come ad esempio *InsertionSort*) possiamo trovare il suo ordine di grandezza (*InsertionSort* nel caso peggiore ha complessita' $Θ(n^2)$).
Esistono vari metodi per trasformare una ricorrenza da **implicita** a **esplicita**. L'esempio che dobbiamo ancora risolvere e' quello di *RecursiveBinarySearch*, che ha complessita', nel caso peggiore, $T(n) = T(\frac{n}{2})+1$. 
Esistono tre modi per la soluzione di ricorrenze:
- il metodo **dell'albero di ricorsione** (o sviluppo in serie)
- il **Master Theorem**
- il metodo della **sostituzione** (o induzione)

## Soluzione di ricorrenze: sviluppo
Il metodo dello sviluppo si basa sull'idea di **sviluppare** la ricorrenza per cercare di estrarne il comportamento.
Se ad esempio $T(n) = T(frazione-di-n) + f(n)$, allora per calcolare *T(frazione di n)* posso ri-applicare la stessa ricorrenza, e ottenere una forma, ancora implicita, ma con un termine in piu'.

Supponiamo di risolvere $T(n) = T(\frac{n}{2})+1$
Da $T(n) = T(\frac{n}{2})+1$, abbiamo che:
$$
\begin{align*}
	T(n) &= T(\frac{n}{2}) + 1 \\
	&= (T(\frac{n}{4}) + 1) + 1 = T(\frac{n}{4}) + 2 \\
	&= (T(\frac{n}{8}) + 1) + 2 = T(\frac{n}{8}) + 3 \\
	...,
\end{align*}
$$

finche' e' possibile dividere, cioe' finche' l'algoritmo della *T* diventa 1 o meno (e quindi non si puo' piu' dividere). Assumendo che $T(1)=1$, si ottiene che $T(n) = ⌊log(n)⌋ + 1 = Θ(log(n))$. Questa e' una forma semplice di **albero di ricorsione**, che ha un solo ramo.

Consideriamo, come altro esempio, la ricorrenza $T(n) = 2· T(\frac{n}{2}) + n$.
Sviluppando, otteniamo:
$$
\begin{align*}
	T(n) &= 2 · T(\frac{n}{2}) + n \\
	&= 2 · (2 · T(\frac{n}{4}) + \frac{n}{2}) + n = 4 · T(\frac{n}{4}) + 2 · n \\
	&= 4 · (2 · T(\frac{n}{8}) + \frac{n}{4}) + n = 8 · T(\frac{n}{8}) + 3 · n \\
	...,
\end{align*}
$$

finche' e' possibile dividere, cioe' finche' l'algoritmo diventa 1 o meno. 
Assumendo che $T(1)=1$, otteniamo $T(n) = 2^{log(n)} + n · log(n)$, cioe' $T(n) = n + n · log(n)$, cioe' $T(n) = Θ(n · log(n))$. 

Sviluppiamo adesso come ultimo esempio, la ricorrenza $T(n)=3·T(n/2)+n^2$. Otteniamo:
$$ 
\begin{align*}
	T(n) &= 3 · T(\frac{n}{2}) + n^2 \\
	&= 3 · ( 3 · T(\frac{n}{4}) + (\frac{n}{2})^2) + n^2 = 9 · T(\frac{n}{4}) + \frac{3}{4} · n^2 + n^2 \\
	&= 9 · ( 3 · T(\frac{n}{8}) + (\frac{n}{4})^2) + n^2 = 27 · T(\frac{n}{8}) + ... \\
	...
\end{align*}
$$
Arrivando alla seguente forma, sempre sotto ipotesi che $T(1)=1$:
$$ 3^{log(n)} + \sum_{i=0}^{log(n)-1}(\frac{3}{4})^i · n^2 = n^{log(3)} + n^2 · \sum_{i=0}^{log(n)-1}(\frac{3}{4})^i = Θ(n^2) $$
ricordando che:
$$ x^{log(y)} = (2^{log(x)})^{log(y)} = 2^{log(y)·log(x)} = (2^{log(y)})^{log(x)} = y^{log(x)} $$

## Soluzione di ricorrenze: il Master Theorem
Generalizzando quanto visto, ci accorgiamo che esiste una certa **forma** di ricorrenza tale che, se la ricorrenza ha quella forma, la sua soluzione **sembra** abbastanza semplice. Questa forma, in generale, e':
$$ T(n) = a · T(\frac{n}{b}) + f(n)$$

Infatti, le ricorrenze in questa forma si possono facilmente sviluppare e/o testare per sostituzione. Possiamo generalizzare questo concetto ed evitare di ripetere molti passi?
La prima osservazione e' che se sviluppiamo **in modo generale** questa ricorrenza otteniamo:
$$ \sum_{i=0}^{lob_b(n)-1}a^i · f(\frac{n}{b^i}) + Θ(n^{log_b(a)}) $$ sotto ipotesi che *n* sia **potenza esatta** di *b*. I ragionamenti che seguono possono essere ulteriormente generalizzati quando questa ipotesi non e' rispettata.

Supponiamo adesso che $f(n)$ sia ordine **polinomicamente** inferiore a $n^{log_b(a)}$; questo si scriverebbe: $f(n)=O(n^{log_b}(a)-ε)$ per un certo $ε>0$. In questo caso, valutiamo la prima parte dell'espressione precedente:
$$
\begin{align*}
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &= O(\sum_{i=0}^{log_b(n)-1} a^i · (\frac{n}{b^i})^{log_b(a) - ε)} \qquad &\text{sostituzione} \\ 
&=O(\sum_{i=0}^{log_b(n)-1} \frac{a^i · n^{log_b(a)-ε}}{b^{i · (log_b(a)-ε)}}) \qquad &\text{moltiplicazione} \\
&= O(n^{log_b(n)-ε} · \sum_{i=0}^{log_b(n)-1} \frac{a^i·b^{i·ε}}{b^{i·log_b(a)}}) \qquad &\text{estrazione} \\
&= O(n^{log_b(n)-ε} · \sum_{i=0}^{log_b(n)-1} (\frac{a·b^ε}{b^log_b(a)})^i) \qquad &\text{esponente} \\
&= O(n^{log_b(n)-ε} · \sum_{i=0}^{log_b(n)-1} (b^ε)^i) \qquad &\text{prop. logaritmi} \\
\end{align*}
$$
Ci accorgiamo che la sommatoria e' nota: si tratta di una sommatoria geometrica, che sappiamo sempre valutare.

Quindi:
$$
\begin{align*}
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &= O(n^{log_b(n)-ε} · \frac{b^{ε·log_b(n)}-1}{b^ε-1} ) \qquad &\text{somma geometrica} \\
&= O(n^{log_b(n)-ε} · \frac{n^ε-1}{b^ε-1}) \qquad &\text{prop. logaritmi} \\
&= O(n^{log_b(a)}) \qquad &\text{moltip. e diff. di O()}
\end{align*}
$$

Mettendo tutto nell'espressione dalla quale siamo partiti:
$$ T(n) = O(n^{log_b(a)}) + Θ(n^{log_b(a)}) = Θ(n^{log_b(a)}) $$ 
Cosa succederebbe se $f(n) = Θ(n^{log_b(a)})$? Avremmo che
$$
\begin{align*}
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &= Θ(\sum_{i=0}^{log_b(n)-1} a^i · (\frac{n}{b^i})^{log_b(a)}) \qquad &\text{sostituzione} \\
&= Θ( n^{log_b(a)} · \sum_{i=0}^{log_b(n)-1} \frac{a^i}{b^{i·log_b(a)}} ) \qquad &\text{moltiplicazione} \\
&= Θ( n^{log_b(a)} · \sum_{i=0}^{log_b(n)-1} -1) \\
&= Θ( n^{log_b(a)} · log_b(n))
\end{align*}
$$

E di nuovo, mettendo tutto nell'espressione dalla quale siamo partiti:
$$ T(n) = Θ(n^{log_b(a)} · log_b(n)) + Θ(n^{log_b(a)}) = Θ(n^{log_b(a)} · log_b(n)) $$

Supponiamo, infine, che $f(n) = Ω(n^{log_b(a)+ε})$ (cioe' *f(n)* polinomicamente di grado superiore a $n^{log_b(a)}$), che implica $f(n) = Ω(n^{log_b(a)})$. Immaginiamo anche $a · f(\frac{n}{b}) ≤ c · f (n)$ per qualche $c < 1$ e tutti gli $n ≥ n_0$. Allora:
$$
\begin{align*}
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &≤ \sum_{i=0}^{log_b(n)-1} c^i · f(n) \\
&≤ f(n) · \sum_{i=0}^\infty c^i \qquad \text{maggiorazione} \\
&= f(n) \cdot \frac{1}{1-c} \qquad \text{somma di serie geometrica} 
\end{align*}
$$
Pertanto:
$$ \sum_{i=0}^{log_b(n)}a^i · f(\frac{n}{b_i}) = Θ(f (n)) $$
Ma questo implica:
$$ T(n) = Θ(f(n)) + Θ(n^{log_b(a)}) = Θ(f(n)) $$
considerato che $f(n)$ e' polinomicamente di grado superiore a $n^{log_b(a)}$, di nuovo, per ipotesi.

Se mettiamo tutto assieme, generalizzando per *n* **non necessariamente** potenza perfetta di *b*, e intendendo $\frac{n}{b}$ come $⌊\frac{n}{b}⌋$ oppure $⌈\frac{n}{b}⌉$, indifferentemente, otteniamo che se:
$$ T(n) = a · T(\frac{n}{b}) + f(n) $$
allora questa puo' essere risolta attraverso l'applicazione di quello che e' diventato noto come il **Master Theorem** per le ricorrenze.
$$  
T(n) = 
\left\{\begin{align} 
\Theta(n^{log_b(a)}) \qquad &\text{se} \quad f(n)=O(n^{log_b(a)-\epsilon}) \quad \text{per qualche } \epsilon>0 \quad &\text{caso I} \\
\Theta(n^{log_b(a)} \cdot log^{k+1}(n)) \qquad &\text{se} \quad f(n) = \Theta(n^{log_b(a)} \cdot log^k(n)) \quad \text{per qualche } k>0 \quad &\text{caso II} \\
\Theta(f(n)) \qquad &\text{se} \quad f(n) = \Omega(n^{log_b(a)+\epsilon}) \quad \text{e se esiste} \quad c \lt 1 \text{ t.c. } a \cdot f(\frac{n}{b}) \le c \cdot f(n) \quad \text{per ogni } m \ge n_0 \quad &\text{caso III}
\end{align} \right.
$$

Usiamo il teorema per la ricorrenza, $T(n) = 2 \cdot T(\frac{n}{2}) + n$. Abbiamo che il caso corretto e' il numero 2, con a=b=2, k=0, $f(n)=n$, per ottenere:
$$
T(n) = \Theta (n \cdot log(n))
$$
Usiamo il teorema per la ricorrenza $T(n) = 4 \cdot T(\frac{n}{2}) + n^2 \cdot log(n)$. Abbiamo, che il caso corretto e' il numero 2, con a=4, b=2, k=1, $f(n)=n^2 \cdot log(n)$, per ottenere:
$$
T(n) = \Theta(n^2 \cdot log^2(n))
$$

## Soluzione di ricorrenze: se il Master Theorem non basta
Cosa accade se la ricorrenza **non** ha la forma corretta per il Master Theorem?
Sebbene in questi casi si possa sempre utilizzare lo sviluppo (ma non il teorema che generalizza la tecnica), a volte si trovano ricorrenze risolte per **sostituzione**. Questa tecnica consiste nell'**indovinare** il risultato e poi dimostrare che questo e' corretto con induzione.

## Soluzione di ricorrenze: sostituzione
Possiamo come primo esempio **verificare** la soluzione di $T(n) = T(⌈\frac{n}{2}⌉)$. Questa ricorrenza potrebbe essere trattata con il Master Theorem nel quale gli arrotondamenti (come ⌈⌉ o ⌊⌋) possono essere ignorati.
Sostituzione significa fare un **ipotesi** che spiega $T(n) = O(log(n))$, e poi verificarla per induzione.
Domandiamoci: perche' $T(n) = O(log(n))$? Ci sono tante possibili ipotesi:
- $T(n) \le c \cdot log(n)$ per qualche $c \gt 0$;
- $T(n) \le c \cdot log(n) ± d$ per qualche $c,d \gt 0$;
- $T(n) \le c \cdot log(n ± d)$ per qualche $c,d \gt 0$;
- ...

Immaginiamo ad esempio che $T(n) \le c \cdot log(n)$; allora si ha:
$$
\begin{align}
T(n) &= T(\lceil\frac{n}{2}\rceil) + 1 \\
T(n) &\le c \cdot log(\lceil\frac{n}{2}\rceil) + 1 \qquad &\text{ipotesi induttiva} \\
&\le c \cdot log(\frac{n+1}{2}) + 1 \qquad &\text{maggiorazione} \\
&\le c \cdot log(n+1) - c \cdot log(2) + 1 \qquad &\text{proprieta' logaritmi} \\
&\le^? c \cdot log(n) \qquad &\text{NON FUNZIONA}
\end{align}
$$
Non funziona perche' non c'e' nessuna scelta di *c* che rende vero $c \cdot log(n+1) -c+1 \le c \cdot log(n)$. 

Non esiste una strategia sicura per costruire la giusta ipotesi. Immaginiamo che $T(n) \le c \cdot log(n-1)$; allora si ha:
$$
\begin{align}
T(n) &= T(\lceil\frac{n}{2}\rceil) + 1 \\
T(n) &\le c \cdot log(\lceil\frac{n}{2}\rceil -2) + 1 \qquad &\text{ipotesi induttiva} \\
&\le c \cdot log(\frac{n+1}{2} + \frac{1}{2} -2) + 1 \qquad &\text{maggiorazione} \\
&\le c \cdot log(\frac{n-2}{2}) + 1 \qquad &\text{algebra} \\
&\le c \cdot log(n-2) -c \cdot log(2) + 1 \qquad &\text{algebra} \\
&\le c \cdot log(n-2) \qquad &\text{maggiorazione}
\end{align}
$$

Come altro esempio, cerchiamo di mostrare che $T(n)=2 \cdot T(\lfloor\frac{n}{2}\rfloor) + n$ ha come soluzione $O(log(n))$. Assumiamo che $T(n) \le c \cdot n \cdot log(n)$ per qualche $c \gt 0$, e si ha:
$$
\begin{align}
T(n) &= 2 \cdot T(\lfloor\frac{n}{2}\rfloor) + 2 \qquad &\text{ricorrenza} \\
&\le 2 \cdot (c \cdot \lfloor\frac{n}{2}\rfloor \cdot log(\lfloor\frac{n}{2}\rfloor)) + n \qquad &\text{ipotesi induttiva} \\
&\le c \cdot n \cdot log(\frac{n}{2}) + 2 \qquad &\text{algebra e maggiorazione} \\
&= c \cdot n \cdot log(n) - c \cdot n \cdot log(2) + n \qquad &\text{proprieta' di log()} \\
&= c \cdot n \cdot log(n) - c \cdot n + n \qquad &\text{log(2) = 1} \\
&\le c \cdot n \cdot log(n) \qquad &\text{maggiorazione, con } c \ge 1
\end{align}
$$

## Soluzione di ricorrenze: se il Master Theorem non basta
Proviamo ad applicare la sostituzione ad un caso reale di ricorrenza che non puo' essere risolta dal Master Theorem.
Nel caso della ricorrenza $T(n)=T(\frac{n}{3}) + T(\frac{2}{3} \cdot n) + n$, usiamo il seguente sviluppo:

$$
\begin{align}
T(n) &= T(\frac{n}{3}) + T(\frac{2 \cdot n}{3}) + n \\
&= T(\frac{n}{9}) + T(\frac{2 \cdot n}{9}) + \frac{n}{3} + T(\frac{2 \cdot n}{9}) + T(\frac{4 \cdot n}{9}) + \frac{2 \cdot n}{3} + 3 \\
&= T(\frac{n}{27}) + T(\frac{2 \cdot n}{27}) + \frac{n}{9} + 2 \cdot (T(\frac{2 \cdot n}{27}) + T(\frac{4 \cdot n}{27}) + \frac{2 \cdot n}{9}) + T(\frac{4 \cdot n}{27}) + T(\frac{8 \cdot n}{27}) + T(\frac{8 \cdot n}{27}) + \frac{4 \cdot n}{27} + n + n = \\
...
\end{align}
$$

Sotto le solite ipotesi, si ottiene
$$
\begin{align}
T(n) &= 2^{log_{\frac{3}{2}}(n)} + \sum_{i=0}^{log_{\frac{3}{2}}(n)-1} n \\
&= n^{log_{\frac{3}{2}}(2)} + n \cdot \sum_{i=0}^{log_{\frac{3}{2}}(n)-1} 1 = n^{log_{\frac{3}{2}}(2)} + n \cdot log_{\frac{3}{2}}(n)
\end{align}
$$

In questo esempio stiamo facendo l'ipotesi che tutti i rami di ricorsione siano lunghi quanto il piu' lungo di essi (alcuni rami dividono per 3, altri dividono per $\frac{2}{3}$, quindi in realta' non e' cosi'). Inoltre siamo giunti ad un'espressione non facile da valutare. Una strategia, a questo punto, potrebbe essere ipotizzare che valga:
$$
T(n) = O(n \cdot log(n))
$$
e utilizzare la sostituzione per verificarlo. In questo caso **non** siamo autorizzati ad utilizzare la notazione $\Theta()$ a meno di non utilizzare la sostituzione una seconda volta.

Per assicurarci dunque che la soluzione di $T(n) = T(\frac{n}{3}) + T(\frac{2}{3} \cdot n) + n$ sia corretta, assumiamo che $T(n) \le c \cdot n \cdot log(n)$ per qualche $c \gt 0$, e si ha:
$$
\begin{align}
T(n) &=T(\frac{n}{3}) + T(\frac{2}{3} \cdot n) + 2 \qquad &\text{ricorrenza} \\
&\le c \cdot \frac{n}{3} \cdot log(\frac{n}{3}) + c \cdot \frac{2 \cdot n}{3} \cdot log(\frac{2 \cdot n}{3}) + n \qquad &\text{ipotesi induttiva} \\
&\le c \cdot \frac{n}{3} \cdot (log(n) - log(3)) + c \cdot \frac{2 \cdot n}{3} \cdot (log(2) + log(n) - log(3)) + n \qquad &\text{proprieta' di log()} \\
&\le c \cdot \frac{n}{3} \cdot log(n) -c \cdot \frac{n}{3} \cdot log(3) + c \cdot \frac{2 \cdot n}{3} \cdot log(n) - c \cdot \frac{2 \cdot n}{3} \cdot log(3) + n \qquad &\text{algebra} \\
&\le c \cdot n \cdot log(n) - (c \cdot n \cdot log(3) - c \cdot \frac{2 \cdot n}{3} - n) \end{align}
$$

Questo ci porta a esprimere una condizione su *c* ; poiche' questa condizione e' compatibile con $c \gt 0$, la proprieta' e' verificata e possiamo concludere che $T(n) = O(n \cdot log(n))$.

## Soluzione di ricorrenze: riassunto
Una strategia per una ricorrenza puo' essere delineata come segue: come primo step proviamo il Master Theorem, e se non funziona (o la forma della ricorrenza non lo permette), sviluppiamo in serie per poi verificare, se necessario (oppure, se lo sviluppo e' troppo complesso e sono stati necessari dei passi di approssimazione), il risultato con una o piu' sostituzioni.

---

# MergeSort e QuickSort 

## Algoritmi di ordinamento
Il problema dell'ordinamento di un array e' un mattone fondamentale dell'algoritmica. Non solo appare come problema in innumerevoli applicazioni dirette, ma e' anche particolarmente adatto a fare emergere alcune delle idee piu' importanti dell'algoritmica. 

Chiameremo **elementari** quei metodi, basati sui confronti, che condividono la seguente caratteristica: ogni elemento viene spostato verso il/al posto giusto separatamente dagli altri, in maniera iterativa o ricorsiva. Questi metodi includono *InsertionSort*, *SelectionSort*, *BubbleSort*, e altri.

## Alternative a *InsertionSort*: *SelectionSort*
*SelectionSort* e' un'alternativa elementare a *InsertionSort* per l'ordinamento:
``` Pseudocodice
proc SelectionSort(A){
	for (j=1 to A.lenght-1) {
		min = j
		for (i=j+1 to A.lenght) {
			if (A[i] < A[min])
				then min=i
		}
		SwapValue(A, min, j)
	}
}
```

Nel codice di *SelectionSort* abbiamo usato una procedura *SwapValue()* che, dati due indici dell'array, ne scambia i contenuti. Questa procedura ha costo costante.

## Algoritmi di ordinamento non elementari
Tipicamente, gli algoritmi di ordinamento non elementari che non fanno uso di ipotesi aggiuntive sono ricorsivi. Nel nostro programma ne vediamo tre: *MergeSort*, *QuickSort* e *HeapSort*. 

## *MergeSort* e *Merge* 
Per la definizione dell'algoritmo *MergeSort*, studiamo prima un algoritmo che risolve un problema piu' semplice, cioe' quello di costituire una sequenza ordinata partendo da due sequenze ordinate.
L'input e' un array A di interi e tre indici p, q, r su di esso, tali che $p \le q \lt r$ e che entrambi $A[p, ..., q]$ e $A[q+1, ..., r]$ sono ordinati. Vogliamo, come risultato, una permutazione di A tale che $A[p, ..., r]$ e' ordinato.

``` Pseudocodice
proc Merge(A, p, q, r) {
	n1 = q - p + 1
	n2 = r - q
	let L[1, ..., n1] and R[1, ..., n2] be new array      // dichiarazione e allocazione
	// inizializzazione di L e R
	for (i=1 to n1) L[i] = A[p+i-1]
	for (j=1 to n2) R[j] = A[q+j]
	i = 1
	j = 1
	// MERGE
	for (k=p to r) {
		if (i <= n1) then { 
			if (j <= n2) then {
				if (L[i] <= R[j]) then {
					CopyFromL(i)
				} else {
					CopyFromR(j)
				}
			} else {
				CopyFromL(i)
			}
		} else {
			CopyFromR(j)
		}
	}
}
```

L'idea alla base di *Merge* e' scegliere il minimo trai i due elementi in cima a L ed a R e posizionarlo nel giusto posto di A; in seguito, a seconda della scelta, si avanza su L o su R. Utilizziamo due procedure elementari chiamate *CopyFromL* e *CopyFromR* che hanno come effetto quello di copiare in A un elemento (quello il cui indice e' dato come parametro) di L (rispettivamente, R) e avanzare di una posizione la variabile indicata come parametro. La scelta di usare oggetti esterni L ed R e; molto conveniente per poter usare A come array di arrivo, ma questo significa che, **nella nostra implementazione**, ==*Merge* non e' in place==: quanto e' piu' grande l'input, maggiore e' la quantita' di spazio utilizzata da L e R, che pertanto non e' costante. Chiaramente ==*MergeSort*== eredita questa caratteristica, quindi ==non e' in place==.

### Correttezza e complessita' di *Merge* 
Studiamo la **terminazione**. I primi due cicli **for** terminano quando i=n1 e j=n2; il terzo ciclo **for** e' sempre vincolato tra *p* e *r*, e per ipotesi $p \lt r$. Pertanto *Merge* termina sempre quando la chiamata rispetta l'ipotesi.
Per la **correttezza**, la nostra **invariante** e': nel terzo ciclo **for**, $A[p, ..., k-1]$ contiene k-p elementi piu' piccoli di $L[1, ..., n_1]$ e di $R[1, ..., n_2]$, ordinati; inoltre $L[i]$ e $R[j]$ sono i piu' piccoli elementi di L ed R non ancora copiati in A. Per dimostrare che l'invariante va bene, dobbiamo mostrare che dalla stessa si ottiene la proprieta' desiderata sull'output.

Adesso mostriamo che l'invariante vale.
- **Caso base**: k=p. In questo caso $A[p, ..., k-1]$ e' vuoto. Si verifica ce contiene i k-p=0 elementi piu' piccoli di L e R, e poiche' i=j=1 si ha che $L[i]$ ed $R[j]$ sono gli elementi piu' piccoli dei rispettivi oggetti non ancora ricopiati in A.
- **Caso induttivo**: si assume che la proprieta' valga per *k* e si dimostra che vale per k+1. All'inizio della iterazione k+1, $L[i]$ ed $R[j]$ sono gli elementi piu' piccoli dei rispettivi oggetti non ancora ricopiati in A (ipotesi induttiva). Se $L[i] \le R[j]$ (e $i, j \le n_1, n_2$), allora $L[i]$ e' il piu' piccolo elemento non ancora copiato in A. Poiche' $A[p, . . . , k − 1]$ sono gli elementi piu' piccoli e sono ordinati (ipotesi induttiva) e poiche' $L[i]$ si copia in $A[k]$, dopo la k + 1-esima iterazione $A[p, . . . , k]$ contiene gli elementi piu' piccoli, ordinati, e $L[i]$ è l'elemento piu' piccolo di L (giacche' i si è incrementato). Il caso $L[i] \gt R[j]$, e i casi in cui i o j sono n1 o n2, rispettivamente, si dimostrano in maniera simile.

Per completare lo studio di *Merge*, manca ancora da vedere la **complessità**. I primi due cicli **for** impiegano $\Theta(n_1)$ e $\Theta(n_2)$, ed entrambi sono equivalenti a $\Theta(n)$. Il terzo ciclo **for** impiega $\Theta(n)$. Poiche' $\Theta(n) + \Theta(n) = \Theta(n)$, quest'ultima e' la complessita' di *Merge*.

## Ordinamento con *MergeSort* 
L'algoritmo di *MergeSort* presenta un codice molto semplice, pero' e' **ricorsivo**. Queto implica che il calcolo della complessità portera' a una ricorrenza. La caratteristica di essere ricorsivo spiega anche che nell'input dell'algoritmo appaiano due parametri *p, r* che sembrano non avere senso, quando il problema dell'ordinamento e' definito su tutto l'array di input. Infatti dobbiamo pensare che l'algoritmo **richiama se stesso** durante la computazione; quindi da un punto di vista dell'utente, dobbiamo solo capire qual'e' la giusta chiamata iniziale.

``` Pseudocodice
proc MergeSort(A, p, r) {
	if (p < r) then {
		q = [(p+r)/2]
		MergeSort(a, p, q)
		MergeSort(A, q+1, r)
		Merge(A, p, q, r)
	}
}
```
![[MergeSortAlgorithm.png]]

**Terminazione**: ad ogni chiamata ricorsiva, la distanza tra *p* e *r* diminuisce. Si effettua una nuova chiamata solo quando $p \lt r$. Adesso vediamo la **correttezza**, che si basa su un'invariante ricorsiva (o induttiva). In questo caso affermiamo che, dopo ogni chiamata ricorsiva *MergeSort(A, p, r)*, $A[p, ..., r]$ e' ordinato.

Dobbiamo far vedere, per induzione, che l'invarante e' corretta.
- **Caso base**. Quando p=r, non si effettuano chiamate; ma $A[p, ..., r]$ contiene una sola posizione, ed e' quindi ordinato.
- **Caso induttivo**. Supponiamo quindi che $p \lt r$, e ragioniamo sulla chiamata k+1-esima. Questa corrisponde a due chiamate a *MergeSort*, entrambe a livello *k* della ricorsione. Per ipotesi induttiva, entrambe restituiscono un array ordinato. Questi due risultati vengono poi passati a *Merge*, il quale restituisce un array ordinato tra le posizioni *p* ed *r*.

Stabiliamo adesso la **complessità** di questo algoritmo. Come in *Merge*, non ha senso chiedersi se ci sono degli input che migliorano, o peggiorano le sue prestazioni. *MergeSort* da luogo alla seguente ricorrenza:
$$
T(n) = 2 \cdot T(\frac{n}{2}) + \Theta(n)
$$
Seguendo il Master Theorem, questa si esplicita in:
$$
T(n) = \Theta(n \cdot log(n))
$$

La funzione $n \cdot log(n)$ e' chiamata **pseudo lineare** (piu' efficiente rispetto a $n^2$)

## *QuickSort* e *Partition*
Come *MergeSort*, si tratta di un algoritmo ricorsivo che usa una procedura ausiliare, chiamata *Partition*, che permette di formulare diverse soluzioni a problemi che sono considerati **satellite** rispetto all'ordinamento.

L'algoritmo *Partition* ha l'effetto di generare una permutazione dell'array in input in maniera tale che tutti gli elementi piu' piccoli o uguali ad un elemento, chiamato **pivot** e che viene individuato arbitrariamente sullo stesso, si trovino a sinistra di esso, e tutti gli elementi piu' grandi a destra. L'idea piu' importante di questo algoritmo e' il fatto che questa permutazione venga trovata in tempo lineare e senza spazio aggiuntivo.

``` Pseudocodice
proc Partition(A, p, r) {
	x = A[r]
	i = p - 1
	for (j=p to r-1) {
		if (A[j] <= x) then {
			i = i + 1
			SwapValue(A, i, j)
		} 
		SwapValue(A, i+1, r)
	}
	return i+1
}
```

Per osservare che *Partition* vale la **terminazione** osserviamo che il ciclo principale e' un **for**, che noi sappiamo termina sempre.
Per quanto riguarda la **correttezza** usiamo la seguente **invariante**: all'inizio di ogni iterazione, per ogni indice *k*: se $p \le k \le i$, allora $A[k] \le x$; se $i + 1 ≤ k ≤ j − 1$, allora $A[k] \gt x$ e se $k=r$, allora $A[k] = x$. Non possiamo dire nulla sul contenuto di A da j in poi, in quanto non siamo ancora arrivati a quel punto del ciclo.
Alla terminazione ($j=r$); applicando l'invariante, dopo l'ultima istruzione si ottiene un array che , tra le posizioni *p* ed *r*, e' tale che esiste un valore **pivot** ($i + 1$) e tutti i valori di A prima di $i+1$ sono minori o uguali al valori $A[i+1]$ il quale a sua volta e' minore o uguale a tutti i valori dopo la posizione $i+1$.

### Correttezza di *Partition*
Dimostriamo la validita' dell'invariante. L'induzione e' sulla variabile j.
- **Ciclo base**. Osserviamo cosa accade prima della prima esecuzione del ciclo **for**. Si ha che $i=p-1$ e $j=p$, quindi la parte di A fino alla posizione $j-1$ (cioe' $p-1$) e' vuota, pertanto l'invariante predica trivialmente su un insieme vuoto ed e' vera.
- **Caso induttivo**. Prima della j-esima esecuzione, l'elemento j-esimo non e' stato ancora visto, e l'invariante predica su tuti gli elementi fino al j-1-esimo compreso. Sul destino del j-esimo elemento decide la condizione $A[j] \le x$. Supponiamo che sia vera: quindi l'elemento $A[j]$ va spostato nel primo gruppo. Per fare questo, i avanza di uno e si scambiano le posizioni $i+1$ e j. Quindi, a fine di questo ciclo, il gruppo di sinistra ha un elemento in piu' (prima condizione dell'invariante) e il gruppo di destra ha lo stesso numero di elementi, ma j e' avanzato di una posizione. Se si verificasse che $A[j] \gt x$, *i* non si sposterebbe (mantenendo lo stesso numero di elementi a sinistra), ma *j* si, aumentando il gruppo a destra di un elemento.

### Ordinamento con *QuickSort*
L'idea alla base di *QuickSort* e' procedere ricorsivamente (divide and conquer) nella seguente maniera. Un array A, considerato dalla posizione *p* alla posizione *r*, viene prima separato in due sotto-strutture $A[p, ..., q-1]$ e $A[q, ..., r]$ in maniera che tutti gli elementi **prima** di *q* siano minori o uguali a $A[q]$, il quale, a sua volta, sia minore o uguale a tutti gli elementi dalla posizione $q+1$ in poi, attraverso *Partition*. Quindi, richiamiamo ricorsivamente l'algoritmo su $A[p, ..., q-1]$ e $A[q, ..., r]$: al ritorno dalla chiamata ricorsiva, entrambi sono ordinati e pertanto non c'e' bisogno di alcuna operazione per combinarli. Abbiamo cosi' ordinato $A[p, ..., r]$.

``` Pseudocodice
proc QuickSort(A, p, r) {
	if (p < r) then {
		q = Partition(A, p, r)
		QuickSort(A, p, q-1)
		QuickSort(A, q+1, r)
	}
}
```

### Correttezza di *QuickSort*
La correttezza di *QuickSort* puo' essere dimostrata banalmente a partire da quella di *Partition*. La sua **terminazione** dipende dal fatto che, come in *MergeSort*, la distanza tra *p* ed *r* diminuisce sempre perche' *q* cade sempre in mezzo ai due. Quindi prima o poi non ci saranno piu' chiamate ricorsive e il processo terminera'. Per la **correttezza** abbiamo la seguente **invariante ricorsiva**: al termine di ogni chiamata ricorsiva con indici *p, r*, $A[p, ..., r]$ e' ordinato.

Dimostriamo la validita' dell'invariante:
- il **caso base** e' triviale: quando $p=r$, $A[p, ..., r]$ e' compreso da una sola posizione. ed e' quindi ordinato.
- Nel **caso induttivo**, si considerino certi indici $p \lt r$. La prima operazione consiste nel chiamare *Partition*, che abbiamo mostrato essere corretto, e che restituisce un indice *q* e una permutazione di $A[p, ..., r]$ tale che $A[p, ..., q-1]$ contiene solo gli elementi piu' piccoli o uguali a $A[q]$, il quale a sua volta e' piu' piccolo di $A[q+1, ..., r]$. Dopo le due chiamate ricorsive, per ipotesi induttiva, $A[p, ..., q-1]$ e $A[q+1, ..., r]$ sono entrambi ordinati. Ma allora $A[p, ..., r]$ e; ordinato, come volevamo dimostrare.

### Complessita' di *QuickSort*
La **complessità** di *Partition* e' facile da calcolare: $\Theta(n)$. Il calcolo della complessita' di *QuickSort*, invece, presenta molti problemi. 
Per cominciare, analizziamo il caso peggiore. Il primo problema e' stabilire qual'e' la situazione che ci porta al caso peggiore: lo denominiamo **partizione sbilanciata**. Una partizione sbilanciata e' si ottiene quando la scelta del pivot e' tale che, ad ogni passo, genera un sottoproblema di dimensione 0 e un sottoproblema di dimensione $n-1$. In questo caso la ricorrenza che si ottiene e':
$$
T(n) = T(n-1) + \Theta(n)
$$
la cui situazione, vista come sviluppo di una serie aritmetica, e' evidentemente $T(n) = \Theta(n^2)$. Quindi, stiamo considerando un algoritmo che si comporta in maniera peggiore di *MergeSort*, e, inoltre, e' tale che il suo caso peggiore si verifica quando l'array i partenza e' gia' ordinato in partenza: proprio una situazione nella quale addirittura *InsertionSort* si comporta meglio ($\Theta(n)$).

D'altra parte, il caso migliore si verifica quando la partizione e' sempre bilanciata nel modo piu' equo possibile. In questo caso la ricorrenza che si ottiene e':
$$
T(n) = 2 \cdot T(\frac{n}{2}) + \Theta(n)
$$
la cui soluzione, per il Master Theorem, e' $\Theta(n \cdot log(n))$. Cosa accade quando la partizione e' bilanciata in modo iniquo? Per esempio assumendo che si creino sempre due sottoproblemi di dimensione relativa 9 a 1?
Anche questo e' un caso particolare. La ricorrenza risultante e'
$$
T(n) \le T(\frac{9 \cdot n}{10} + T(\frac{n}{10}) + \Theta(n))
$$
la cui soluzione si puo' mostrare essere $T(n) = O(n \cdot log(n))$. Questi esempi suggeriscono che quando la partizione e' costante allora non e' mai sbilanciata.

La considerazione precedente si puo' portare all'estremo, osservando che **per ogni** scelta del pivot  (quindi, per ogni versione di *Partition*) **esiste** un input tale che portera' la corrispondente versione di *QuickSort* al caso peggiore. Possiamo pensare a questo problema in termini di gioco: per ogni mossa del giocatore **buono** (chi costruisce *Partition*) esiste una mossa del giocatore **cattivo** (chi costruisce l'input) in maniera da generare il caso peggiore. 
Ma per un dato input ed una data versione di *Partition*, qual'e' la possibilita' che questo succeda?

Mostreremo che la complessita' del caso medio di *QuickSort* e' $O(n \cdot log(n))$, sotto l'ipotesi che tutti gli input siano equamente probabili. In particolare possiamo **garantire** che tutti gli input siano equamente probabili attraverso una versione **randomizzata** dell'algoritmo.
Otteniamo due vantaggi:
1. non ci possono essere input **volutamente** pessimi (possono ancora esistere, ma con una bassa probabilita')
2. l'assunzione "tutti gli input sono equamente probabili" e' rispettata, il che rende leggermente piu' semplice il calcolo

## La variante *RandomizedQuickSort* e la sua complessità
``` Pseudocodice
proc RandomizedPartition(A, p, r) {
	s = p<=Random()<=r
	SwapValue(A, s, r)
	x = A[r]
	i = p-1
	for j=p to r-1 {
		if (A[j] <= x) then {
			i = i +1
			SwapValue(A, i, j)
		}
	}
	SwapValue(A, i+1, r)
	return i+1
}
```

``` Pseudocodice
proc RandomizedQuickSort(A, p, r) {
	if (p < r) then {
		q = RandomizedPartition(A, p, r)
		RandomizedQuickSort(a, p, q-1)
		RandomizedQuickSort(A, q+1, r)
	}
}
```

La scelta casuale del pivot implica che ad ogni passo tutte le partizioni sono ugualmente probabili. Questo significa che possiamo parlare della complessita' di *RandomizedQuickSort* nel caso medio in termini di probabilita' di una determinata partizione. Guardiamo cosa succede quando effettuiamo una partizione qualsiasi. Supponiamo che, dopo aver eseguito *Partition*, la situazione sia la seguente:
![[RandomizedQuickSort.png]]

La posizione di P (il pivot) determina su quanti elementi di *QuickSort* si richiamera'. Detta *k* la cardinalita' del lato sinistro, la cardinalita' del lato destro sara' n-1-k, considerando che il pivot non viene piu' toccato.

Se tutte le partizioni sono ugualmente probabili, la probabilita' di una specifica partizione e' proprio $\frac{1}{n}$. Formalizzando, si ottiene che 
$$
T(n) = \frac{1}{n} \cdot \sum_{k=0}^{n-1}(T(k) + T(n-k-1)) + \Theta(n)
$$
e quindi:
$$
T(n) = \frac{2}{n} \cdot \sum_{k=0}^{n-1}T(k) + \Theta(n)
$$
Dimostreremo che $T(n) = O(n \cdot log(n))$ attraverso la tecnica della sostituzione. In particolare, mostreremo che $T(n) \le a \cdot n \cdot log(n) + b$ per qualche costante a, b.

Procediamo. In prima istanza semplifichiamo la nostra espressione:
$$
\begin{align}
T(n) &= \frac{n}{2} \cdot \sum_{k=0}^{n-1}T(k) + \Theta(n) \qquad &\text{ricorrenza} \\
&\le \frac{2}{n} \cdot \sum_{k=1}^{n-1}(a \cdot k \cdot log(k) + b) + \frac{2 \cdot b}{n} + \Theta(n) \qquad &\text{ip. produttiva} \\
&= \frac{2}{n} \cdot \sum_{k=1}^{n-1}(a \cdot k \cdot log(k) + b) + \Theta(n) \qquad &k=0 \quad \text{messo in } \Theta(n) \\
&= \frac{2}{n} \cdot \sum_{k=1}^{n-1} a \cdot k \cdot log(k) + \frac{2}{n} \cdot \sum_{k=1}^{n-1}b + \Theta(n) \qquad &\text{somma distributiva} \\
&= \frac{2 \cdot a}{n} \cdot \sum_{k=1}^{n-1} k \cdot log(k) + \frac{2 \cdot a}{n} \cdot (n-1) + \Theta(n) \qquad &\text{seconda somma valutata} \\
&\le \frac{2 \cdot a}{n} \cdot \sum_{k=1}^{n-1} k \cdot log(k) + 2 \cdot b + \Theta(n) \qquad &\frac{n-1}{n} \lt 1
\end{align}
$$

Secondariamente, osserviamo che $\sum_{k=1}^n k \cdot log(k)$ puo' essere limitata verso l'alto, come segue:
$$
\begin{align}
\sum_{k=1}^{n-1}(k \cdot log(k)) &= \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} (k \cdot log(k)) + \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} (k \cdot log(k)) \qquad &\text{spezzo} \\
&\le \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} (k \cdot log(\frac{n}{2})) + \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} (k \cdot log(n)) \qquad &\text{maggioro} \\
&= (log(n) - 1) \cdot \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k + log(n) \cdot \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} k \qquad &\text{estraggo} \\
&= (log(n) - 1) \cdot \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k - \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k + log(n) \cdot \sum_{k=\lceil \frac{n}{2} \rceil}^{n-1} k \qquad &\text{distribuisco} \\
&= log(n) \cdot \sum_{k=1}^{n-1}k - \sum_{k=1}^{\lceil \frac{n}{2} \rceil -1} k \qquad &\text{sommo} \\
&\le \frac{1}{2} \cdot log(n) \cdot n \cdot (n-1) - \frac{1}{2} \cdot \frac{n}{2} \cdot (\frac{n}{2} - 1) \qquad &\text{valuto} \\
&= \frac{1}{2} \cdot (n^2 \cdot log(n) - n \cdot log(n)) - \frac{1}{8} \cdot n^2 + \frac{1}{4} \cdot n \qquad &\text{valuto} \\
&\le \frac{1}{2} \cdot log(n) - \frac{1}{8} \cdot n^2 \qquad &n \ge 2 \\
\end{align}
$$

Finalmente, usiamo questo risultato per completare il caso induttivo:
$$
\begin{align}
T(n) &\le \frac{2 \cdot a}{n} \cdot (\frac{1}{2} \cdot (n^2 \cdot log(n) - \frac{1}{8} \cdot n^2) + 2 \cdot b + \Theta(n)) \qquad &\text{dalla precedente} \\
&= a \cdot n \cdot log(n) - \frac{a \cdot n}{4} + 2 \cdot b + \Theta(n) \\
&\le a \cdot n \cdot log(n) + b \\
\end{align}
$$
Quindi il tempo del caso medio di *RandomizedQuickSort* e' precisamente $O(n \cdot log(n))$ e abbiamo escluso la possibilita' di **poter** scegliere un'istanza di input al fine di ottenere il caso peggiore!

## Conclusione
*MergeSort* e *Quicksort* sono esempi di strategia **divide and conquer**. Questa strategia si basa sull'idea di dividere il problema in due o piu' sotto-problemi separati tra loro, e poi combinare i risultati di questi ultimi. Si contrappone a strategie piu' semplici, come quelle iterative (come *InsertionSort*), piu' complesse, come la **programmazione dinamica**, o semplicemente diverse come la strategia **greedy**. Allo stesso modo, *QuickSort*, *Partition* e *RandomizedQuickSort* racchiudono numerosi concetti che possono essere considerati archetipici di molte idee algoritmiche.  

---
# *CountingSort* e *RadixSort* 

## Limiti per l'ordinamento basato su confronti
Qual'e' la complessita' minima del problema dell'ordinamento? Per rispondere, possiamo focalizzarci su un tipo specifico di operazione elementare; se riusciamo a dire **questo e' il minimo numero di operazioni di quel tipo**, allora abbiamo un limite inferiore per tutte le operazioni.

Un algoritmo di ordinamento si **basa sui confronti** se ogni passo puo' essere visto come un'operazione di confronto tra due elementi, seguita da uno spostamento. Gli algoritmi visti fino ad ora sono basati sul confronto.
Possiamo **generalizzare** il processo di ordinare per confronti?
Assumendo che possiamo solo confrontare 2 elementi per volta, il processo di ordinare puo' essere visto come un albero binario la cui radice e' l'inpiut. Ad ogni nodo si associa la **permutazione** dell'oggetto che corrisponde ad uno scambio.

Quante permutazioni possibili con *n* elementi? Precisamente $n!$, e un albero binario con $n!$ foglie e' alto, almeno, $log(n!)$. Quindi $log(n!)$ e' un limite inferiore alla lunghezza massima di un ramo nell'albero che rappresenta l'esecuzione di un qualsiasi algoritmo di ordinamento basato sui confronti. Sappiamo che:
$$
log(n!) = \Theta(n \cdot log(n))
$$
Quindi il nostro limite inferiore e' $\Omega(n \cdot log(n))$. Per esempio, questo significa che *MergeSort* e' **ottimo**, visto che ha complessita', nel caso peggiore, $\Theta(n \cdot log(n))$. *QuickSort* non e' ottimo, neppure nella sua versione randomizzata, ma altre considerazioni che abbiamo fatto ci portano a preferirlo in tate situazione.
Per numeri sufficientemente **piccoli**, possiamo fare meglio di $n \cdot log(n)$, In realta', il limite che **non possiamo** vincere e' $log(n!)$.

E' possibile ordinare correttamente 5 numeri basandoci sui confronti in meno di 8 confronti? Se rispondiamo di no, allora sbagliamo, anche se e' vero che $5 \cdot log(5) = 11.6 ≈ 12$. La risposta e' si, perche' $log(5!) = log(120) = 6,90 ≈ 7$, che e' il numero minimo di confronti necessari per decidere l'ordine di 5 elementi diversi. In linea di principio ne esiste uno specifico per qualunque numero **fissato** di elementi: questi algoritmi non sono eleganti, non insegnano nulla di concettuale e servono solo esclusivamente per rappresentare un'idea.

Oltre ad esistere un numero minimo di confronti che qualunque algoritmo deve fare per poter risolvere il problema dell'ordinamento, esiste un numero minimo di operazioni, e quindi di complessita' minima.

## Ipotesi aggiuntive
Aggiungendo delle ipotesi agli oggetti che vogliamo ordinare possiamo vincere dei limiti. Un esempio che vediamo sono gli algoritmi *CountingSort* e *RadixSort*.
Entrambi gli algoritmi sono basati sull'ipotesi aggiuntiva di stare ordinando dei numeri interi du cui conosciamo il limite superiore. *CountingSort* lavora direttamente coi numeri e *RadixSort* ne generalizza l'uso.
*CountingSort* si basa su un array A, su un appoggio di C (quindi non e' in place) e su uno di uscita B, dove si ottiene il risultato.

## *CountingSort*
``` Pseudocodice
proc CountingSort(A, B, k) {
	let C[0, ..., k] new array
	for (i=0 to k) C[i] = 0    // azzera dei contatori
	for (j=1 to A.length) C[A[j]] = C[A[j]] + 1   // inizializza i contatori
	for (j=1 to k) C[i] = C[i] + C[i-1]    /* calcola in C il numero di volte in
											  cui un numero piu' piccolo o uguale
											  a 'i' compare in A */  
	for (j=A.lenght downto 1) {
		B[C[A[j]]] = A[j]
		C[A[j]] = C[A[j]] - 1
	}
}
```

![[CountingSort.png]]

### Correttezza di *CountingSort*
Per la **terminazione**, vediamo che tutto l'algoritmo e' governato da cicli **for**, e quindi termina per definizione.
Guardiamo la **correttezza**. Alla fine del secondo ciclo **for**, in C la posizione i-esima contiene il numero di elementi di A che sono uguali ad *i*, e alla fine del terzo, la posizione i-esima contiene il numero di elementi di A che sono uguali ad *i*. Un'**invariante** corretta per il quarto ciclo **for** e': al j-esimo passo, $C[A[j]]$ e' la posizione corretta di $A[j]$ in B. In primo luogo osserviamo che, prima di iniziare il quarto ciclo, per ogni *j* si ha che $C[A[j]] - z$ e' la posizione corretta di $A[j]$ in B se $A[j]$ e' tale che $|{A[I]|I > j, A[I] = A[j]}| = z$. 

Adesso dimostriamo il caso generale.
- **Caso base**. L'invariante e' chiaramente vera per $j=n$, per l'osservazione precedente.
- **Caso induttivo**. Supponiamo che l'invariante sia vera per un certo *j*. Dopo aver effettuato l'inseriemnto di $A[j]$ nella posizione $C[A[j]]$, quest'ultimo valore diminuisce di un'unita'. Consideriamo adesso l'elemento $A[j-1]$. Se questo e' diverso da tutti gli elementi $A[I]$ con $I \ge j$, allora per l'osservazione precedente l'invariante e' ancora vera. Supponiamo invece che esistano *p* elementi del tipo $A[I]$, con $I \ge j$, tali che $A[I] = A[j-1]$. Nei passaggi precedenti, il valore $C[A[j-1]]$ e' dunque diminuito di *p* unita'. Questo significa che adesso il valore di $C[A[j-1]]$ e' quello della posizione corretta di $A[j-1]$ in B.

La correttezza di questa invariante implica direttamente che l'ultimo ciclo dell'algoritmo ordina A in B, come richiesto.

### Complessita' di *CountingSort*
La **complessità** di *CountingSort* e' data dai quattro cicli. Due di questi hanno complessita' $\Theta(n)$ e gli altri due hanno complessita' $\Theta(k)$. Pertanto se $k = O(n)$ la complessita' totale e' $\Theta(n)$. Ma essendo precisi, la complessita' andrebbe scritta come $\Theta(n+k)$. Quindi se volessimo usare *CountingSort* su un'array qualsiasi che contiene interi, di cui non conosciamo il massimo, l'idea di fare una passata iniziale per computare il massimo, e dichiarare C in maniera dinamica, e' rischiosa: se *k* e' troppo grande, l'allocazione potrebbe fallire, e comunque, se $k>>n$, la complessita' non e' piu' lineare.

## L'algoritmo *RadixSort* per ordinare elementi multi-indice
Immaginiamo di voler ordinare *n* elementi multi-indice. Un esempio potrebbe essere quello di ordinare *n* date (giorno-mese-anno). 
Un algoritmo classico che si puo' utilizzare per risolvere questo problema prevede di ordinare gli elementi rispetto all'indice piu' significativo, per poi passare al secondo, e cosi' via. Nell'esempio, avremmo le date ordinate per anno, mese ed infine giorno. Il difetto principale di questa strategia e' che dopo la prima passata di ordinamento dobbiamo separare gli elementi per gruppi. Quando disponiamo di un algoritmo stabile come *CountingSort* possiamo usare una strategia contro-intuitiva: ordinare prima secondo gli indici meno significativi.
*RadixSort* **non** e' un algoritmo a se' stante, ma un **meta-algoritmo**: si basa sun algoritmo interno di ordinamento, per esempio *CountingSort*.

``` Pseudocodice
proc RaidxSort(A, d) {
	for (i=1 to d) AnyStableSort(A) on digit i
}
```

![[RadixSort.png]]

### Correttezza di *RadixSort*
La **terminazione** e' completamente ovvia, assumendo che la procedura interna termini. Per la **correttezza**, l'**invariante** e': dopo la i-esima esecuzione del ciclo piu' interno, gli elementi formati dalle ultime *i* colonne sono correttamente ordinati.

Mostriamo che l'invariante funziona.
- **Caso base.** Se $i=1$, allora stiamo parlando di elementi fatti da un solo indice. Quindi, la correttezza segue dalla correttezza dell'algoritmo stabile utilizzato come sotto-procedura.
- **Caso induttivo.** Assumiamo allora che $i > 1$, e che l'invariante valga per $i-1$. Consideriamo l'indicei-esimo. Dopo la i-esima esecuzione del ciclo **for**, gli elementi sono lessicograficamente ordinati sull'indice i-esimo (il piu' significativo). Consideriamo due elementi a, b che hanno lo stesso valore per l'indice i-esimo, ma valori diversi sull'indice (i-1)-esimo. Per ipotesi induttiva, dopo la (i-1)-esima esecuzione *a* viene posto prima di *b*; poiche' la sotto-procedura e' stabile, questa relazione viene mantenuta dopo la i-esima esecuzione, implicando che vale ancora $a<b$ dopo la i-esima esecuzione.

### Complessita' di *RadixSort*
Tipicamente, ogni indice considerato da solo e; numerico con massimo *k* costante (per esempio, nelle date i giorni vanno da 1 a 31). Quindi, tipicamente, si usa *CountingSort* come procedura interna. Pertanto, la complessità di *RadixSort* per *n* elementi su *d* indici, essendo ogni indice limitato tra 0 e *k*, e' $\Theta(d \cdot (n+k))$. In generale, la complessita' del caso pessimo e' $O(d \cdot f(n))$, dove $O(f(n))$ e' la complessita' nel caso pessimo della procedura interna.

---
# Liste, pile e code

## Strutture Dati
Cosa possiamo dire delle strutture dati? Quali caratteristiche ci interessano?

La **tassonomia** classica delle strutture dati prevede tre dimensioni originali tra loro. Una struttura dati puo' essere **statica** o **dinamica**: con dinamica intendiamo una struttura che e' pensata per aggiungere e togliere elementi durante l'esecuzione di un algoritmo (per esempio un'array e' dinamico mentre un grafo e' statico); **compatta** o **sparsa**: tipicamente le strutture dinamiche sono sparse, ovvero non possiamo fare ipotesi sulla posizione fisica degli elementi in memoria (per esempio, gli array son una struttura compatta perche', indipendentemente da dove da dove e' memorizzato, possiamo assumere che sa fatto in maniera da avere *n* posizioni)fisiche vicine tra loro); **basata** o **non basata sull'ordinamento** delle chiavi: se gli elementi sono disposti in maniera dipendente dal valore delle chiavi, allora sono basate sull'ordinamento, altrimenti non. 

Un altro elemento fondamentale e' la differenza tra **chiave** e **dato satellite**. Cio' che associamo ad una chiave si chiama **dato satellite**, e normalmente questo e' il contenuto informativo della chiave. Quando effettuiamo un movimento di chiave, implicitamente muoviamo anche i dati satelliti, la cui dimensione e' considerata costante.

Array e liste sono considerate strutture dati **concrete** (o **fondamentali**). Su di esse, a volte, e' conveniente costruire strutture dati **astratte**, che nascondono l'implementazione soggiacente.. Questo e' cio' che viene fatto ad esempio nelle librerie offerte dai linguaggi di programmazione. La distinzione e' a volte non completamente netta (come nel caso delle liste).

Possiamo pensare gli array come una struttura dati statica (quindi senza algoritmi di inserimento o cancellazione) associata al problema dell'ordinamento. Quando studiamo strutture dati basate sull'ordinamento delle chiavi, potremmo, in linea teorica, associare anche ad esse il problema dell'ordinamento: normalmente questo non si fa, perche' si presta attenzione ad altri problemi.

Una **lista concatenata** e' una struttura dati dinamica, non basata sull'ordinamento e sparsa. Ad essa, associamo le operazioni di inserimento, cancellazione e ricerca. In certo modo, una lista e' la versione dinamica di un array; cio' nonostante, l'operazione di ordinamento delle chiavi normalmente avviene attraverso la copiatura degli elementi su un oggetto di tipo array e non direttamente.

## Puntatori e lista
Un **puntatore** e' un tipo di dato fondamentale e supportato da quasi tutti i linguaggi di programmazione. Quei linguaggi che non lo supportano offrono comunque le strutture dati principali in termini di oggetti e metodi. 
Un puntatore e' un tipo di variabile che contiene un indirizzo di memoria. Viene associato al **tipo di dato puntato**, per cui un puntatore ad un intero e' in generale diverso da un puntatore ad un tipo complesso.

Nel caso di liste concatenate, immaginiamo un tipo di dato **elemento** (ce contiene almeno la chiave ed un puntatore al contenuto) e due puntatori ad elemento, che chiamiamo **predecessore** e **successore**. Un elemento e' quindi un tipo di dato ricorsivo e come tale va dichiarato. Nella realta' dei linguaggi di programmazione i puntatori vanno creati, dando ordine al sistema operativo di allocare sufficiente spazio di memoria. 
Ai fini didattici, creiamo adesso una variabile *L* (un oggetto di tipo lista) come una struttura che contiene, almeno, un attributo `L.head`, di tipo puntatore ad elemento, che punta alla testa dell'oggetto.. Si puo' pensare che contenga anche attributo tipo `L.numel` che indica il numero di elementi presenti in ogni momento. All'inizio, `L.head = nil` e `L.numel = 0`.


Concentriamoci nel caso di liste **non ordinate**, **doppiamente collegate**; quindi stabiliamo che  un nodo *x* ha definiti la chiave (`x.key`), e i puntatori al prossimo (`x.next`) e al precedente (`x.prev`) elemento. All'inizio la lista e' vuota. Immaginiamo che sia *x* il nuovo elemento, gia' allocato, per esempio con la chiave 5.
![[PuntatoriELista1.png]]

L'operazione `ListInsert` crea il collegamento.
![[PuntatoriELista2.png]]

``` Pseudocodice
proc ListInser(L, x) {
	x.next = L.head
	if (L.hea != nil)
		then L.head.prev = x 
	L.head = x
	x.prev = nil
}
```

Il puntatore `L.head` punta sempre al primo elemento. Se l'oggetto e' vuoto, al primo inserimento *x* e' sia il primo che l'ultimo elemento, quindi il suo prossimo (*next*) e' vuoto. Se d'altra parte non è vuoto, allora l'elemento che segue *x* è quello che era il primo. Poiché si tratta di una lista doppiamente collegata (gli elementi puntano al loro predecessore), se non è vuoto (= se `L.head` non è `nil`) allora il predecessore di quello che prima era il primo elemento deve diventare *x*. Se invece era vuoto, questo puntatore non si modica e rimane `nil`. 

``` Pseudocodice
proc ListSearch(L, k) {
	x = L.head
	while (x != nil) and (x.key != k) x = x.next
	return x
}
```

In quanto a `ListDelete` abbiamo:
![[PuntatoriELista3.png]]

Eliminando *x*, dove *x* punta all'elemento che contiene la chiave 3, si ottiene:
![[PuntatoriELista4.png]]

Il codice e':
``` Pseudocodice
proc ListDelete(L, x) {
	if (x.prev != nil) 
		then x.prev.next = x.next
		else L.head = x.next
	if (x.next != nil)
		then x.next.prev = x.prev
}
```

## Correttezza e complessità delle operazioni su lista
La **correttezza** di queste operazioni e' immediata da dimostrare, cosi' come le loro **complessità**: l'inserimento prende $\Theta(1)$, in quanto inserisce sempre in testa all'oggetto, la cancellazione prende $\Theta(1)$ assumendo di conoscere il puntatore dell'elemento da cancellare, il quale si ottiene attraverso la ricerca che costa $\Theta(n)$ (nel caso peggiore). Anche se abbiamo detto che non ha molto senso parlare di inserimento e cancellazione in array (come struttura concreta), possiamo domandarci come, dal punto di vista della complessità, array e liste si possano confrontare.

## Array vs Liste: confronto
![[ArraiVSListe.png]]

$*$ : assumendo di avere allocato abbastanza spazio, altrimenti costa $\Theta(n)$.
$**$ : per la cancellazione di una **chiave** (senza conoscere l'indirizzo della sua posizione), va sommato il costo della ricerca.

Sebbene dal confronto le due soluzioni sembrano identiche, si osservi che gli array permettono l'accesso diretto, mentre le liste no. Questa differenza gioca un ruolo fondamentale quando le strutture dati di cui abbiamo bisogno sono indicizzabili. Per esempio, un insieme di nomi e' naturalmente memorizzato in un oggetto di tipo lista, ed un nome non e' visto come un indice. Invece, un insieme (abbastanza piccolo) di numeri interi positivi puo' trovare posto in un array e il contenuto di una posizione puo' essere visto come un indice.

## Pile e code
Le pile e le code sono entrambe due strutture dati astratte. Nella nostra tassonomie sono da ritenersi dinamiche e non basate sull'ordinamento. Possono essere implementate in maniera compatta (basate su array), o sparsa (basate su liste). La loro caratteristica distintiva e' che l'accesso agli elementi non e' libero, ma vincolato ad una **politica**. La ragione per cui puo' essere utile avere una politica di accesso e' che questa puo' fare risparmiare dei dettagli implementativi, assicurando un certo ordine di inserimento ed estrazione.

### Pile
Una **pila** (o **stack**) e' una struttura dati astratta che implementa la politica **last in first out** (**LIFO**). Una pila si utilizza in diversi contesti, tra cui valutazione di espressioni, processi di backtracking (per ricordarci le mosse che abbiamo fatto e l'ultimo punto di scelta), eliminazione della ricorsione, e molte altre. 

### Pile su Array
Per implementare una pila ci possiamo basare su un array; avremo quindi una implementazione compatta. L'idea e' quella di **mascherare** la struttura portate all'utente finale. Si puo' pensare come un **oggetto** con i suoi relativi **metodi**. Assumeremo quindi che S sia  un array di interi, che interpretiamo come uno stack e che viene dotato con i parametri (naturali) `S.top` (inizializzato a 0) e `S.max` (che indica la massima capacita' di S). Le operazioni di inserimento ed eliminazione di un elemento nelle pile prendono il nome di *Push* e *Pop*, rispettivamente. La variabile S e' un array dotato di struttura

``` Pseudocodice
proc Empty(S) {
	if (S.top = 0) 
		then return true
	return false
}
```

``` Pseudocodice
proc Push(S, x) {
	if (S.top = S.max)
		then return "overflow"
	S.top = S.top + 1
	S[S.top] = x
}
```

``` Pseudocodice
proc Pop(S) {
	if (Empty(S))
		then return "underflow"
	S.top = S.top - 1
	return S[S.top + 1]
}
```
### Code su array

Una **coda** (o **queue**) e' una struttura dati elementare che implementa una politica **first in first out** (**FIFO**). Permette di accedere ai dati attraverso inserimento ed eliminazione che prendono normalmente i nomi di *Enqueue* e *Dequeue*, rispettivamente. Anche una coda puo' avere diversi usi: in una **playlist** le canzoni si trovano in una coda (circolare), in maniera che la canzone appena ascoltata si ripetera' **il piu' tardi possibile**; oppure nei processi di visita di strutture dati piu' complesse come i grafi.

Per implementare uno stack ci possiamo basare su un array. Assumendo quindi che *Q* sia una coda (un array dotato di struttura), con parametri `Q.head`, `Q.tail` (naturali), la cui inizializzazione e' `Q.head = Q.tail = 1`. Per assicurare di essere capaci di distinguere perfettamente le due situazioni di pila vuota e di pila piena, entrambe caratterizzate da `Q.head = Q.tail`, aggiungiamo una variabile, chiamata *dim* inizialmente a 0 (perche' la coda e' vuota).

``` Pseudocodice
proc Enqueue(Q, x) {
	if (Q.dim = Q.lenght)
		then return "overflow"
	Q[Q.tail] = x
	if (Q.tail = Q.lenght)
		then Q.tail = 1
		else Q.tail = Q.tail + 1
	Q.dim = Q.dim + 1
}
```

``` Pseudocodice
proc Dequeue(Q) {
	if (Q.dim = 0)
		then return "underflow"
	x = Q[Q.head]
	if (Q.head = Q.lenght)
		then Q.head = 1
		else Q.head = Q.head + 1
	Q.dim = Q.dim - 1
	return x
}
```

**Correttezza**, **complessità** e **terminazione** di queste operazioni sono triviali da dimostrare.

### Pile su liste
Pile e code possono anche essere implementate in maniera sparsa, utilizzando delle liste concatenate come supporto. Per quanto riguarda le pile, l'oeprazione di inserimento in testa di una lista e' proprio l'operazione di *Push* e l'operazione di *Pop* e' una semplificazione dell'operazione di delete. In questo caso, S e' semplicemente una lista, senza campi aggiuntivi.

``` Pseudocodice
proc Empty(S) {
	if (S.head = nil)
		then return true
	return false
}
```

``` Pseudocodice
Proc Push(S, x) {
	Insert(S, x)
}
```

``` Pseudocodice
proc Pop(S) {
	if (Empty(S))
		then return "underflow"
	x = S.head
	Delete(S, x)
	return x.key
}
```

**Correttezza**, **complessità** e **terminazione** di queste operazioni non presentano nessun problema.

### Code su liste
Per implementare una coda attraverso una lista dobbiamo immaginare che la lista Q sia dotata del campo `Q.tail` in aggiunta al campo `Q.head`. In questo modo possiamo implementare *Enqueue* semplicemente chiamando l'inserimento in una lista e *Dequeue* semplicemente chiamando l'eliminazione di un elemento in una lista.

``` Pseudocodice
proc Empty(Q) {
	if (Q.head = nil)
		then return true 
	return false
}
```

``` Pseudocodice
proc Enqueue(Q, x) {
	Insert(Q, x)
}
```

``` Pseudocodice
proc Dequeue(Q) {
	if Empty(Q)
		then return "underflow"
	x = Q.tail
	Delete(Q, x)
	return x.key
}
```

**Correttezza**, **complessità** e **terminazione** sono immediate.

## Conclusione
La lista e' la piu' semplice delle strutture dati dinamiche e puo' essere implementata con collegamento semplice, doppio, oppure in maniere molto complesse che permettono di sfruttare delle euristiche di miglioramento delle complessita'. Le pile e le code sono strutture dati che non solo servono nelle situazioni che abbiamo descritto, ma sono anche rappresentative di concetti fondamentali in informatica e permettono di porsi domande fondamentali per la teoria della complessita' e della calcolabilita'

---

# Heaps, heapsort e code di priorità

## Min e Max Heap
Un heap e' una struttura dati astratta, parzialmente basata sull'ordinamento e necessariamente compatta (sara' dunque basata su array). La caratteristica principale e' quella che una heap mantiene le chiavi semi-ordinate. Useremo le heap come base per le code di priorita' (anche loro strutture dati astratte), ma anche come base per un nuovo algoritmo di ordinamento che risolve il maggior problema di `MergeSort` (quello di non essere in place) ed il maggior problema di `QuickSort` (quello di non avere tempo quadratico nel caso peggiore).

Una **(min/max) heap** e' un array *H* che puo' essere visto come un albero binario **quasi completo**, cioe' tale da avere tutti i livelli pieni, meno l'ultimo. I nodi dell'albero corrispondono agli elementi dell'array. L'elemento `H[1]` dell'array e' la **radice** dell'albero e, normalmente, si tende a differenziare i valori `H.lenght()` (lunghezza dell'array che **contiene** l'heap) e `H.heapsize()` (numero di elementi della heap contenuti in `H`). Tipicamente si ha che `0 <= H.hepsize <= H.lenght`. 
Una heap e' un array con il fatto che per convenienza si puo' visualizzare come albero, dal punto di vista algebrico non ci sono differenze, ma dal punto di vista delle strutture dati le heap **non** sono alberi.

![[full-complete-perfect-trees.png]]

La corretta implementazione di una heap prevede che i **figli** di un nodo nella posizione *i* siano precisamente gli elementi nelle posizioni $2 \cdot i$ e $2 \cdot i+1$ (sinistro e destro rispettivamente). Per conseguenza, il **padre** di un nodo *i* e' identificato dall'indice $\lfloor \frac{i}{2} \rfloor$.

``` Pseudocodice
proc Parent(i) {
	return floor(1/2)
}
```

``` Pseudocodice
proc Left(i) {
	return 2*i
}
```

``` Pseudocodice
proc Right(i) {
	return 2 * i + 1
}
```

## Heap binarie su array
Immaginiamo una variabile *H*, che e' un array di interi con un campo aggiuntivo `H.heapsize()`. Distinguiamo tra due tipi di heap: **max-heap** e **min-heap**. Entrambe soddisfano una proprieta':
- nel primo caso abbiamo che per ogni *i*, `H[Parent(i)] >= H[i]`
- nel secondo caso, per ogni *i*, `H[Parent(i)] <= H[i]`
Di conseguenza, il massimo elemento di una max-heap si trova alla radice, mentre nel caso di una min-heap e' il minimo elemento a trovarsi alla radice.
L'**altezza** di una heap e' la lunghezza del massimo cammino dalla radice alla foglia.

Esempio di min-heap:
![[min-heap.png]]

## Heap binarie su array: calcolo dell'altezza
Se una heap ha altezza *h*, quali sono il minimo e il massimo di numeri che puo' contenere?
In generale: una heap di altezza *h* contiene **al minimo** $2^h$ elementi ed **al massimo** $2^{h+1} -1$ elementi. Questa proprieta' dipende esclusivamente dal fatto che la heap e' come un albero binario quasi completo. Da qui otteniamo che:
$$
\begin{align}
& 2^h \le n \le 2^{h+1}-1 \\ 
&\Rightarrow 2^h \le n < 2^{h+1} \\ 
&\Rightarrow h \le log(n) < h+1
\end{align}
$$
Quindi:
- dalla prima si ottiene che $h ≤ log(n))$ cioe' $h = O(log(n))$;
- dalla seconda si ottiene che $h > log(n)-1$ cioe' $h = \Omega(log(n))$.
Pertanto $h = \Theta(log(n))$.

## Heap binarie su array: `BuildMinHeap()` e `MinHeapify()` 
Problema: data una (non)heap *H* di numeri interi non negativi (cioe' una array), trasformarlo una una min-heap.
A questo fine risolviamo un problema piu' semplice: dato un array *H* ed un indice *i* su di esso tale che `H[Left(i)]` `H[Right(i)]` sono gia' delle min-heap, trasformare *H* in un array tale che anche `H[i]` e' una min-heap.
Procediamo in maniera ricorsiva: sistemiamo il potenziale errore localmente a `i, Left(i) e Right(i)` e poi correggiamo ricorsivamente gli errori che vengono generati dalla sistemazione a livelli piu' bassi. Vediamo una procedura che si chiama `MinHeapify` che fa quanto detto. 
Si potrebbe anche fare riferimento a max-heap e alla procedura simmetrica `MaxHeapify`.

``` Pseudocodice
proc MinHeapify(H, i) {
	l = Left(i)
	r = Right(i)
	smallest = i
	if ((I <= H.heapsize) and (H[l] < H[i]))
		then smallest = l
	if ((r <= H.heapsize) and (H[r] < H[smallest]))
		then smallest = r
	if smallest != i  then {
		SwapValue(H, i, smallest)
		MinHeapify(H, smallest)	
	}
}
```

### Correttezza e complessità di `MinHeapify` 
Per la **terminazione**, osserviamo che la procedura termina in due casi:
- o perche' l'indice *i* non cambia durante una esecuzione (in questo caso non si effettuano chiamate ricorsive)
- oppure perche' l'indice e' diventato piu' grande della dimensione dell'heap (poiche' se cambia cresce sempre)
Per quanto riguarda la **correttezza**, osserviamo che `MinHeapify` e' costruita in maniera ricorsiva. Quindi dobbiamo trovare una **invariante** ricorsiva: dopo ogni chiamata a `MinHeapify` su un nodo di altezza *h* tale che entrambi i figli sono radici di min-heap prima della chiamata, quel nodo e' la radice di una min-heap.

Dimostriamo l'invariante:
- Supponiamo , come **caso base**, `MinHeapify` venga chiamata su un nodo ad altezza h=0. Le ipotesi sono rispettate (il nodo e' figlio); inoltre la procedura non ha alcun effetto, ma allo stesso tempo, un nodo senza figli e' gia' una min-heap.
- Per il **caso induttivo**, consideriamo un nodo in posizione *i* ad altezza $h>0$. Sappiamo che entrambi i suoi figli, in posizione $2 \cdot i$ e $2 \cdot i + 1$, se esistono, sono radici di min-heap per ipotesi.
  Poiche' `H[i]` e' il minimo tra `H[i], H[2 · i], e H[2 · i + 1]`, allora anche il nodo *i* e' radice di una min-heap, come volevamo.

Per calcolare la **complessità** di `MinHeapify`, dobbiamo costruire una ricorrenza. 
Incorriamo in due problemi:
- primo, dobbiamo capire qual'e' il caso peggiore
- secondo, dobbiamo renderci conto che, da un lato vorremmo che come sempre la complessità fosse in funzione della quantita' di elementi nella struttura dati e dall'altro la complessita' di `MinHeapify` dipende dall'altezza dell'elemento su cui e' richiamato.
In primo luogo, il caso peggiore occorre quando la heap sulla quale la procedura e' richiamata tende ad essere sbilanciata, forzando piu' chiamate ricorsive. Possiamo mostrare che il peggior sbilanciamento possibile e' $\frac{2}{3}$. Inoltre, poiche' vogliamo un risultato che possiamo usare in ogni situazione, utilizziamo una ricorrenza leggermente piu' debole, cioe':
$$
T(n) \le T(\frac{2}{3}n) + \Theta(1).
$$

Risolvendo la ricorrenza classica associata a quella precedente si ottiene che $T(n) = \Theta(log(n))$ (Master Theorem, caso 2). Pertanto, nel caso peggiore `MinHeapify` costa $O(log(n))$.
Questo e' un'approssimazione dovuta al fatto che dovremmo calcolare la complessita' in base all'altezza del nodo su cui la procedura viene chiamata; una forma alternativa di scrivere questo risultato e' dire che la complessità e' $O(h)$, dove *h* e' l'altezza della heap (anche in questo caso stiamo approssimando perche' non teniamo conto dell'altezza reale del nodo).

## Heap binarie su array: `BuildMinHeap`
Problema originale: dato un array di *H* interi, convertirlo in una min-heap.
``` Pseudocodice
proc BuildminHeap(H) {
	H.heapsize = H.lenght
	for (i = floor(H.lengt/2) downto 1) 
		MinHeapify(H, i)
}
```

### Correttezza e complessita' di `BuildMinHeap`
La **terminazione** della procedura e' ovvia.
Per la **correttezza**, l'**invariante** che usiamo e': all'inizio di ogni iterazione del ciclo **for**, ogni elemento `H[i+1], H[i+2], ...` e' la radice di una min-heap e all'uscita dall'iterazione anche `H[i]` lo e'.
Dimostriamo:
- Nel **caso base** $i = \lfloor \frac{A.lenght}{2} \rfloor$: ogni elemento del tipo `1H[i+k]` con k>0 e' una foglia e pertanto la radice triviale di un solo elemento.
- Nel **caso induttivo**, e' sufficiente riferirsi alla correttezza di `MinHeapify`. Questa proprieta', riferita all'uscita dal ciclo, dice: `H[1]` e' una min-heap.

Un calcolo della **complessità** approssimativo ci porterebbe alla seguente conclusione: ogni chiamata di `MinHeapify` costa $O(log(n))$ nel caso peggiore e si chiama $\Theta(n)$ volte, pertanto il costo totale e' $O(n \cdot log(n))$. 
In questo caso possiamo dare un limite piu' stretto grazie ad un'analisi piu' dettagliata. Il costo di `MinHeapify` puo' essere espresso come $O(h)$; supponiamo che *h* sia l'altezza del **nodo** su cui viene chiamato. Una semplice osservazione ci dice che se in un albero binario quasi completo ci sono *n* elementi, allora al massimo $\lceil \frac{n}{2^{h+1}} \rceil$ di loro si trovano ad altezza *h*.

Il costo totale nel caso peggiore (quando `MinHeapify` deve sempre arrivare alle foglie), si puo' limitare con:
$$
\sum^{log(n)}_{h=0} (\lceil \frac{n}{2^{h+1}} \rceil \cdot O(h)) = O(n \cdot \sum^{log(n)}_{h=0} \frac{h}{2^h}) $$
L'altezza va da 0 a $log(n)$ e, fissata un'altezza *h*, ci sono $\lceil \frac{n}{2^{h+1}} \rceil$ nodi. Per ogni nodo ad altezza *h*, chiamare `MaxHeapify` costa $O(h)$ e quindi $\sum^{log(n)}_{h=0} (\lceil \frac{n}{2^{h+1}} \rceil \cdot O(h))$ . Ma *n* non dipende da *h* (e quindi moltiplica la sommatoria) e $\frac{1}{2^{h+1}} \cdot h$ si puo' maggiorare con $\frac{h}{2^h}$, e quindi e' $O(n \cdot \sum^{log(n)}_{h=0} \frac{h}{2^h} )$.

$$
\begin{align}
& O \Big(n \cdot \sum^{log(n)}_{h=0} \frac{h}{2^h} \Big) = \\
& O \Big(n \cdot \sum^{log(n)}_{h=0} h \cdot (\frac{1}{2})^h \Big) = \\
& O \Big(n \cdot \sum^{\infty}_{h=0} h \cdot (\frac{1}{2})^h \Big) = \\
& O(n)
\end{align}
$$

La sommatoria $\sum^{log(n)}_{h=0} h \cdot (\frac{1}{2})^h$ e' sostituita dalla serie infinita $\sum^{\infty}_{h=0} h \cdot (\frac{1}{2})^h$ perche' quest'ultima converge ad una costante e poi scompare nella notazione $\Theta$. Il fatto che $\sum^{\infty}_{h=0} h \cdot (\frac{1}{2})^h$ converga a una costante si ha perche'
$$
lim_{h \rightarrow \infty} \frac{(h+1) \cdot (\frac{1}{2})^{h+1}}{h \cdot (\frac{1}{2})^h} = \frac{1}{2}
$$

e, per il teorema del rapporto, questa condizione e' sufficiente per la convergenza.
Il caso migliore (quello in cui un array e' gia' una min-heap) ha costo $\Theta(n)$, perche' la procedura e' governata da un ciclo **for**. 

## Ordinamento con `HeapSort`
Una max-heap puo' essere adesso usata efficientemente per progettare un algoritmo di ordinamento. Consideriamo una max heap e ricordiamo una delle priorita' e' che il massimo elementi di H si trova in `H[1]`. Se consideriamo `H[1]` come gia' ordinato (basta metterlo sulla giusta posizione: l'ultima) e sostituiamo il contenuto di `H[1]` succede che `H[2]` e `H[3]` sono ancora max-heap. Quindi chiamando `MAxHeapify` rispettiamo le ipotesi e possiamo ripetere il processo. Il codice di `HeapSort` si basa precisamente su questa osservazione.

``` Pseudocodice
proc HeapSort(H) {
	BuildMaxHeap(H)
	for (i=H.lenght downto 2) {
		SwapValue(H, i, 1)
		H.heapsize = H.heapsize - 1
		MaxHeapify(H, 1)
	}
}
```

### Correttezza e complessita' di `HeapSort` 
Nel caso di `HeapSort` la **correttezza** e' immediata, perche' dipende direttamente dalle procedure su cui e' basato. Anche la **terminazione** e' ovvia. La **complessità** di `HeapSort`, nel caso peggiore, si calcola come segue. La chiamata a `BuildMaxHEap` costa $\Theta(n)$; per ogni *i* si effettua uno scambio 
($O(1)$) ed una chiamata a `MaxHeapify` ($\Theta(log(n))$). Il totale e' $\theta(n \cdot log(n))$. La complessità e' la stessa nel caso migliore e quindi nel caso medio: dopo aver effettuato `BuildMaxHeap` , per definizione ogni chiamata successiva a `MaxHeapify` (dopo lo scambio) deve arrivare alle foglie. Possiamo anche osservare che la nostra implementazione di `HeapSort` non e' stabile: si puo' dimostrare osservando il suo comportamento sull'array $H = [1,1]$. D'altra parte e' certamente in place, a meno delle chiamate ricorsive, che, come abbiamo osservato, sono unicamente 
tail-recursive.

## Code di priorita'
Una **coda di priorità** e' una struttura dati astratta basata sull'ordinamento e necessariamente compatta. Possiamo costruire una coda di priorita' basandoci su una min-heap. A differenza di una coda classica, che implementa una politica FIFO, una coda di priorita' associa ad ogni chiave la **priorità** e serve (cioe' estrae) l'elemento a priorita' piu' bassa. Questa estrazione e' associata all'operazione che **aggiusta** la struttura dati, ed anche alla possibilita' di **inserire** nuovi elementi, o **cambiare la priorità** di un elemento inserito. Sia quindi *Q* una min-heap senza campi aggiuntivi.

``` Pseudocodice
proc Enqueue(Q, priority) {
	if (Q.heapsize = Q.lenght)
		then return "overflow"
	Q.heapsize = Q.heapsize + 1
	Q[heapsize] = ∞
	DecreaseKey(Q, Q.heapsize, priority)
}

proc DecreaseKey(Q, i, priority) {
	if (priority > Q[i])
		then return "error"
	Q[i] = priority
	while ((i>1) and (Q[Parent(i)] > Q[i]))
		SwapValue(Q, i, PArent(i))
		i = Parent
}

proc ExtractMin(Q) {
	if (Q.heapsize < 1)
		then return "undeflow"
	min = Q[1]
	Q[1] = Q[Q.heapsize]
	Q.heapsize = Q.heapsize - 1
	MinHeapify(Q, 1)
	return min
}
```

### Code di priorita' su heap binarie
La **correttezza** di queste operazioni e' immediata da dimostrare usando gli stessi ragionamenti visti prima; nello stesso modo e' immediato costruire una versione simmetrica sia dell'estrazione (quindi del massimo), sia del decremento (quindi incremento) di una chiave. La **complessità** di tutte queste operazioni e' $\Theta(log(n))$ nel caso pessimo.

### Code di priorita' su array
Le code di priorita' come struttura dati astratta possono essere implementate anche direttamente su array, senza dover passare dalle heap. Questa soluzione ha il vantaggio della semplicita' di implementazione. La complessita' delle operazioni, invece, non sono comparabili. Questo significa che questa soluzione e' migliore in qualche caso e peggiore in qualche altro. In questa soluzione conviene esplicitare le chiavi associate per evitare confusioni.

Q e' un array (che immaginiamo sempre con campo `Q.lenght`) e diciamo che ogni posizione *i* ha tre valori: *i* stesso, `Q[i]` e `Q[i].empty`, immaginiamo di aver inizializzato tutti i valori `Q[i].empty` a falso. In questo modo, abbiamo fatto l'equivalente della costruzione della coda. Il **costo** di questa inizializzazione e' $\Theta(n)$.

``` Pseudocodice
proc Enqueue(Q, i, priority) {
	if (i > Q.lenght)
		then return "overflow"
	Q[i] = priority
}

proc DecreaseKey(q, i, priority) {
	if ((Q[i] < priority) or (Q[i].empty = 1))
		then return "error"
	Q[i] = priority
}

proc ExtractMin (Q) {
	MinIndex = 0
	MinPriority = ∞
	for (i = 1 to Q.length) {
		if ((Q[i] < MinPriority) and (Q[i].empty = 0)) 
			then 
			MinPriority = Q[i]
			MinIndex = i		
	}
	if (MinIndex = 0)
		then return "underflow"
	Q[MinIndex].empty = 1
	return MinIndex
}
```

In questa soluzione, la cui **correttezza** e **terminazione** sono immediate, l'operazione di estrazione del minimo costa $O(n)$, perche' nel caso peggiore il ciclo **for** deve scorrere tutti gli elementi. Invece, grazie all'ipotesi sul valore degli elementi, il decremento costa $\Theta(1)$, cosi' come l'operazione di inserimento di un nuovo elemento.

## Code di priorita' su array vs su heaps: confronto

|                   | array (c. peggiore e medio) | heaps (c. peggiore e medio) |
| ----------------- | --------------------------- | --------------------------- |
| inizializzazione  | $\Theta(n)$                 | $\Theta(n)$                 |
| inserimento       | $\Theta(1)$                 | $\Theta(log(n))$            |
| decrememto        | $\Theta(1)$                 | $\Theta(log(n))$            |
| estrazione minimo | $\Theta(n)$                 | $\Theta(log(n))$            |

---
# Tabelle Hash
Una **tabella hash** e' una struttura dati astratta. 
Nella sua versione piu' generale, il problema puo' essere descritta cosi': dato un numero di oggetti **relativamente piccolo**, ognuno dei quali e' denotato da una chiave il cui universo e' **relativamente grande**, trovare un modo efficiente di memorizzare in maniera dinamica questi oggetti e implementare le operazioni di inserimento, cancellazione e ricerca.
Nella nostra implementazione, le tabelle hash sono dinamiche, parzialmente compatte e non basate su ordinamento.

Non e' possibile costruire una tabella di assegnamento in maniera efficiente, ed i metodi impliciti generano chiavi molto grandi. La soluzione che prevede l'uso di un array (e si chiama **tabella hash ad accesso diretto**) ha ottime complessita': $\Theta(1)$ per tutte le operazioni. Purtroppo, la grandezza delle chiavi rende inaccettabile la maggioranza dei casi.
La soluzione che prevede l'uso di una lista presenta $\Theta(1)$ per l'inserimento e $\Theta(n)$ per ricerca e cancellazione nei casi medio e pessimo.

## Tabelle hash: notazione
Una tabella hash ad accesso diretto *T* e' un semplice array di **puntatori** ad oggetti; `T[key]` punta all'oggetto la cui chiave assegnata e' `key`.
Diciamo che gli oggetti sono denotati con `x, y, ...` e che per ognuno di essi il campo `key` e' la sua chiave.
Per una tabella ad accesso diretto:
- `x.key` e' sempre piccolo (se `m` e' la dimensione della tabella $\rightarrow$ `x,key <= m`)
- `x.key != y.key` per ogni coppia di `x != y`.
Per *m* molto grande, anche un operazione elementare come **creare** *T* vuoto prende troppo tempo.

Qual'e' una caratteristica comune a tutte le applicazioni tipiche in questo contesto? Il numero di chiavi effettivamente utilizzate e' molto inferiore alla cardinalita' del dominio, che in questo contesto si chiama **universo**, e si denota con $\mathcal{U}$. 

==Condizioni di utilizzo tabella hash==
- Universo grande.
- Sottoinsieme utilizzato dall'universo relativamente piccolo ma non assolutamente piccolo
- $\rightarrow$ non posso allocare tutto in maniera compatta
- $\rightarrow$ non posso dire che l'accesso lineare e' efficiente

## Tabelle hash con chaining
Se il numero effettivo *n* di elementi **effettivamente utilizzati** e' molto piu' piccolo della cardinalita' dell'unicerso, possiamo ancora implementare *T* con un array di posizioni `1, ..., m`, ma nasce il problema della memorizzazione della chiave `k` molto piu' grande di `m`, quindi senza accesso diretto.
Per risolverlo creiamo questa funzione:
$$
h: \mathcal{U} \rightarrow \{1, ..., m\}
$$
detta **funzione hash**, per poter indirizzare l'elemento `k` alla posizione `h(k)`. `h` non puo' essere iniettiva. Quando $k_1 \ne k_2$ e si da il caso che $h(k_1) = h(k_2)$, chiameremo questa situazione di **conflitto**.
Abbiamo gia' ottenuto un primo vantaggio:
- `T` e' molto piccolo (ha esattamente `m` posizioni) e quindi diventa inutilizzata.
Inoltre, il tempo di accesso e' ancora costante **a meno dell'overload dovuto ad un conflitto**.

>La funzione di hash puo' essere:
 - crittografica
> - non crittografica
>
 La funzione di hash puo' essere:
 - crittografica
> - non crittografica

Vorremmo che la funzione hash:
- Resistenza alla per-immagine
- Resistenza ai conflitti
- Uniforme (distribuisca bene)

Il nostro obiettivo e' quello di progettare una funzione di hash che **minimizzi** i conflitti. 
La tecnica chiamata **chaining** risolve tutti i conflitti utilizzando una lista doppiamente collegata. La testa della lista e' memorizzata in `T[h(k)]`, che quando e' vuota contiene **nil**.

> Questa soluzione occupa un po' piu' di `m` parole di memoria, ma molte meno di cardinalita' dell'universo

![[hashMapChaining.png]]

``` Pseudocodice
proc HashInsert(T, k) {
	let x be a new node with key k
	i = h(k)
	ListInsert(T[i], x)
}

proc HashSearch(T, k) {
	i = h(k)
	return ListSearch(T[i], k)
}

proc HashDelete(T, k) {
	i = h(k)
	x = ListSearch(T[i], k)
	ListDelete(T[i], k)
}
```

### Tabelle hash con chaining: complessità della ricerca
Queste operazioni sono **corrette** e **terminanti**, la loro **complessità** dipende dalla politica che utilizziamo sulle liste. Nel caso studiato da noi, l'inserimento e' precisamente $\Theta(1)$. Anche nel caso di `HashDelete` la complessita' e' $\Theta(1)$ piu' il costo di cercare il nodo giusto da cancellare.

Quanto costa un `HashSearch`?
- Fissiamo `n` come numero di elementi effettivamente inseriti in `T` e `m` come dimensione di `T`. Se tutti gli `m` elementi finiscono nella stessa cartella, la ricerca e quindi anche la cancellazione sono nel caso peggiore e la loro complessita' diventa $\Theta(n)$. 

Come calcoliamo il caso medio?
- Il caso medio dipende dalla bonta' della funzione `h`. Sotto l'ipotesi di **hashing uniforme e semplice**, `h` inserisce una chiave `k` in un determinato slot con la stessa probabilita' con la quale inserisce in qualsiasi altro slot, indipendentemente dalla presenza di chiavi in `T`. Per ogni posizione `j` tra 1 e `m`, la lunghezza della lista `T(j) (|T(j)|)` e' tale che $\sum_{j=1}^m |T[j]| = n$, per cui il **valore atteso** di `|T(j)|`e' $\frac{n}{m}$ (che viene detto **fattore di carico**).  
  Quindi, sotto l'ipotesi di hashing uniforme, il caso medio di `HashSearch` su una tabella hash a collisioni risolte per chaining ed in caso di ricerca con esito **negativo** ha complessita' $\Theta(1 + \frac{n}{m})$, assumendo che il calcolo `h` prenda tempo $O(1)$.

Cosa succede quando la ricerca ha esito positivo (la chiave e' **presente** in `T`)?
- Rispetto al caso piu' complesso, il calcolo del tempo medio di esecuzione di `HashSearch` e' piu' complesso perche' il tempo dipende dal **numero di elementi della lista per la posizione** `h(k)` che bisogna guardare prima di arrivare a `k` stesso. 
- Osserviamo che questo numero e' esattamente il numero di elementi tale che:
	- hanno chiave `k'` dove `h(k) = h(k')`
	- sono stati inseriti **dopo** il momento in cui e' stato inserito `k`.
- Le liste sono strutturate in modo che gli elementi nuovi appaiono prima e quindi se cerco un elemento a chiave `k` nella lista puntata da `h(k)` tale che `I` elementi hanno chiave `k'` con `h(k) = h(k')` sono stati inseriti dopo `k`, dovro' guardare `I` elementi prima di avere successo.
- L'elemento cercato con chiave `k` ha la stessa probabilita' di essere uno degli `n` elementi correntemente in `T`. La probabilita' che due chiavi siano indirizzate alla stessa posizione e' $Pr(h(k)) = h(k')$, che sotto l'ipotesi di hashing uniforme semplice e' precisamente $\frac{1}{m}$.
- Il valore atteso per la variabile probabilistica `V` '**numero di elementi da esaminare per giungere a** `k`' e':
$$
E[V] = \frac{1}{n} \cdot \Big(\sum_{i=1}^n \Big(1+\sum_{j=i+1}^n \frac{1}{m} \Big)\Big).
$$
- Quindi
$$
\begin{align}
E[V] &= 1 + \frac{1}{n \cdot m} \cdot \sum_{i=1}^n(n-i) \qquad &\text{calcolo algebrico} \\
&= 1 + \frac{1}{n \cdot m} \cdot \Big( \sum_{i=1}^n n - \sum_{i=1}^n i \big) \qquad &\text{calcolo algebrico} \\
&= 1 + \frac{1}{n \cdot m} \cdot \Big( n^2 - \frac{n \cdot (n+1)}{2} \Big) \qquad &\text{sommatoria} \\
&= 1 + \frac{n-1}{2 \cdot m} \qquad &\text{calcolo algebrico} \\
&= \Theta(1 + \frac{n}{m})
\end{align}
$$

Concludendo, il tempo medio per una ricerca e' $\Theta(1 + \frac{n}{m})$ (visto che e' uguale sia per esito positivo che negativo).

## Funzioni di hash per il chaining
Da una funzione hash vorremmo che fosse uniforme semplice e che fosse computazionalmente semplice da calcolare.
La letteratura ci ore varie possibilità che tengono conto delle diverse esigenze anche sul valore di m:
- se abbiamo chiavi naturali con `m` numero primo, lontano dalla potenza di 2, allora usiamo il metodo della **divisione**;
- se invece abbiamo chiavi naturali, con $m = 2^p$ per qualche `p`, allora usiamo il metodo della **moltiplicazione**;
- infine, se abbiamo chiavi che sono stringhe o oggetti complessi, con `m` numero primo, lontano da una potenza di 2, possiamo usare il metodo dell'**addizione**.

### Funzioni di hash per il chaining: divisione
Nel caso dei numeri naturali, una soluzione molto intuitiva e' usare il resto della divisione per `m`:
$$
h(k) = (k \cdot mod(m)) + 1
$$

Quando una funzione e' una funzione di hash uniforme semplice?
- Quando si verifica che `m` e' un numero primo e lontano da una potenza di 2. Infatti quando `m` non e; un numero primo, i suoi divisori danno luogo a liste di chaining particolarmente lunghe. Inoltre, se $m=2^p$ per qualche `p`, allora `k mod(m)` dipende unicamente dagli ultimi `p` bits della rappresentazione binaria di `k`. Se le chiavi k sono uniformemente distribuite, nessuno di questi due problemi è realmente importante. Ma nella realtà le chiavi raramente sono uniformemente distribuite, e quindi usiamo questi accorgimenti per rimediare. 

### Funzioni di hash per il chaining: moltiplicazione
Se non vogliamo che la scelta di `m` influenzi le prestazioni, possiamo usare il metodo della moltiplicazione.
Scegliamo una costante `A` tra 0 e 1 reale e costruiamo:
$$
h(k) = \lfloor m \cdot (k \cdot A - \lfloor k \cdot A) \rfloor ) \rfloor +1
$$

Questo metodo funziona bene per ogni costante `A`, esistendo pero' certe costanti 'note' per cui la letteratura riporta ottimi comportamenti, come ad esempio $A = \frac{\sqrt{5} - 1}{2}$.

### Funzioni di hash per il chaining: stringhe (addizione)
Come ci comportiamo quado le chiavi sono stringhe di un alfabeto $\sum$?
- Una soluzione naturale consiste nell'interpretare ogni stringa come un numero naturale. Questo puo' essere ottenuto utilizzando la codifica ASCII, quindi una stringa diventa un numero in base 127 che viene trasformato in base 10:
  $$
	ab7 = 97 \cdot (127)^2 + 98 \cdot (127) + 55 = 1564513 + 12446 + 55 = 1577014
   $$
   
> Un'altra funzione di hash possibile e' 'mid square':
> - $h(k) = \text{le R cifre centrali del ... } k^2$ 

E' davvero efficiente questo calcolo?
- Trasformare una stringa in un numero intero e' un'operazione che costa $\Theta(d)$ dove `d` e' il numero di caratteri della stringa.
Infatti, data una stringa:
$$ a_1 a_2 ... a_n $$
e la cardinalita' `B` di $\sum$, la trasformazione corrisponde a calcolare:
$$a1 · B^{d−1} + a2 · B^{d−2} + . . . + ad · B^0$$
cioe':
$$ ((a_1 \cdot B + a_2) \cdot B + a_3) \cdot B + ... a_d $$

Il problema che emerge nelle applicazioni reali e' quello della dimensione dei numeri ottenuti. Questi devono essere sottoposti poi ad operazioni aritmetiche, come il modulo o il confronto. Queste ultime non possono essere effettuate quando i numeri hanno dimensione troppo grande e hanno costo $\Theta(1)$. 
Ricordiamo che l'operazione e il modulo e' invariante rispetto all'addizione e, se `m` e' primo, anche rispetto alla moltiplicazione. Allora abbiamo che:
$$
\begin{align}
(a_i \cdot B + a_{i+1}) &=_m (z \cdot B +  a_{i+1}) \qquad &\text{se e solo se} \\
(a_i \cdot B) &=_m (z \cdot B) &\text{se e solo se} \\
a_i &=_m z
\end{align}
$$

Poiche' questa puo' essere generalizzata, ci da un metodo semplice per calcolare 
`h(k) = (k mod(m)) + 1`, con `k` **molto grande** senza mai dover memorizzare `k`.

#### Funzione modulo, correttezza e complessita'
``` Pseudocodice
proc HashComputeModulo(w, B, m) {
	let d = |w|
	z_0 = 0
	for (i=1 to d) z_i+1 = ((z_i * B) + a_i) mod m
	return z_d + 1 
}
```

`HashComputeModulo` prende parametri $w = a_1 a_2 ... a_d$, `B`(dimensione dell'alfabeto) e `m`.
Evidentemente questa funzione **termina** sempre, ha **complessità** $\Theta(d)$, ed e' **corretta** perche' utilizza le proprieta' dell'aritmetica modulare per `m` numero primo. 

## Tabelle hash: open hashing
Questa tecnica ha le seguenti caratteristiche importanti::
- si eliminano le liste e quindi i chaining
- una tabella hash di `m` elementi potra' ospitare al massimo `m` elementi e per ottenere questo risultato si rinuncia ad implementare la funzione di cancellazione.
L'indirizzamento aperto si basa sull'idea di provare piu' di una posizione sulla tabella, finche' se ne trova una libera oppure si ha la certezza che la tabella e' piena.

### Open hashing: operazioni
Data una chiave `k` da inserire in una tabella ad indirizzamento aperto per ottenere una posizione `h(k)`. Se questa e' libera, la chiave puo' essere inserita. Se invece la posizione e' gia' occupata, proviamo **un'altra posizione**: questa sequenza di tentativi si chiama **sequenza di probing**. Non tutte le sequenze di probing funzionano bene: la condizione e' che **tutte le posizioni della tabella devono essere provate** prima o poi.
Questa condizione si chiama **hashing uniforme** e generalizza la condizione di hashing uniforme semplice.
Data una funzione di hashing qualsiasi, definiamo una sequenza di probing come:
$$ h(k, i) $$
che dipende dalla chiave ma anche dalla posizione **precedentemente tentata**.

``` Pseudocodice
proc OaHashInsert(T, k) {
	i = 0
	repeat {
		j = h(k, i)
		if T[j] = nil
			then 
			T[j] = k
			return j
		else 
			i = i + 1
	}
	until (i = m)
	return "overflow"
}

proc OaHashSearch(T, k) {
	i = 0
	repeat {
		j = h(k, i)
		if T[j] = k
			then return j
		i = i + 1
	}
	until ((T[j] = nil) or (i = m))
	return nil
}
```

Come si ottiene una sequenza di probing uniforme?
Due metodi:
- **probing lineare**
	- la sequenza viene stabilita cosi': $$h(k, i) = ((h'(k) + i) \cdot  mod(m)) + 1$$
	- `h'` e' qualunque funzione di hashing uniforme semplice. Si vede chiaramente che le proprieta' di uniformita' e' rispettata per ogni chiave.
- probing quadratico
	- si scelgono due costanti $c_1$ e $c_2$ e si impone: $$ h(k, i) = ((h'(k) + c_1 \cdot i + c_2 \cdot i^2) \cdot  mod(m)) + 1 $$
	- anche in questo caso la sequenza e' uniforme.

Non studiamo in maniera esplicita e formale la **correttezza, complessità e terminazione** delle due operazioni in questo caso. La letteratura ci dice che tendenzialmente l'indirizzamento si comporta meglio del chaining e che il probing quadratico si comporta meglio di quello lineare. In quanto alla funzione di cancellazione, succede che eliminare un elemento sostituendolo con **nil** puo' rendere scorretta l'operazione di ricerca, dovuto al probing. Pertanto, quando si utilizza l'indirizzamento aperto, l'eliminazione di un elemento tipicamente avviene in forma virtuale (utilizzando una flag).

---
# Strutture dati per insiemi disgiunti 

## Insiemi disgiunti
Sono una struttura dati astratta, parzialmente dinamica, parzialmente sparsa e non basata sull'ordinamento.
La caratteristica principale di un insieme disgiunto e' che le operazioni ad esso associate sono tipicamente:
- `MakeSet` $\rightarrow$ costruisce un nuovo insieme disgiunto
- `Union` $\rightarrow$ unisce due insiemi disgiunti in uno solo
- `FindSet` $\rightarrow$ trova il rappresentante dell'insieme al quale appartiene l'elemento.
Ogni insieme e' dotato di un elemento rappresentativo.
Gli insiemi crescono solo in due modi:
- quando vengono creati (e contengono esattamente un elemento)
- quando vengono uniti due insiemi in uno solo che contiene gli elementi di entrambi

Immaginiamo di avere i seguenti insiemi: $S_1 = \{5, 12, 20\}, \space S_2 = \{7\}, \space S_3=\{13, 2\}$. Ognuno di essi puo' essere rappresentato da uno qualsiasi dei suoi elementi e cio' che dobbiamo mantenere e' l'informazione dell'insieme stesso, il quale non ha una vera struttura interna.
Indipendentemente dall'implementazione scelta, possiamo immaginare che `S` abbia almeno un array che contiene tutte queste chiavi e, per ognuna di esse, un informazione aggiuntiva che ci permetta di ricostruire tutta la struttura.

La particolare scelta delle operazioni che si vogliono rendere disponibili influenza la struttura dati, che tendenzialmente e' ottimizzata per quelle operazioni. Se si volesse offrire un'altra operazione diversa da quelle originali, questo risulterebbe impossibile o troppo costoso.
Gli insiemi disgiunti hanno una applicazione fondamentale in uno degli algoritmi su grafi che vedremo piu' avanti (per il calcolo dell'albero di copertura minimo).

Consideriamo questo scenario: abbiamo una rete molto grande di criminali e tutti usano molti **alias** diversi tra loro. I nostri informatori riescono, di tanto in tanto, a scoprire che due **alias** sono la
stessa persona. 

### Insiemi disgiunti: liste

Qual è la struttura dati ottima per mantenere questa informazione?
- L'implementazione piu' intuitiva per gestire `S` passa attraverso l'uso delle liste collegate. L'elemento $S \in S$ e' quindi una lista dotata degli attributi `S.head` (che punta al primo elemento) e `S.tail` (che punta all'ultimo elemento). Ogni elemento `x` e' dotato di `x.next` e `x.head` che punta all'insieme `S` che lo contiene.
  ![[InsiemiDisgiunti.png]]
  In questa versione l'informazione aggiuntiva che contiene ogni $S[i]$ e' un puntatore all'elemento `i` in memoria, cioe' alla casella `x` che contiene la chiave `i`. Lo chiamiamo per esempio `S[i].set`.
  ![[InsiemiDisgiunti2.png]]
  In questa maniera, implementare `MakeSet(x)` e' banale: crea un nuovo oggetto `S` tale che `S.head = S.tail = x`. Se poi decidiamo che il rappresentante di ogni `S` e' precisamente l'elemento puntato da `S.head`, allora implementare `FindSet(x)` e' altrettanto banale: dato `x` cerchiamo prima `x.head` poi `x.head.head` per arrivare al suo rappresentante.
  Entrambe le operazioni costano $O(1)$. 
  Tra le sue caratteristiche:
	  - E' commutativa
	  - Viene eseguita a partire da elementi dei due insiemi uniti
	  - Distrugge il secondo insieme 
In ogni operazione, gli elementi sono ipotizzati gia' **creati e nella memoria principale**.
Creare un nuovo insieme (`MakeSet`) significa:
  - creare un oggetto `S`, creare un oggetto `x` con la chiave che vogliamo e collegarli.
 La differenza con una lista 'normale' e' che l'oggetto 'lista' e' in memoria principale e non nello stack.
 L'operazione di `FindSet`, dato un oggetto `x`, che contiene una chiave della quale vogliamo conoscere il rappresentante, consiste nel seguire il puntatore `head` di `x` per arrivare ad `S` e poi nuovamente il puntatore `head`.
 L'operazione di `Union` di `x` e `y` consiste nel trovare $S_1$ ed $S_2$ e, se sono diversi, aggiornare $S_1.tail.next$ e $S_2.head$ e per ogni `z` tale che $z.head = S_2$, impostare $z.head = S_1$. 

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
> 2. tutte le `Union` costano $n-1 \rightarrow m = 2 * n-1$ 


Per poterlo calcolare, diciamo che `m` denota il numero di operazioni qualsiasi fatte su `S`, ed 
`n <= m` denota il numero di `MakeSet` tra le`m` operazioni.
Consideriamo il caso peggiore:
- Non e' difficile definire una situazione in cui abbiamo gli oggetti $x_1, ..., x_n$ ed ognuno costituisce il suo proprio insieme. Quindi abbiamo `n` operazioni `MakeSet` seguite da `n - 1 Union` in maniera che $m=2 \cdot n-1$. Spendiamo $\Theta(n)$ ooer generare gli insiemi. Nel caso peggiore, spendiamo 1 per la prima unione, 2 per la seconda e cosi' via fino all'ultima unione che costa `n`. Il totale e' $\Theta(n^2)$.
  Le operazioni `FindSet` non contribuiscono a cambiare la struttura di `S` e hanno un costo costante; pertanto le abbiamo escluse dall'analisi.

> Sapendo che `m` operazioni di cui `n` sono `MakeSet` e danno il caso peggiore nella situazione vista prima e che in quella situazione $2*n+1 = \Theta(n)$, il costo medio ammortizzato di un'oerazione e' $\frac{\Theta n^2}{\Theta(n)} = \Theta(n)$ 

Qual'e' la differenza tra l'analisi ammortizzata e quella del caso medio?
- Nell'analisi ammortizzata non ci sono considerazioni probabilistiche: si calcola il costo medio di ogni operazione nei casi ottimo, medio e pessimo, in maniera da tenere conto dell'influenza mutua tra operazioni.
Perche' non abbiamo avuto occasione di utilizzarla prima?
- Perche' altre strutture dati, come le liste, non sono tali che eseguire un operazione influenza in maniera evidente il costo di eseguire altre operazioni.

### Insiemi disgiunti: liste con unione pesata
Una prima strategia che possiamo usare per migliorare la situazione e' chiamata **unione pesata**.
Il principio sul quale si basa e' semplice:
- se manteniamo in ogni insieme `S` anche il numero degli elementi dell'insieme, allora possiamo implementare `Union` in maniera che gli aggiornamenti dei puntatori si facciano sempre sull'insieme piu' piccolo

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
Adesso il caso peggiore accade quando tutti gli `S` sono di dimensione uguale, L'analisi ammortizzata ci dira' che esiste un risparmio in media di tempo.
Mettiamoci nelle stesse condizioni di primaL `m` operazioni generiche di cui `n MakeSet`. Come nel caso precedente, ci dovremmo fermare quando avremo raggiunto un solo insieme con tutti gli element. Le operazioni `FindSet` vengono inizialmente escluse dal ragionamento.
Dati `n` insiemi tutti di un solo elemento, la situazione peggiore si verifica effettuando $\frac{n}{2}$ unioni: infatti, se non facessimo cosi', arriveremo ad avere un insieme con `n` elementi dopo solamente 
`n-1` passi e non avremmo la situazione peggiore.
Se, per comodita', ipotizziamo $n = 2^k$ per qualche `k`, allora possiamo proseguire ragionando nello stesso modo: $\frac{n}{2}$ unioni la prima volta, seguite da $\frac{n}{4}$ unioni e cosi' via, precisamente $log(n)$ volte.

Quanto e' il costo totale delle `m` operazioni?
- Ogni unione costa: 1 per la prima volta, 2 per la seconda, 4 per la terza e cosi' via.
  Il costo totale di tutte le unioni che possiamo fare prima di arrivare all'insieme con tutti gli elementi e' $\Theta(n \cdot log(n))$. 
  Nel caso peggiore (forzare che le `m` operazioni siano `n MakeSet` seguite da tutte le `Union` possibili) ci da un costo totale di $\Theta(n \cdot log(n))$.
  In questo caso, aggiungere qualche `FindSet` nel gruppo di `m` operazioni puo' solo migliorare la complessita', ed e' per questo che le escludiamo dall'analisi del caso peggiore.
Possiamo migliorare queste prestazioni?
- La strategia implementativa che ci permette una ulteriore miglioria consiste in tre passi:
	1. cambiare la rappresentazione
	2. adattare l'unione pesata alla nuova rappresentazione
	3. modificare l'operazione `FindSet` per renderla **attiva**

### Insiemi disgiunti: foreste di alberi
Il modo piu' efficiente per trattare gli insiemi disgiunti sono le **foreste di alberi**.

La rappresentazione e' basata in **alberi** piuttosto che liste. Un nodo `x` conteine le seguenti informazioni:
- `x.p` (il padre)
- `x.rank` (un limite superiore all'altezza del sotto-albero radicato in `x`)
Gli alberi sono `k-ari` e formano una **foresta** di $\mathcal{S}$.
L'operazione di `MakeSet` e' la stessa:
- si crea un nuovo albero di altezza massima 0 tale che il padre dell'unico nodo `x` e' `x` stesso.
==Attenzione: questi alberi sono liste particolari e non vanno confusi con alberi e grafi.==

Un nodo di un albero `k-ario` **non ha, in generale, nessun puntatore ai figli**. Infatti, non abbiamo un limite superiore a quanti figli posso avere e non ci interessa agli scopi di questa struttura dati.
Il rango **non** e' la misura dell'altezza ma un suo limite superiore. Questo significa che un albero in questa struttura puo' avere altezza `h` e rango qualsiasi (`>= h`).

![[InsiemiDisgiunti3.png]]

L'operazione di unione, in due fasi, consiste nel trovare rappresentanti degli elementi utilizzati come indici; se le due radici sono `x` e `y`, si sceglie quello il cui rango sia inferiore e si aggiorna **solo il padre**, rendendolo uguale all'altro elemento. Con il criterio **unione per rango** (il corrispondente dell'unione pesata nella versione con le liste), il rango dell'insieme risultante cambia **solo se i ranghi dei due componenti erano uguali** e rimane invariato negli altri casi.
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
Il calcolo della **complessità** di `m` operazioni in questa implementazione e' molto difficile.
La ragione per quale l'unione per rango sommata alla compressione del percorso migliora le prestazioni globali sono chiare. In sostanza, l'unione effettua sempre al massimo un aggiornamento sui puntatori. L'operazione `FindSet` aggiorna un certo numero di puntatori, ma questi, una volta aggiornati, non vengono toccati mai piu' e il prossimo `FindSet` su elementi del percorso che e' gia' stato compresso costera' un tempo costante. 
Sia $\alpha$ una certa funzione da $\mathbb{N}$ a $\mathbb{N}$ che cresce approssimativamente come l'inverso della funzione di Ackermann (cioe' cresce in maniera **estremamente** lenta). Nel caso concreto in questione abbiamo che $\alpha(n)$ e' minore o uguale a 4 per $n \le 10^80$.
Una corretta analisi di `m` operazioni darebbe che il costo totale e' $O(m \cdot \alpha(n))$, che, in ogni situazione pratica, e' lo stesso che $O(m)$. 

## Conclusione
Gli insiemi disgiunti sono un esempio di struttura dati non intuitiva. E' un esempio di struttura dati che fornisce idee non banali a chi la studia, che possono essere riutilizzate in altri contesti. 

---
# Alberi e Alberi binari di ricerca

## Alberi
Gli alberi sono strutture dati fondamentali dinamiche e sparse. A seconda degli usi che se ne fanno, possono essere basate sull'ordinamento oppure no.
Da un lato possiamo dire che gli alberi generalizzano le liste:
- se vediamo il puntatore **next** come un successore, allora nelle liste il successore e' unico e negli alberi no.
In questo senso, possiamo dire che i grafi, a loro volta generalizzano gli alberi.

Dal punto di vista delle strutture dati, liste, alberi e grafi sono oggetti molto diversi e con usi diversi; il fatto che uno sia la generalizzazione dell'altro non significa che gli utilizzi si ereditino.
==Attenzione a non confondere i vari concetti==: qui studiamo una struttura dati concreta; le heaps sono array che possono essere convenientemente viste come alberi; un albero di decisione, o un albero di ricorsione, sono strutture concettuali (non strutture dati).
La struttura dati **albero** e' troppo generica e ubiqua per essere associata a uno o piu' nomi specifici.

Un **albero radicato** (semplicemente **albero**) e' un grafo aciclico connesso tale che ogni coppia di vertici e' connessa da al piu' un cammino.
- I vertici vengono chiamato **nodi**. 
- Un'albero e' `k-ario` se ogni nodo ha al piu' `k` figli distinti.
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
Consideriamo la generica visita di un nodo `x` tale che ci sono `k` nodi nel sotto-albero radicato a sinistra il che implica che ci sono $n-k-1$ nodi nel sotto-albero radicato a destra. Possiamo considerare che il costo della visita a `x` stesso sia costante.

Il costo totale e' dato dalla ricorrenza:
$$
T(n) = T(k) + T(n-k-1) + \Theta(1)
$$

Sospettiamo che $T(n) = \Theta(n)$ e dimostriamo che $T(n)=O(n)$. Ipotizziamo che $T(n) \le c \cdot n$ per qualche $c>0$:
$$
\begin{align}
T(n) &= T(k) + T(n-k-1) + \Theta(1) \\
&\le c \cdot k + c \cdot (n-k-1) + \Theta(1) \qquad &\text{ipotesi} \\
&= c \cdot k + c \cdot n - c \cdot k - c + \Theta(1) &\text{calcolo} \\
&= c \cdot n &\text{calcolo} 
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
- trovare un algoritmo per stampare tutte e sole le chiavi della **frontiera** di `T`;
- trovare un algoritmo per stampare tutte e sole le chiavi del **costato sinistro** (o destro) di `T`;
- dire quale/quali visite sono necessarie per ricostituire la struttura di `T`;
- arricchire la struttura di `T` in maniera che ogni nodo punti anche allo **zio** (se esiste), otlre che al padre, e a molti altri.

## Alberi binari di ricerca: introduzione
Gli **alberi binari di ricerca (BST)** sono una struttura:
- dinamica;
- basata sull'ordinamento;
- implementata in maniera sparsa.
Associamo gli alberi binari di ricerca le operazioni di inserimento, cancellazione, ricerca, minimo, massimo, successore e predecessore. Possiamo farlo perche' la struttura e' basata sull'ordinamento delle chiavi.
I nodi di un BST sono nodi di un albero definiti come nel caso generale.

Le regole che un albero binario di ricerca deve rispettare (anche note come **proprieta' BST**), sono:
1. Per ogni nodo `x`, se un nodo `y` si trova nel **sotto-albero sinistro**, allora `y.key <= x.key`;
2. Per ogni nodo `x`, se un nodo `y` s trova nel **sotto-albero destro**, allora `y.key > x.key`.
Dunque si puo' dire che un BST e' **parzialmente ordinato**.

![[BST.png]]

### Alberi binari di ricerca: creazione e visita di un albero
I BST si creano vuoti come nel caso generale ed hanno esattamente la stessa struttura. Tutte le visite che abbiamo visto negli alberi generici hanno senso anche negli alberi binari di ricerca.
In alcuni casi (come quello della visita in order) restituiscono un risultato ancora piu' naturale.
Se un albero e' un BST ordinato, il risultato della sua visita in order e' l'insieme delle chiavi ordinato.

### Alberi binari di ricerca: ricerca, minimo e massimo
Vogliamo una struttura che dati `x, k` ritorna un puntatore al nodo che contiene `k`, se esiste, e ritorna **nil** altrimenti.

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
La **correttezza** di `BSTTreeSearch` e' triviale: se la chiave esiste, questa vinee trovata sicuramente, perche' tutti i nodi nella parte dell'albero dove la chiave deve essere. 
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
- dato un nodo `x` in un BST, trovare il nodo `y`, se esiste, tale che `y.key` e' il **successore immediato** di `x.key` nell'ordinamento naturale delle chiavi.

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
- se `x` ha figlio destro $\rightarrow$ il successore immediato e' il **minimo** del sottoalbero destro di `x`;
- se `x` non ha figlio destro $\rightarrow$ il successore immediato si trova tra i suoi antenati: bisogna risalire finche' la relazione padre-figlio e' di tipo padre-figlio **sinistro**

#### Correttezza e complessità di `BSTTreeSuccessor`

La **correttezza** di `BSTTreeSuccessor` e' evidente, considerato che implementa esattamente la strategia vista prima. La **complessità** di `BSTTreeSuccessor` e' proporzionale all'altezza dell'albero, cioe' $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio.

### Alberi binari di ricerca: inserimento
L'operazione di **inserimento** di un nodo e' quella che ci permette di costruire e modificare un dato BST. Stabiliamo che l'inserimento opera su un BST (possibilmente vuoto) denotato da `T`, tale che `T.root = nil` quando l'albero e' vuoto, e punta alla radice di `T` in caso contrario. Si inserisce in `T` un nodo `z` in maniera che `z.key` contiene la chiave da inserire, `z.left = z.right = nil`; si noti che il nuovo nodo inserito finisce sempre per essere una nuova foglia di `T`.

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
Vogliamo mostrare che `BSTTreeInsert` e' **corretta**, cioe' se `T` e' un BST e `T'` e' il risultato di inserimento, allora `T'` e' un BST. Se `T` e' vuoto, il ciclo **while** non si segue e tra le istruzioni restanti si segue solo la prima, mettendo z come radice di `T'`, che diventa un albero con un solo nodo e quindi corretto. Sia quindi `T` un BST corretto non vuoto. Vogliamo mostrare che l'**invariante** del ciclo e': la posizione corretta di `z` e' nel sottoalbero radicato in `x` e `y` ne mantiene il padre.
Questa e':
- vera all'inizio (**caso base**), perche' `x = T.root`;
- vera anche dopo l'i-esima esecuzione del ciclo (**caso induttivo**): assumendola vera dopo la (i-1)-esima esecuzione, `z` viene confrontato con `x` (che non e' ancora una foglia) e `x` viene spostato correttamente, cosi' come `y`, mantenendo vera la proprieta'.
Alla fine del ciclo, `x = nil`, ed e' precisamente la posizione di `z`: poiche' si e' persa la relazione padre-figlio tra `y` e `x`, le ultime due istruzioni recuperano questa relazione per ottenere la posizione corretta.

Osserviamo che e' l'operazione di inserimento che si occupa di decidere se le chiavi uguali vanno a sinistra o a destra. Tutte e due le scelte sono buone.
La **complessità** di `BSTTreeInsert` e' proporzionale all'alteza dell'albero e pertanto e' $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio.

### Alberi binari di ricerca: eliminazione
Eliminare un elemento da un BST dato e' un'operazione leggermente piu' difficile delle altre. 
Considerando un nodo `z` qualsiasi:
- se `z` e' foglia, si puo' eliminare semplicemente;
- se `z` ha un solo figlio, allora l'operazione di eliminazione coincide con l'operazione di eliminazione in una lista;
- se `z` ha due figli, allora dobbiamo trovare il modo di ricostruire l'albero dopo l'eliminazione.
Se `z` ha due figli, il nodo che contiene la chiave successore di quella contenuta in `z` certamente non ha mai figlio sinistro. Questa osservazione e' importante perche' ci permette di ridurre il caso difficile ad uno piu' semplice.

Procediamo cosi':
- se `z` non ha figli sinistri, o e' una foglia, allora **trapiantiamo** il sotto-albero `z.right` al posto di `z` (anche se `z.right` e' **nil**: caso senza figli).
- se `z` ha figlio sinistro, ma non destro, allora **trapiantiamo** il sotto-albero `z.left` al posto di `z` (`z.left` non puo' essere **nil**, altrimenti saremmo nel caso anteriore).
- se `z` ha due figli, allora andiamo a prendere il suo successore immediato `y`, che si trova nel sotto-albero destro di `z` e non ha **al piu' un figlio**.
  Il nodo `y` va a prendere il posto di `z` e se `y` e' figlio immediato di `z` allora il figlio destro di `z` diventa il figlio destro di `y` e il resto rimane invariato, altrimenti (`y` e' nel sotto-albero destro di `z` a non e' suo figlio immediato) allora prima rimpiazziamo `y` con il suo figlio destro, e poi rimpiazziamo `z` con `y`.

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
La **complessità** di `BSTTreeDelete`, che non contiene cicli, e' nuovamente $\Theta(n)$ nel caso peggiore e $\Theta(log(n))$ nel caso medio; questo si deve naturalmente alla presenza di una chiamata a `BSTTreeMinimum`.

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

---
# Alberi red-black (RBT)

## Albero red-black: introduzione
Un **albero red-black** (RBT) e' un albero binario di ricerca (BST) **bilanciato** per costruzione. Possiede tutte le caratteristiche di un BST, ma la sua altezza e' sempre $\Theta(log(n))$, dove `n` e' il numero di elementi di un albero.
Un RBT e' una struttura dati dinamica, basata sull'ordinamento e sparsa. Tutte le operazioni, ed in particolare quelle di ricerca, che funzionano in un tempo proporzionale all'altezza diventano esponenzialmente piu' efficienti su un RBT.
La caratteristica principale di queste strutture e' che l'inserimento e l'eliminazione mantengono la proprieta' di bilanciamento.

Ogni nodo in un RBT ha un'informazione in piu' rispetto a un nodo di BST: oltre a un puntatore al padre, i puntatori ai due figli e la chiave, abbiamo il **colore** (`x.color`), che per convenzione e' rosso o nero.
Ogni foglia possiede due figli **virtuali**, che non contengono chiave e sono sempre di colore nero. 
Per convenzione, il padre della radice e' anche lui un nodo virtuale senza chiave, senza figli e di colore nero.

Le regole che ogni RBT deve rispettare sono:
1. Ogni nodo e' rosso o nero;
2. La radice e' nera;
3. Ogni foglia (esterna, **nil**) e' nera;
4. Se un nodo e' rosso, entrambi i suoi figli sono neri;
5. Per ogni nodo, tutti i percorsi semplici da lui alle sue foglie, contengono lo steso numero di nodi neri.

Chiameremo i nodi di un albero RB **interni**, per distinguerli dai nodi **esterni** che aggiungiamo in maniera artificiale a ogni albero RB. Una foglia esterna e' un nodo che ha tutte le proprieta' di ogni altro nodo ma non porta alcuna chiave, ed e' sempre di colore nero.
Ogni nodo interno di un RBT ha sempre due figli, a differenza di quello esterno che non ha figli.
Dal punto di vista implementativo, definiamo una sentinella `T.nil` come un nodo con tutte le proprieta' di un nodo `T` e colore fissato a nero per il ruolo di foglia esterna.

![[RBT_introduzione.png]]

Il principio fondamentale degli RBT e' che le proprieta' sono valide quando l'albero e' vuoto e vengono mantenute tali dopo ogni inserimento e eliminazione. Dobbiamo ancora dimostrare che esse garantiscono il bilanciamento dell'albero - a meno di una costante. 
Cominciamo definendo l'**altezza nera** (`bh(x)`) di un nodo `x` in `T` come il numero di nodi neri si qualsiasi percorso semplice da `x` (senza contare `x`) a una foglia esterna (contandola).
L'altezza nera di `T` e' `bh(T.root)`.

Dimostriamo che se `T` e' un RBT con `n` nodi interni (quindi escludendo le foglie esterne), allora la sua altezza massima e' $2 \cdot log(n+1)$. 
Mostriamo che il sotto-albero radicato in `x` contiene almeno $2^{bh(x)}-1$ nodi interi, per induzione.
Quando `bh(x)` e' 0, allora per definizione `x = T.Nil`, e il sotto-albero indicato in `x` non ha nodi interni; l'alteza nera di `x` e' 0 (perche' non si include il nodo stesso), ed abbiamo che $2^{bh(x)}-1 = 1-1 = 0$, come volevamo. Se `bh(x)` e' positiva, allora l'altezza nera di entrambi i suoi figli e' almeno `bh(x) - 1`. Per ipotesi induttiva ognuno dei due sotto-alberi ha almeno $2^{bh(x)-1}-1$ nodi interni. Quindi il sotto-albero radicato in `x` ha almeno $2 \cdot (2^{bh(x)-1}-1)+1$ nodi interni, che e' esattamente $2^{bh(x)}-1$.

Consideriamo adesso `T` di altezza `h`. Per la proprieta' 4, almeno la meta' dei nodi della radice (esclusa) ad una foglia si qualsiasi rampo e' nera. Quindi $\text{bh(T.root)} \ge \frac{h}{2}$. Dalla proprieta' precedente, il numero `n` di nodi in `T` e' $n \ge 2^{\text{bh(T.root)}}-1$, cioe' $n \ge 2^{\frac{h}{2}}-1$.
Quindi: 
$$
\begin{align}
n &\ge 2^{\frac{h}{2}}-1 \qquad &\text{risultato precedente} \\
n+1 &\ge 2^{\frac{h}{2}} &\text{calcolo algebrico} \\
log(n+1) &\ge \frac{h}{2} &\text{proprieta' logaritmi} \\
h &\le 2 \cdot log(n+1) &\text{tesi}
\end{align}
$$
Un albero binario completo ha altezza `h` sempre maggiore o uguale a $log(n)-1$, dove `n` e' il numero di nodi totali.
Pertanto, $log(n)-1 \le h \le 2 \cdot log(n+1)$, cioe' $h = \Theta(log(n))$.

## Alberi red-black: rotazioni
Inserimento ed eliminazioni in un RBT possono violare le proprieta' e che la maggiore difficolta' nell'implementare queste procedure consiste precisamente nel modificare la struttura dell'albero per ripristinare queste proprieta'.
Un passo intermedio fondamentale per questa riparazione e' la **rotazione**, che puoi' essere destra o sinistra e che preserva le proprieta' BST. L'idea e' che possiamo ribilanciare l'albero e poi preoccuparci dei colori.
Risolviamo il problema di **rotazione sinistra**: dato un RBT `T` ed un nodo `x` in `T` con figlio destro `y`, ottenere un nuovo albero `T'`, dove `y` ha come figlio sinistro `x`.
Simmetricamente, potremmo definire il problema della rotazione destra. In entrambi i casi la complessita' e' $\Theta(1)$.

``` Pseudocodice
proc BSTTreeLeftRotate(T, x) {
	y = x.right
	x.right = y.left
	if (y.left != T.Nil)
		then y.left.p = x
	y.p = x.p
	if (x.p = T.Nil)
		then T.root = y
	if ((x.p != T.Nil) and (x = x.p.left))
		then x.p.left = y
	if ((x.p != T.Nil) and (x = x.p.right))
		then x.p.right = y
	y.left = x
	x.p = y
}
```

Una volta capito come funzionano i cambi di puntatori, mostrare la **correttezza** delle rotazioni e' immediato. Inoltre, si vede subito che la complessita' e' costante in entrambi i casi destro e sinistro.

## Alberi red-black: inserimento
Risolviamo il problema di inserire un nodo `z` in un RBT `T` in maniera da mantenere tutte le proprieta' di `T`. Chiaramente usiamo `BSTTreeInsert` cosi' com'e', abbiamo la garanzia che la proprieta' BST sia rispettata. Se il nodo inserito e' colorato di rosso, allora anche la proprieta' 5 e' rispettata; inoltre, poiche' `z` sara' sempre una nuova foglia, inserendo correttamente le sue foglie esterne, garantiamo anche la proprieta' 3. La proprieta' 1 e' rispettata semplicemente assegnando il colore (rosso) a `z`. Quindi, solo due proprieta' possono essere violate: se `z` diventa la radice, allora **violiamo 2**, se invece `z` diventa figlio di un nodo rosso, **violiamo 4**.

``` Pseudocodice
proc BSTTreeInsert(T, z) {
	y = T.Nil
	x = T.root
	while (x != T.Nil)
		y = x
		if (z.key < x.key)
			then x = x.left
			else x = x.right
	z.p = y
	if (y = T.Nil)
		then T.root = z
	if ((y != T.Nil) and (z.key < y.key))
		then y.left = z
	if ((y != T.Nil) and (z.key >= y.key))
		then y.right = z
	z.left = T.Nil
	z.right = T.Nil
	z.color = RED
	RBTreeInsertFixup(T, z)
}
```

Nell'albero di esempio, inseriamo `z` con chiave 27, ottenendo una violazione della proprieta' 4:
![[RBT_inserimento.png]]

``` Pseudocodice
proc RBTreeInsertFixup(T, z) {
	while (z.p.color = RED)
		if (z.p = z.p.p.left)
			then RBTreeInsertFixUpLeft(T, z)
			else RBTreeInsertFixUpRight(T, z)
		T.root.color = BLACK
}

proc RBTreeInsertFixUpLeft(T, z) {
	y = z.p.p.right
	if (y.color = RED) then
		z.p.color = BLACK
		y.color = BLACK
		z.p.p.color = RED
		z = z.p.p
	else 
		if (z = z.p.right) then 
			z = z.p
			TreeLeftRotate(T, z)
		z.p.color = BLACK
		z.p.p.color = RED
		TreeRightrRotate(T, z.p.p)
}

proc RBTreeInsertFixUpRight(T, z) {
	y = z.p.p.left
	if (y.color = RED) then
		z.p.color = BLACK
		y.color = BLACK
		z.p.p.color = RED
		z = z.p.p
	else 
		if (z = z.p.left) then 
			z = z.p
			TreeRightRotate(T, z)
		z.p.color = BLACK
		z.p.p.color = RED
		TreeLeftRotate(T, z.p.p)
}
```

## Alberi red-black: correttezza dell'inserimento
La scelta che si fa all'inizio di  `RBTreeInsertFixup` genera due casi, che dipendono dal fatto che `z.p` sia figlio destro o sinistro di `z.p.p`.
All'interno di ogni caso vi sono tre sotto-casi, che si distinguono dal colore di y (lo **zio** di `z`):
- se e' rosso e' un caso
- se e' nero:
	- se `z` e' figlio destro e' un secondo caso
	- se `z` e' figlio sinistro e' un terzo caso

Il totale e' quindi di 6 casi, i primi tre completamente simmetrici ai secondi tre. Osserviamo che se `z` e' la radice (abbiamo inserito un nodo in un albero vuoto), allora `z,p = T.Nil` e 
`T.Nil.color = BLACK`: quindi la condizione del ciclo **while** e' corretta e determina un corretto caso di terminazione. Similmente, se `z` e' un figlio diretto della radice, allora `z.p` e' la radice, e quindi `z.p.p = T.Nil`, pertanto `z.p.p.left` e `z.p.p.right` sono entrambi `Nil` e diversi da `z.p`: quindi tutte le condizioni **if** sono ben definite.

Analizziamo il codice. L'idea di fondo e': se esiste un problema dopo l'inserimento (violazione della proprieta' 2 o della proprieta' 4), questo si **spinge verso l'alto** con il caso 1. Quando non e' piu' possibile, si salta al caso 2 (immediatamente convertito al caso 3) o al caso 3: una rotazione risolve il problema in forma definitiva e garantisce l'uscita dal ciclo (**terminazione**). 
Per mostrare la **correttezza**, usiamo la seguente **invariante**: `z` e' rosso, se `z.p` e' al radice, allora e' nera, e se `T` viola qualsiasi proprieta', allora ne viola esattamente una, che e' la 2 o la 4.
La condizione di uscita (nei tre casi) e' che `z.p` e' di colore nero; quindi l'invariante sommata alla condizione di uscita piu' l'ultima istruzione di `RBTreeInsertFixup` ci da la correttezza.

Sappiamo che `T` e' un RBT legale prima di chiamare `RBTreeInsertFixup`. Per quanto riguarda l'**inizializzazione**, dobbiamo mostrare che l'invariante e' vera prima di chiamare `RBTreeInsertFixup` Osserviamo, prima di tutto, che `z` viene inserito rosso. Inoltre, se `z.p` e' la radice, allora `z.p` era nera e prima di chiamare `RBTreeInsertFixup` questo non e' cambiato. Infine, sappiamo che le proprieta' 1, 3 e 5 non sono violate alla chiamata di `RBTreeInsertFixup`. Se `T` viola 2, deve essere perche' `z` e' la radice (e `T` era vuoto prima dell'inserimento); in questo caso `z.p = z.left = z.right = T.Nil` sono tutti nodi neri, percio' la proprieta' 4 non e' violata e la violazione della 2 e' l'unica. Se invece `T` viola 4, poiché `z.left = z.right = T.Nil` sono neri, e il resto di `T` non ha violazioni, deve essere perché `z.p` è rosso come `z`.

Cosa accade dopo la fine dell'inserimento?
- Al termine della procedura`z.p` e' nero. 
- La proprieta' 4 e' rispettata al termine.
- Se al termine del ciclo la proprieta' 2 e' violata, l'ultima linea di codice la ripristina.

Ci rimane da dimostrare che l'invariante e' mantenuta da un ciclo al seguente. Dei sei casi da analizzare ne analizziamo solo tre, assumendo che `z.p` e' figlio sinistro di `z.p.p`. Stiamo quindi assumendo che si esegue `RBTreeInsertFixUpLeft`.

- **Caso 1**: lo zio `y` di `z` e' rosso. Poiche' `z.p.p` e' nero, coloriamo di nero sia `z.p` che `y` e coloriamo di rosso `z.p.p`, per mantenere la proprieta' 5. Adesso `z.p.p` diventa `z` (quindi spostiamo il potenziale problema un passo piu' in alto). Dobbiamo mostrare che il nuovo `z` e' tale che l'invariante e' mantenuta. Prima di tutto, `z` e' rosso; poi `z.p` non cambia colore, quindi, se e' la radice, e' rimasta nera; infine, le proprieta' 1 e 3 non sono a rischio e sappiamo gia' che 5 e' mantenuta: se `z` e' la radice, allora e' rossa e si viola 2, giacche' `z.p = T.Nil` e' nero, se il nuovo `z` non e' la radice allora solo 4 puo' essere ancora violata e grazie alle altre tre ipotesi ed alla correzione nel ciclo seguito, questa violazione e' dovuta a che `z.p` e' rosso.
  
- **Caso 3**: lo zio di `y` e' nero e `z` e' figlio sinistro di suo padre. Il caso 2 (`z` e' figlio destro di suo padre) si riporta immediatamente al caso 3 attraverso una rotazione ed un ricoloramento. Il nodo `z.p` diventa nero e il nodo `z.p.p` diventa rosso. La rotazione a destra su `z.p.p` ripristina la proprieta' 5. Ci rimane da mostrare che `z`  e' tale che l'invariante e' mantenuta: prima di tutto, `z` e' rosso, poi, se `z.p` e' la radice, e' diventata nera; infine le proprieta' 1 e 3 non sono a rischio e sappiamo gia' che 5 e' mantenuta; inoltre in questo caso la proprieta' 2 non si puo' violare. L'unica violazione alla proprieta' 4 (`z` e `z.p` entrambi rossi) viene corretta e non ci sono altre violazioni.

Concludendo l'analisi dell'inserimento (che ha **complessità**, nel caso peggiore, $\Theta(h) = \Theta(log(n))$: nel peggior caso si esegue tutto il ciclo while seguendo il caso 1 e si percorre un ramo intero), osserviamo che il caso 1 si verifica in un RBT previamente bilanciato e si sistemano i colori per mantenere, al peggio, un leggero sbilanciamento. I casi 2 e 3 invece operano su un RBT gia' leggermente sbilanciato: ma questo si rivela facile da sistemare grazie a, al massimo, due rotazioni e poi un ricoloramento sistematico. Questo complesso sistema ci permette di mantenere una struttura bilanciata anche quando si opera un inserimento di elementi in ordine, che invece genererebbe un BST molto sbilanciato. Come si puo' intuire, l'eliminazione di un nodo da un RBT e' **molto** complessa. Anche questa operazione ha costo $\Theta(h) = \Theta(log(n))$.

## Alberi binari di ricerca, red-black e liste: confronto


|               | Liste       | BST (c. medio)   | BST (c. peggiore) | RBT (c. peggiore) |
| ------------- | ----------- | ---------------- | ----------------- | ----------------- |
| Inserimento   | $\Theta(1)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Cancellazione | $\Theta(1)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Visita        | $\Theta(n)$ | $\Theta(n)$      | $\Theta(n)$       | $\Theta(n)$       |
| Ricerca       | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Successore    | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Predecessore  | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Massimo       | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |
| Minimo        | $\Theta(n)$ | $\Theta(log(n))$ | $\Theta(n)$       | $\Theta(log(n))$  |

---
# Alberi B (BT)

## Alberi B: introduzione
Un albero B (BT) generalizza un albero RB con fini differenti, ma mantenendo la sua proprieta' fondamentale di bilanciamento. Un albero B e' sempre completo.
Nel modello piu' semplice e diffuso di computazione, dobbiamo distinguere tra memoria **principale** e **secondaria**. La memoria secondaria e' vari ordini di grandezza piu' capace, ma e' piu' lenta della memoria primaria.
Gli alberi B sono una struttura dati ottimizzata per minimizzare gli accessi al disco; la complessita' delle operazioni si da lungo le direttive, tempo di CPU e numero di accessi al disco. La loro principale applicazione e' nelle basi di dati.
Gli alberi B sono una struttura dinamica, basata sull'ordinamento e memorizzata in maniera sparsa.

> Alberi B $\rightarrow$ BT (Balanced Tree)

Un **albero B (BT)** si caratterizza per possedere una varieta' (che in questo contesto si conosce come **branching factor** (t)) superiore a 2, spesso dell'ordine di migliaia, un'altezza proporzionale a un algoritmo a base molto alta di `n`, dove `n` e' il numero di chiavi, per avere nodi che contengono molte chiavi, tra loro ordinate, e per crescere verso l'alto, non verso il basso: un nodo comincia con essere la radice, e poi si converte in nodo interno generando una nuova radice.
La complessita' delle operazioni e' proporzionale all'altezza: quindi i BT possono essere usati come degli RBT.

Un nodo `x` in un BT si caratterizza per avere comunque il puntatore al nodo padre (`x.p`), ma glia altri dati sono diversi da nodi degli alberi binari visti fino ad ora. Infatti, abbiamo:
- numero delle chiavi memorizzate nel nodo (`x.n`)
- l'informazione sull'essere, o meno, una foglia (`x.leaf` = 1 se x e' foglia)
- i puntatori agli $n+1$ figli di `x` (`x.c_1, ..., x.c_x.n+1`) definiti se `x` e' foglia
- `n` chiavi (`x.key_1, ..., x.key_x.n`) invece di una
Il sotto-albero puntato da `x.c_i` e' legato alle chiavi `x.key_i-1` e `x.key_i`.

In un nodo `x` il numero di chiavi, e quindi il branching factor, e' vincolato da un parametro che si chiama **grado minimo**, si denota con `t` ed e' sempre `>= 2`.
Le proprieta' di un albero B sono:
1. Ogni nodo, tramite la radice, ha almeno `t-1` chiavi;
2. Ogni nodo puo' contenere al massimo `2 * t - 1` chiavi;
3. Per ogni nodo `x`, `x.key_1 <= x.key_2 <= ... <= x,key_x.n`;
4. Per ogni nodo `x`, se un nodo `y` contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono minori o uguali a `x.key_i`
5. Per ogni nodo `x` , se un nodo `y` e; contenuto nel sotto-albero radicato in `x.c_i`, allora tutte le sue chiavi sono maggiori di `x.key_i-1`

Esempio di albero B:
![[BT_esempio.png]]
E' immediato osservare che per `t=2`, si ottiene un albero che e' sempre **isomorfo** a un albero red-black. Questi particolari alberi B sono noti come **alberi 2-3-4**.

Le prime due regole implicano che ogni nodo interno, tranne la radice, deve avere almeno `t` figli. Se l'albero non e' vuoto, allora la radice ha almeno una chiave; un nodo interno puo' avere fino a `2 * t` figli: in questo caso lo chiamiamo **nodo pieno**.
Come conseguenza delle regole, l'altezza massima di un BT `T` con `n` chiavi e grado minimo 
`t >= 2` e': 
$$ h \le log_t(\frac{n+1}{2}) = O(log_t(n)) $$
e tutte le foglie sono alla stessa altezza. Dunque un albero B e' sempre completo.

## Alberi B: caratteristiche e altezza
Per dimostrare la proprieta' sull'altezza logaritmica, osserviamo che nel caso peggiore la radice di `T` contiene una sola chiave e tutti gli altri almeno `t-1`. Percio', `T` di altezza `h` ha almeno due nodi ad altezza 1, almeno $2 \cdot t$ di altezza 3, almeno $2 \cdot t^2$ ad altezza 3, e cosi via, fino a $2 \cdot t^{h-1}$ ad altezza `h`. Percio' il numero di chiavi e':
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
L'operazione di `BTreeSearch` e' la generalizzazione della ricerca su BST. Dobbiamo prendere una decisione tra molte possibilita' per ogni nodo esplorato: invece di scegliere tra due figli, scegliamo tra `x.n + 1` possibili figli. `BTreeSearch` prende in input un puntatore `x` ad un nodo di `T`, ed una chiave da cercare, e ritorna un puntatore ad un nodo `y` piu' un indice `i` (nel nodo) nei casi positivi, o **nil** se la chiave cercata non esiste nel sotto-albero radicato in `x`.
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
Per quanto riguarda la **complessità**, e; facile vedere che si operano, al massimo, $O(h) = O(log_t(n))$ accessi al disco. Inoltre, se utilizziamo la ricerca lineare sul nodo, otteniamo che il tempo totale di CPU nel caso peggiore e' $\Theta(t \cdot h) = \Theta(t \cdot log_t(n))$. La forza dei BT non si apprezza nella notazione $O()$. 

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

In sintesi, cerchiamo di riempire un nodo fino a quando diventa pieno; quando il nodo e' pieno, **dividiamo** il nodo in questione (che ha $2 \cdot t-1$ chiavi) in due nodi di $t-1$ ciascuno, ed inseriamo la nuova chiave nel nodo padre: se il padre diventa pieno i seguito a tale inserimento, ripetiamo l'operazione un livello piu' in alto. Quindi `T` cresce solamente quando la divisione ha luogo sulla radice: in questo caso si crea un nuovo nodo radice e si opera la divisione. 

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

La procedura assume che `x` sia un nodo interno non pieno gia' nella memoria principale e che il nodo figlio `x.c_i`, anche lui gia' nella memoria principale, sia pieno. Il nodo `x.c_i` e' diviso in due nodi, ognuno con la meta' delle chiavi ($2 \cdot t-1$ e' sempre dispari!).

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
proc BTreeInsertNopnFull(x, k) {
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

La condizione di **non essere un nodo pieno** e' certamente vera all'entrata di `BTreeInsertNonFull`. Se `x` e' un nodo foglia, e non e' pieno, ci limitiamo a scegliere la posizione della nuova chiave e a muovere le altre chiavi per mantenere l'ordine. I puntatori ai figli sono tutti **nil** e cosi' rimangono. Altrimenti, si va a cercare la posizione per la scelta del figlio di `x` dove proseguire la ricerca della foglia corretta. 
Quando il figlio viene caricato in memoria principale possono succedere due cose:
- non e' pieno, in questo caso semplicemente si fa una chiamata ricorsiva, giacche' sono rispettate le condizioni;
- e' pieno, in quest caso, operiamo la divisione, scegliamo tra i due nodi nuovi qual'e' quello giusto e facciamo la chiamata ricorsiva.

## Correttezza e complessità di `BTreeInsert`
L'inserimento in BT e' una procedura tail-recursive (`BTreeInsertNonFull`). Quindi possiamo mostrare la **correttezza** usando un invariante.
Scegliamo: **all'inizio di ogni esecuzione di `BTreeInsertNonFull` `x` e' non pieno, e `k` va inserito nel sotto-albero radicato in `x`**.
L'invariante e' vera all'inizio (**caso base**), perche' la procedura viene chiamata sulla radice dell'albero. Supponiamo che sia vera all'inizio di una certa esecuzione di `BTreeInsertNonFull` (**caso induttivo**). Poiché stiamo assumendo che ci sarà una prossima esecuzione, non siamo nel caso base della ricorsione, quindi entriamo nel ciclo **while**. Si trova il posto corretto per `k`, e poiché `x` non è una foglia, carica il giusto figlio. Poiché `x` non è pieno per ipotesi, se il nodo caricato fosse pieno potrebbe essere eseguito lo split, rendendolo non pieno. Questo è il nodo su cui poi verrà richiamato `BTreeInsertNonFull`, e quindi l'inviariante è ancora vera. Nel caso in cui `x` fosse una foglia, l'invariante dice che non è piena e che k va inserito esattamente in `x`.
La **complessità** in numero di operazioni su disco e' chiaramente $\Theta(h)$; invece, quella in termini di operazioni CPU e' $\Theta(h \cdot t) = \Theta(t \cdot log_t(n))$.

--- 
# Grafi: visita in ampiezza e problemi collegati

## Grafi: introduzione
Un **grafo** e' una tripla `G = (v, E, W)` composta da un insieme di `V` di **vertici**, un insieme $E \subseteq V * V$  di **archi**, e una funzione $W : E \rightarrow \mathbb{R}$ che assegna un peso ad ogni arco. Il grafo `G` e' **indiretto** se vale sia $(u, v) \in E \iff (v, u) \in E$ che $W(u, v) = W(v, u)$, e **diretto** altrimenti.
Quando da un vertice `u` possiamo raggiungere un vertice `v` usiamo il simbolo $u ⇝ v$. 
Il **grado entrante** di un **nodo** di un grafo e' i numero di archi che lo raggiungono, ed il **grado uscente** il numero di archi che lo lasciano. Il grafo `G` e' **pesato** se `W` non e' costante, e **non pesato** altrimenti (spesso viene usata la notazione `G = (V, E)`).

Chiamiamo **sparso** un grafo tale che $|E| << |V|^2$, e lo definiamo **denso** altrimenti.
Ci sono due standard per rappresentare un grafo:
- con **liste di adiacenza**;
- con una **matrice di adiacenza**.
In entrambi i casi, e' conveniente pensare che ogni vertice $v \in V$ sia identificabile con un numero naturale da 1 a $|V|$.

Nel caso della rappresentazione a liste di adiacenza, usiamo un array `Adj[1, ..., |V|]` dove ogni elemento punta ad una lista. Per ogni vertice `v`, la lista puntata da `Adj[v]` contiene la chiave `u` se e solo se $(v, u) \in E$ e, nel caso piu' generale, il nodo della lisa contiene anche il peso dell'arco. (per i grafi sparsi si preferisce questa rappresentazione)
![[Grafi_rappr_liste.png]]

Nel caso della matrice di adiacenza, usiamo una matrice `W` che contiene sia l'informazione sul peso di ogni arco sia quella della sua esistenza. Chiaramente e' una matrice quadrata di lato $|V|$, e si preferisce questo metodo per i grafi densi.
Invece di `W[i, j]` si usa la scrittura $W_{ij}$.
![[Grafi_rappr_matrici.png]]

I grafi (e gli algoritmi su di essi) sono pensati per essere **statici**.
Ma come ci comportiamo per un grafo dinamico? Esistono almeno due soluzioni per la rappresentazione di questo caso:
- La prima prevede nodi come oggetti che includono puntatori ai nodi (che rappresentano gli archi).
	- Soluzione complessa quando il grafo e' pesato e quando e' ignoto il grado uscente massimo
- La seconda prevede che sia i nodi, sia gli archi siano oggetti, entrambi inseriti in liste doppiamente collegate.

Un grafo (per noi) e' una struttura dati **statica**, **non basata sull'ordinamento** e rappresentata in maniera **sparsa o compatta** a seconda del caso.

Per quanto riguarda il trattamento degli algoritmi sui grafi useremo la classica notazione `u.att` per indicare un attributo `att` associato con un vertice `v`. 
Per quanto riguarda la complessita' degli algoritmi sui grafi, dobbiamo tenere conto di entrambe le componenti `E` e `V`. Un algoritmo **lineare** nel caso peggiore avra' complessita' $\Theta(|V| + |E|)$ o $O(|V| + |E|)$. 

>`att` $\rightarrow$ nome generico di attributo

Le applicazioni dei grafi sono innnumerevoli:
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

Dato un grafo `G = (V, E)` diretto o indiretto, non pesato, ed un vertice particolare chiamato **sorgente** $s \in V$, vogliamo sapere quanti archi sono necessari a raggiungere qualunque altro vertice (raggiungibile) da `s`. Utilizziamo una visita di `G` chiamata **in ampiezza**.
`BreadthFirstSearch` esplora sistematicamente gli archi di `G` e cerca di **scoprire** nuovi vertici raggiungibili da quelli gia' conosciuti. Computa la distanza minima in termini di numero di archi che esiste tra `s` ed ogni vertice scoperto e produce un **albero di visita in ampiezza** che contiene tutti i vertici raggiungibili. 
Non ha tanto senso utilizzare questa visita per i grafi pesati, anche se funziona. Viene naturale applicare la visita in ampiezza a grafi indiretti.

> visita in ampiezza $\rightarrow$ preferibilmente applicabile a ==grafi indiretti e non pesati==

Utilizzeremo i colori bianco, grigio e nero per colorare i vertici mentre sono scoperti.
Tutti sono bianchi all'inizio: quando un nuovo vertice e' scoperto diventa grigio e quando tutti i vertici adiacente ad esso sono stati scoperti, diventa nero (ed il suo ruolo termina). Solo `s` e' colorato di grigio all'inizio e durante la scoperta `s` diventa la radice dell'albero di visita in ampiezza.
Alla scoperta di un nuovo vertice `v` a partire da un vertice gia' scoperto `u`, l'arco `(u, v)` diventa parte dell'albero e `u` viene marcato come **predecessore** di `v` nell'albero stesso.
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

Tutti i nodi ad eccezione di `s` sono inizializzati di colore bianco, la loro distanza a infinito e il loro padre nell'albero a visita a nullo. I vertici vengono inseriti nella coda e gli adiacenti ai vertici vengono scopeti nell'ordine di inserimento. L'elemento `v.pi`, per ogni `v` e' chiamato **puntatore** (al **padre** di `v` nell'albero di visita in ampiezza). Non e' un puntatore classico, ma contiene il nome del padre di `v` nell'albero di visita.
## Correttezza e complessità di `BreadthFirstSearch`
Definiamo la **distanza piu' corta** di `v` dalla sorgente `s` ($\delta(s,v)$) come il **numero minimo di archi che sono necessari per raggiungere** `v` da `s`.
Le sue proprieta' sono:
- che sia zero tra un vertice e se stesso $\rightarrow$ $\delta(s,s) = 0$;
- che sia infinito da `s` a `v` quando il secondo e' irraggiungibile dal primo $\rightarrow \delta(s,v) = \infty$;
- che sia una distanza $\rightarrow$ disuguaglianza triangolare (per ogni coppia di vertici `v, u` tali che esiste un arco `(u, v)`, succede che $\delta(s,v) \le \delta(s,u)+1$).

**Correttezza** e **terminazione** di `BreadthFirstSearch`. Mostriamo che dopo l'esecuzione, per ogni `v` raggiungibile da `s`, $\text{v.d} = \delta(s,v)$ e se `v != s`, almeno uno dei percorsi piu' brevi da `s` a `v` si ottiene da uno dei percorsi piu' brevi da `s` a `v.pi` con l'arco `(v.pi, v)`.
Osserviamo che in ogni momento gli elementi di `Q` sono ordinati per stima, cioe' se `u` e' prima di `v` in `Q` allora `u.d <= v.d`, e che se $\delta(\text{s, u}) \le \delta(\text{s, v})$, `u` entra in `Q` prima di `v`.
Dimostriamo la correttezza semplicemente trasformandola in **invariante**: appena un vertice `v` entra in `Q`, si ha che $\text{v.d} = \delta(s, v)$; ragioniamo per induzione sul momento in cui `v` entra nella coda.

- **Caso base**: `v = s`. Chiaramente all'entrata in `Q` $\text{s.d} = 0 = \delta(s, s)$.
- **Caso induttivo**. Consideriamo `v` scoperto durante l'esplorazione degli archi di `u`. Supponiamo, per assurdo, che `v` sia il primo vertice che entra nella coda con $\text{v.d} \ne \delta(s, v)$. Siccome `v` e' entrato, questo e' accaduto perche' il vertice `u` e' appena uscito. Per ipotesi induttiva, $\text{u.d} = \delta(s, u)$ e viene assegnato `v.d = u.d + 1`, quindi $\text{u.d} = \delta(s, u)$. Se, nell'ipotesi assurda, fosse che $\text{v.d} \le \delta(s, v)$ si avrebbe $\delta(s, v) > \delta(s, u)+1$, che contraddice la disuguaglianza triangolare. Allora l'unica possibilita' e' che $\text{v.d} > \delta(s, v)$, cioe' che esista `w` tale che $\delta(s, w) < \delta(s, u)$ e $(w,v) \in E$. Ma siccome `v` e' stato scoperto da `u`, `w` non puo' mai avere preceduto `u` in `Q`. Ma questo contraddice il fatto che $\delta(s,w) < \delta(s,u)$. Quindi $\text{v.d} = \delta(s, v)$.

La **complessità** della visita e' semplice da calcolare: e' facile osservare che un nodo entra nell coda al massimo una volta. Per ogni nodo che e' entrato nella coda si analizzano i suoi adiacenti ma il totale degli archi e' comunque `|E|`. 
Questa analisi e' detta **aggregata**, ed e' usata spesso nei grafi.
Con questa analisi, se il grafo e' connesso, allora la complessita' e' $\Theta(|V| + |E|)$ in tutti i casi. Se il grafo e' sconnesso, la complessita' e' $O(|V| + |E|)$. La funzione `|V| + |E|` **diventa** $|V|^2$ quando il grado e' denso. Quindi nel caso peggiore (grafo connesso e denso) e' $\Theta(|V|^2)$, ma normalmente si accetta $\Theta(|V| + |E|)$.

---
# Grafi: visita in profondità e problemi collegati

## Grafi: visita in profondità
Consideriamo un grafo `G`. E' indifferente se `G` e' o no pesato, o se `G` e' o no diretto (come nelle visite in ampiezza). Ma se $G = (V, E)$, e' diretto. 
Ci proponiamo di risolvere tre problemi:
- stabilire se `G` e' **ciclico** $\rightarrow$ stabilire se contiene almeno un ciclo;
- costruire un **ordinamento topologico** di `G` $\rightarrow$ elencare tutti i suoi vertici in un ordine qualsiasi tale che ogni vertice `v` e' elencato solo se tutti i vertici **dai quali** `v` si puo' raggiungere sono stati elencati prima;
- conoscere ed enumerare tutte le **componenti fortemente connesse** di `G` $\rightarrow$ elencare tutti i sottoinsiemi massimali di `V` tale che, ogni vertice in ogni sottoinsieme raggiunge ogni altro vertice di quel sottoinsieme.
La soluzione in comune per questi tre problemi e' la visita in profondita'.

Il proposito della visita in profondita' e' quello di scoprire tutti i vertici raggiungibili da ogni potenziale sorgente `s`. La differenza tra le due visite e; che i vertici nella visita in profondita' vengono scoperti il **prima possibile** a partire da quelli gia' scoperti: la visita in profondita' e ' ricorsiva. Anche `DepthFirstSearch` usa un sistema di colorazione per ricordare i vertici ancora da scoprire e quelli gia' scoperti. `DepthFirsSearch` assume che `G` sia rappresentato con liste di adiacenza e riempie un campo `v.pi` per generare un **albero di visa in profondita'** come risultato della visita; la differenza qui e' che invece di un solo albero, di produce una **foresta** di alberi, uno per ogni sorgente.
`DepthFirstSearch` riempie anche dei campi `v.d` (**momento** della scoperta) e `v.f` (momento nel quale il vertice viene abbandonato). I campi `v.d` e `v.f` sono interi tra $1$ e $2 \cdot |V|$. Vengono generalmente chiamati **tempi**. Per ogni vertice `u`, abbiamo `u.d < u.f`.

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

Ogni volta che `DepthVisit` viene chiamata, si inizia un nuovo albero della foresta degli alberi di visita in profondita'. Se `G` e' tale che tutti i vertici sono raggiungibili dal primo vertice visitato, allora ci sara' un solo albero, altrimenti ce ne saranno di piu'.
Ogni vertice `u` sul quale si chiama `DepthVisit` e' inizialmente bianco, il suo tempo di scoperta viene marcato come tempo di inizio (`u.d`) e viene colorato di grigio.
Ogni vertice nella lista di adiacenza di `u` viene esplorato ricorsivamente; alla fine di questa esplorazione `u` viene marcato come nero ed il suo tempo finale `u.f` viene registrato.

### Complessità di `DepthFirstSearch`
L'analisi della **complessità** viene calcolata attraverso la tecnica dell'analisi aggregata.
`BreadthFirstSearc` esegue un ciclo di che costa $|V|$ per la colorazione iniziale e per ogni vertice chiama `DepthVisit`. Ma il massimo numero di chiamate a `DepthVisit` e' $|E|$. Quindi la complessità e' $\Theta(|V| + |E|)$, che nel caso peggiore diventa $\Theta(|V|^2)$ (anche se accettiamo $\Theta(|V| + |E|)$).

## Grafi diretti: cicli
In un grafo diretto, chiamiamo **ciclo** un percorso $v_1, v_2, ..., v_k$ di vertici tali che per ogni `i` esiste l'arco $(v_i, v_{i+1})$ e che $v_1 = v_k$. Quando un grafo diretto e' privo di cicli, lo chiamiamo `DAG` (Directed Acyclic Graph). 
Definiamo il seguente problema: dato un grafo diretto `G` stabilire se presenta o no un ciclo. L'algoritmo `CycleDet` che usiamo prima di eseguire`DephtFirstSearch` modificata in maniera da interrompersi se si visita un nodo grigio: al momento di visitare un nodo grigio, siamo certi di aver trovato un ciclo.

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
**Correttezza**: l'algoritmo e' corretto se e solo se restituisce `True` quando `G` e' ciclico e `False` altrimenti.
La **complessità** del nostro algoritmo e' la stessa della visita in profondita', e la **terminazione** e' ovvia.

## Grafi: ordinamento topologico di grafi diretti
Uno degli usi piu' interessanti dei grafi diretti consiste nel rappresentare un insieme di vincoli rispetto a un insieme finito di compiti (o **task**). Se tutti i task son inseriti in un grafo diretto `G` dove un arco `(u, v)` rappresenta che `u` deve essere posto prima di `v`, una delle possibili soluzioni al problema di stabilire un ordine lineare (possibile) tra i task e' dato dall'**ordinamento topologico**. Il problema dell'ordinamento topologico non ha senso se il grafo diretto e' ciclico.

Il problema dell'ordinamento topologico prende in input un grafo connesso `G` senza cicli e restituisce una lista collegata $v_1, ..., v_{|V|}$ di vertici topologicamente ordinati; per ogni coppia $v_i, v_j$ di vertici, $v_i$ appare prima nella lista di $v_j$ se e solo se $v_i$ precede topologicamente $v_j$.

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
Dato un grafo diretto `G`, una **componente fortemente connessa** (SCC) e' un sottoinsieme massimale $V' \subseteq V$ tale che, per ogni $u, v \in V'$, succede che $u ⇝ v$ e che $v ⇝ u$. 
Osserviamo il concetto corrispondente nel caso di grafi indiretti e' quello di **componente connessa**; due termini per indicare un'idea simile in due contesti leggermente diversi.

Un'elemento fondamentale nello studio delle SCC e' il grafo trasposto di `G`. Dato `G` diretto, il **grafo trasposto** $G^T$ di `G` e' ottenuto invertendo la direzione di ogni arco. La complessita' e' $\Theta(|V| + |E|)$ quando `G` e' rappresentato con liste di adiacenza. La proprieta' piu' interessante di $G^T$ e' che `G` e $G^T$ hanno le stesse SCC.

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
Osserviamo che da un grafo `G` possiamo ricavare il suo **grado delle componenti connesse** $G^{SCC}$ semplicemente considerando tutti i vertici di ogni SCC di `G` come un unico vertice di $G^{SCC}$ e impostando che esiste un arco (`u, v`) in $G^{SCC}$ se e solo se esiste un arco in `G` da uno dei nodi simboleggiati da `u` a uno dei nodi simboleggiato da `v`. Se $G^{SCC}$ e' un `DAG` possiamo calcolare il suo ordinamento topologico.

Per un gruppo di vertici `C` (che potrebbe essere una SCC) chiamiamo $f(C)$ il massimo tempo `u.f` tra tutti gli $u \in C$.  Osserviamo che la prima esecuzione di `DepthFirstSearch` ci da informazione sulle SCC di G. In poche parole, la componente `C'` viene scoperta tutta prima della fine della scoperta di `C`. Questo significa che una esecuzione di `DepthFirstSearch` sul grafo $G^T$ ha esattamente la proprieta' contraria.

Ragioniamo adesso per induzione sull'indice `k` che indica il k-esimo albero di visita in profondità generato dalla **seconda** visita `DepthFirstSearch`, quella effettuata su $G^T$ . Vogliamo mostrare che tutti questi alberi sono, in effetti, SCC di `G` (e di $G^T$ ) (e questa è l'**invariante** che stiamo cercando).
- Quando `k = 0` (**caso base**) il risultato e' triviale
- Per il **caso induttivo** supponiamo che i primi `k` alberi restituiti coincidano ancora con `k` SCC di `G`, e dimostriamo che questo e' ancora vero per il `(k+1)-esimo` albero. Supponiamo che `u` sia la radice del `(k+1)-esimo` albero. Questo vertice e' tale che `u.f = f(C)` ed e' maggiore di `f(C')` per ogni SCC `C'` ancora da visitare. Tutti i vertici in `C` sono bianchi a questo punto e tutti diventano discendenti di `u`. Quindi, l'albero risultante contiene tutti gli elementi di `C`

Rimane da mostrare che nessun `v` che non appartenga a `C` appare come discendente di `u`.
Infatti, se `v` è discendente di u, deve succedere in $G^T$ che $u ⇝ v$, e che `v` era bianco al momento di considerare `u`. Ma questo accade solo se $u ̸⇝ v$ in `G`. Quindi, nella prima visita di `G`, `v` è ancora bianco quando `u.f` viene deciso, e quindi, quando v viene visitato (e abbandonato) il suo `v.f` sarà maggiore di `u.f` . Pertanto, `v` è già apparso in qualche albero di visita (radicato in qualche `u′`) e quando viene visto durante la visita a partire da `u` è già nero, e non inserito nel rispettivo albero di visita. Allora, come volevamo dimostrare, anche l'albero `k` corrisponde ad una SCC di `G`.

--- 
# Grafi e alberi di copertura minima

## Alberi di copertura minimi
Ci concentriamo sui grafi pesati indiretti connessi. Sia quindi $G = (V, E, W)$. Un grafo indiretto pesato puo' essere una buona rappresentazione di situazioni reali, come ad esempio una rete di connessioni informatiche tra computer, dove il peso di ogni arco rappresenta il costo della connessione. In una situazione come quella descritta possiamo domandarci qual'e' il costo di visitare ogni vertice, ed in particolare se c'e' una scelta di archi ottima, che minimizza il costo.

Definiamo un **alberi di copertura minimo** (o **MST**) come un sottoinsieme di archi che forma un albero, copre tutti i vertici e la cui somma dei pesi e' minima.

> MST $\rightarrow$ **minumum spanning tree** 

La strategia generale per risolvere questo problema e' di tipo **greedy**. Questo significa che ad ogni passo faremo la scelta **localmente** migliore e vogliamo in questo modo ottenere il risultato **globalmente** migliore. 

Osserviamo che un MST `A` e' un insieme di archi tali che formano un albero (indiretto) che tocca tutti i nodi del grafo. Quindi, eliminando qualunque arco da un albero di copertura si ottengono due alberi connessi.
Qualunque partizione di `V` in due sottoinsiemi `S` e `V \ S` viene chiamato **taglio del grafo** (semplicemente, **taglio**). Costruire un MST `T` significa considerare un taglio ed aggiungere progressivamente un arco, partendo dal taglio piu' semplice che comprende un solo nodo di `S`.

![[taglioVS.png]]

Consideriamo un taglio qualsiasi (`S, V\S`) tale che per una certa coppia di nodi $a, \space b, \space a \in S \space \text{e} \space b \notin S$. Poiche' vogliamo costruire un MST, dovremo scegliere un arco che connetta qualche nodo di `S` con qualche nodo di `V\S`: se non lo facessimo non potremo mai ottenere un MST.
Immaginiamo adesso che tra tutti gli archi con questa proprieta', l'arco (`a, b`) sia quello di peso minimo. Vogliamo mostrare che **l'arco** (`a, b`) **deve essere scelto**. 

Immaginiamo di non scegliere (`a, b`) nella costruzione di un MST. Per la proprieta' detta precedentemente, a fine operazioni ci sara' certamente un arco (`u, v`) che connette qualche nodo di `S` con qualche nodo di `V\S`:

![[taglioVS2.png]]

Sia `T` l' MST ottenuto scegliendo (`u, v`) invece di (`a, b`). Possiamo costruire un nuovo albero `T'` cosi': $T' = (T \backslash \{(u, v)\}) \cup \{(a, b)\}$. Chiaramente, se `T` era un albero d copertura, lo e' anche `T'`; inoltre `T'` pesa meno di `T`. Pertanto `T'` **non poteva essere un** MST.
Abbiamo dimostrato la seguente proprieta':
- considerata qualunque situazione di un MST, il prossimo passo per la costruzione e' segliere sempre l'arco di peso minimo che lo attraversa.
Chiamiamo questo arco **sicuro**.

La proprieta' vista prima e' un algoritmo di costruzione di un MST, che non rimane che codificare. Ci permette di fare le seguenti considerazioni. Se tutti i pesi di archi di `G` sono diversi tra loro, allora l'MST e' unico: per ogni taglio ci sarebbe sempre una sola scelta. 
L'algoritmo di Prim e' un modo efficiente per vedere quanto visto.

### Alberi di copertura minimi: algoritmi di Prim
L'idea di base dell'algoritmo di Prim e' quella di partire da un vertice qualsiasi e, ad ogni passo, aggiungere un arco in modo che l'arco aggiunto sia sicuro. La corretta struttura dati in questo caso deve permettere di mantenere un insieme di vertici in maniera da poter facilmente individuare ed estrarre, tra questi, quello che comporta una spesa minima in termini di peso dell'arco che viene scelto per raggiungere quel vertice.

Ogni vertice `G` viene arricchito con due campi:
- `v.key` $\rightarrow$ il peso minimo, inizialmente $\infty$, tra gli archi che connettono qualche vertice `T` con `v` 
- `v.pi` $\rightarrow$ il padre di `v`, inizialmente `Nil`, nel MST risultante.
Inizialmente tutti i vertici si trovano nella coda di priorita' `Q` semi-ordinata su `v.key`, dove inizialmente tutti gli elementi sono $\infty$. La radice `r` e' data esplicitamente e si cerca un MST radicato in `r`. Ad ogni scelta, i pesi degli elementi in `Q` vengono modificati in base al principio che abbiamo spiegato, il vertice estratto da `Q` e inserito in `T` e ripetiamo finche' `Q` si svuota.

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
Per mostrare che `MST-Prim` e' **corretto**, definiamo `T` come l'insieme di tutte le coppie (`v.pi, v`) tali che `v.pi` e' definito e $v \notin Q$.
Mostriamo che vale la seguente **invariante**: `T` e' sempre sottoinsieme di qualche MST.
- Nel **caso base**, `T` e' vuoto e l'invariante e' rispettata in maniera triviale.
- Supponiamo che l'invariante valga per `T` ad un certo punto della computazione (**caso induttivo**) e consideriamo l'insieme `T'` ottenuto dopo una esecuzione del ciclo **while**. Succede che $T' = T \cup \{(v.\pi, v)\}$, e `v` e' il vertice tale che `v.key` e' il minore tra quelli ancora nella coda `Q`. Vogliamo mostrare che `T'` è ancora sottoinsieme di qualche MST. Sia `S` l'insieme di tutti e soli i vertici coperti da archi di `T`. Chiaramente, (`S, V \ S`) è un taglio di `G`, e chiaramente, l'arco (`v.π, v`) è un arco sicuro per il taglio. Pertanto, `T'` è ancora sottoinsieme di qualche MST.

#### Complessità di `MST-Prim`
In alternativa possiamo usare una **heap binaria** per implementare la coda, il che ci permette di avere un vantaggio in caso di grafi sparsi. Avremo che la costruzione della coda costa sempre $\Theta(|V|)$, l'estrazione del minimo costa $\Theta(log(|V|))$ e il decremento costa $\Theta(log(|V|))$.
Con un grafo sparso avremo:
- $\Theta(|V|)$ $\rightarrow$ inizializzazione
- $\Theta(|V|)$ $\rightarrow$ costruzione della coda
- $\Theta(V \cdot log(|V|))$ $\rightarrow$ estrazione del minimo
- $\Theta(E \cdot log(|V|))$ $\rightarrow$ decrementi
- **totale** $\rightarrow$ $\Theta(E \cdot log(|V|))$

Il caso peggiore si verifica con un grafo denso e per minimizzare il danno si preferisce l'implementazione con una coda senza struttura. I grafi si considerano non densi anche quando `|E|` cresce rispetto a `|V|` ma non si avvicina asintoticamente a $|V|^2$; anche in questo caso, la seconda scelta e' migliore della prima.

### Alberi di copertura minimi: algoritmo di Kruskal
Una valida alternativa a `MST-Prim` è l'algoritmo noto come algoritmo di Kruskal, che utilizza una generalizzazione del concetto di taglio e del concetto di arco sicuro per il taglio al ne di ottenere un albero di copertura minimo.

L'idea di `MST-Kruskal` e' che possiamo ordinare gli archi in ordine crescente di peso e, analizzandoli uno a uno in questo ordine, stabilire se inserirlo come parte dell'albero di copertura minimo oppure no.
Quale sarebbe la ragione di non farlo a un certo punto dell'esecuzione? Semplicemente, un arco 
(`u,v`) **non** e' parte di nessun MST se `u` e `v` sono gia' connessi da qualche altro arco precedentemente scelto.
Sia `T` l'insieme di archi che abbiamo scelto fino ad un certo punto. `T` a differenza del caso di 
`MST-Prim` non e' necessariamente un albero ad ogni momento, ma lo e' direttamente alla fine della computazione. Dati gli archi di `T` e dato l'insieme `V` di vertici, diciamo che un sottoinsieme `S` di `V` e' T-**connesso** se, considerando solo archi in `T`, e' un albero ed e' massimale.

Dato `T` ad un certo punto della computazione, identifichiamo tutte le componenti `T-connesse` di `V`: $S_1, S_2, ..., S_n$. La tupla $(S_1, S_2, ..., S_n)$ e' certamente una partizione di `V` e generalizza il concetto di taglio visto prima. Lo chiamiamo **taglio generalizzato**. Possiamo affermare che **un arco di peso minimo tra quelli non ancora considerati** che attraversa un taglio generalizzato e' un arco sicuro.

Come facciamo a garantire che un arco scelto attraversi il taglio? La definizione e' semplice:
- un arco (`u, v`) attraversa un taglio generalizzato se `u` e `v` appartengono a diverse componenti `T-connesse`.
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
Per mostrare che `MST-Kruskal` e' **corretto**, osserviamo che tutti gli insiemi $S_1, S_2, ..., S_{|V|}$ che vengono creati da `MakeSet` nella prima operazione costituiscono un taglio generalizzato, considerando che `T` e' vuoto.
Dimostriamo la seguente **invariante**: ogni arco che viene aggiunto nel ciclo non e' sicuro.
- Poiché gli archi vengono ordinati in maniera crescente, l'invariante è vera all'inizio (**caso base**): il primo arco scelto è un arco di peso minimo, e certamente attraversa il taglio.
- Supponiamo adesso che l'invariante sia vero fino ad una certa esecuzione (**caso induttivo**). Sia $(S_1, . . . , S_n$), con $n ≤ |V|$, il taglio generalizzato corrente, e sia (`u, v`) l'arco considerato. Per definizione di `FindSet`, u e v appartengono a due diverse componenti T-connesse, quindi attraversa il taglio. Ogni arco (`u', v'`) di peso minore di (`u, v`) è gia' stato considerato prima di ('u, v'), e attraversava il taglio quando era stato considerato (dunque, dopo Union, non lo attraversa piu') oppure non lo attraversava. Quindi (`u', v'`) non attraversa il taglio, e (`u, v`) è un arco di peso minimo che attraversa il taglio.

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

---

# Grafi e percorsi minimi con sorgente singola

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
Se G ha pesi negativi, ma non ci sono **cicli negativi** raggiungibili da $s$, allora i percorsi minimi da $s$ sono ancora ben definiti.
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

---
# Grafi e percorsi minimi tra tutte le copie di vertici

## Percorsi minimi tra tutte le copie di vertici
Dato un grafo $G = (V, E, W)$ pesato, diretto e connesso, vogliamo calcolare il peso del **percorso minimo per ogni coppia di vertici**.
Osserviamo in primo luogo che se aggiungiamo l'ipotesi di non avere archi negativi, possiamo risolvere questo problema applicando $|V|$ volte l'algoritmo di `Dijkstra`.
- In caso di grafi densi, questo porterebbe ad un algoritmo $\Theta(|V|^3)$
- In caso di grafi sparsi, $\Theta(|E| \cdot |V| \cdot log(|V|))$ 
In caso di archi negativi siamo costretti ad usare `Bellman-Ford`, per una complessita' di $\Theta(|V|^4)$.
Ci domandiamo se possiamo fare meglio di cosi'.

Il primo problema che dobbiamo affrontare e' quello della rappresentazione. Fino ad ora abbiamo usato la rappresentazione a **liste di adiacenza**. Ma il problema dei percorsi minimi tra tutte le coppie e sostanzialmente diverso da quello con sorgente singola, e in un certo modo richiede in maniera naturale una rappresentazione **matriciale** del grafo.
Faremo la seguente assunzione: un grafo $G$ e' rappresentato dalla sola matrice $W$ di pesi.

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
La **complessità** e' $\Theta(|V|^3) e la sua terminazione e' ovvia.

--- 
# Teoria della complessità: accenni ed esempi

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























[[#INDICE| GO TO INDEX]] 