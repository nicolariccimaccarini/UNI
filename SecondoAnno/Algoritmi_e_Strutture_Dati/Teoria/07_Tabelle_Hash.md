Una **tabella hash** e' una ==struttura dati astratta==. 
Nella sua versione piu' generale, il problema puo' essere descritta cosi': dato un numero di oggetti **relativamente piccolo**, ognuno dei quali e' denotato da una chiave il cui universo e' **relativamente grande**, trovare un modo efficiente di memorizzare in maniera dinamica questi oggetti e implementare le operazioni di inserimento, cancellazione e ricerca.
Nella nostra implementazione, ==le tabelle hash sono dinamiche, parzialmente compatte e non basate su ordinamento==.

Non e' possibile costruire una tabella di assegnamento in maniera efficiente, ed i metodi impliciti generano chiavi molto grandi. La soluzione che prevede l'uso di un array (e si chiama **tabella hash ad accesso diretto**) ha ottime complessita': $\Theta(1)$ per tutte le operazioni. Purtroppo, la grandezza delle chiavi rende inaccettabile la maggioranza dei casi.
La soluzione che prevede l'uso di una lista presenta $\Theta(1)$ per l'inserimento e $\Theta(n)$ per ricerca e cancellazione nei casi medio e pessimo.

## Tabelle hash: notazione
Una tabella hash ad accesso diretto $T$ e' un semplice array di **puntatori** ad oggetti; `T[key]` punta all'oggetto la cui chiave assegnata e' `key`.
Diciamo che gli oggetti sono denotati con $x, y, \ldots$ e che per ognuno di essi il campo `key` e' la sua chiave.
Per una tabella ad accesso diretto:
- `x.key` e' sempre piccolo (se $m$ e' la dimensione della tabella $\rightarrow$ `x.key <= m`)
- `x.key != y.key` per ogni coppia di `x != y`.
Per $m$ molto grande, anche un operazione elementare come **creare** $T$ vuoto prende troppo tempo.

Qual'e' una caratteristica comune a tutte le applicazioni tipiche in questo contesto? Il numero di chiavi effettivamente utilizzate e' molto inferiore alla cardinalita' del dominio, che in questo contesto si chiama **universo**, e si denota con $\mathcal{U}$. 

==Condizioni di utilizzo tabella hash==
- Universo grande.
- Sottoinsieme utilizzato dall'universo relativamente piccolo ma non assolutamente piccolo
- $\rightarrow$ non posso allocare tutto in maniera compatta
- $\rightarrow$ non posso dire che l'accesso lineare e' efficiente

## Tabelle hash con chaining
Se il numero effettivo $n$ di elementi **effettivamente utilizzati** e' molto piu' piccolo della cardinalita' dell'unicerso, possiamo ancora implementare $T$ con un array di posizioni $1, \ldots, m$, ma nasce il problema della memorizzazione della chiave $k$ molto piu' grande di $m$, quindi senza accesso diretto.
Per risolverlo creiamo questa funzione:
$$
h: \mathcal{U} \rightarrow \{1, ..., m\}
$$
detta **funzione hash**, per poter indirizzare l'elemento $k$ alla posizione `h(k)`. $h$ non puo' essere iniettiva. Quando $k_1 \ne k_2$ e si da il caso che $h(k_1) = h(k_2)$, chiameremo questa situazione di **conflitto**.
Abbiamo gia' ottenuto un primo vantaggio:
- $T$ e' molto piccolo (ha esattamente $m$ posizioni) e quindi diventa inutilizzata.
Inoltre, il tempo di accesso e' ancora costante **a meno dell'overload dovuto ad un conflitto**.

>La funzione di hash puo' essere:
 - crittografica
> - non crittografica 

Vorremmo che la funzione hash:
- Resistenza alla per-immagine
- Resistenza ai conflitti
- Uniforme (distribuisca bene)

Il nostro obiettivo e' quello di progettare una funzione di hash che **minimizzi** i conflitti. 
La tecnica chiamata **chaining** risolve tutti i conflitti utilizzando una lista doppiamente collegata. La testa della lista e' memorizzata in `T[h(k)]`, che quando e' vuota contiene **nil**.

> Questa soluzione occupa un po' piu' di $m$ parole di memoria, ma molte meno di cardinalita' dell'universo

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
- Fissiamo $n$ come numero di elementi effettivamente inseriti in $T$ e $m$ come dimensione di $T$. Se tutti gli $m$ elementi finiscono nella stessa cartella, la ricerca e quindi anche la cancellazione sono nel caso peggiore e la loro complessita' diventa $\Theta(n)$. 

Come calcoliamo il caso medio?
- Il caso medio dipende dalla bonta' della funzione $h$. Sotto l'ipotesi di **hashing uniforme e semplice**, $h$ inserisce una chiave $k$ in un determinato slot con la stessa probabilita' con la quale inserisce in qualsiasi altro slot, indipendentemente dalla presenza di chiavi in $T$. Per ogni posizione $j$ tra 1 e $m$, la lunghezza della lista $T(j) (|T(j)|)$ e' tale che $\sum_{j=1}^m |T[j]| = n$, per cui il **valore atteso** di $|T(j)|$ e' $\frac{n}{m}$ (che viene detto **fattore di carico**).  
  Quindi, sotto l'ipotesi di hashing uniforme, il caso medio di `HashSearch()` su una tabella hash a collisioni risolte per chaining ed in caso di ricerca con esito **negativo** ha complessita' $\Theta(1 + \frac{n}{m})$, assumendo che il calcolo $h$ prenda tempo $O(1)$.

Cosa succede quando la ricerca ha esito positivo (la chiave e' **presente** in $T$)?
- Rispetto al caso piu' complesso, il calcolo del tempo medio di esecuzione di `HashSearch()` e' piu' complesso perche' il tempo dipende dal **numero di elementi della lista per la posizione** $h(k)$ che bisogna guardare prima di arrivare a $k$ stesso. 
- Osserviamo che questo numero e' esattamente il numero di elementi tale che:
	- hanno chiave $k'$ dove $h(k) = h(k')$ 
	- sono stati inseriti **dopo** il momento in cui e' stato inserito $k$.
- Le liste sono strutturate in modo che gli elementi nuovi appaiono prima e quindi se cerco un elemento a chiave $k$ nella lista puntata da $h(k)$ tale che $I$ elementi hanno chiave $k'$ con $h(k) = h(k')$ sono stati inseriti dopo $k$, dovro' guardare $I$ elementi prima di avere successo.
- L'elemento cercato con chiave $k$ ha la stessa probabilita' di essere uno degli $n$
  elementi correntemente in $T$. La probabilita' che due chiavi siano indirizzate alla stessa posizione e' $Pr(h(k)) = h(k')$, che sotto l'ipotesi di hashing uniforme semplice e' precisamente $\frac{1}{m}$.
- Il valore atteso per la variabile probabilistica $V$ '**numero di elementi da esaminare per giungere a** $k$' e':
$$
E[V] = \frac{1}{n} \cdot \Big(\sum_{i=1}^n \Big(1+\sum_{j=i+1}^n \frac{1}{m} \Big)\Big).
$$
- Quindi
$$
\begin{align}
E[V] &= 1 + \frac{1}{n \cdot m} \cdot \sum_{i=1}^n(n-i) \qquad &&\text{calcolo algebrico} \\
&= 1 + \frac{1}{n \cdot m} \cdot \Big( \sum_{i=1}^n n - \sum_{i=1}^n i \big) \qquad &&\text{calcolo algebrico} \\
&= 1 + \frac{1}{n \cdot m} \cdot \Big( n^2 - \frac{n \cdot (n+1)}{2} \Big) \qquad &&\text{sommatoria} \\
&= 1 + \frac{n-1}{2 \cdot m} \qquad &&\text{calcolo algebrico} \\
&= \Theta(1 + \frac{n}{m})
\end{align}
$$

Concludendo, il tempo medio per una ricerca e' $\Theta(1 + \frac{n}{m})$ (visto che e' uguale sia per esito positivo che negativo).

## Funzioni di hash per il chaining
Da una funzione hash vorremmo che fosse uniforme semplice e che fosse computazionalmente semplice da calcolare.
La letteratura ci offre varie possibilità che tengono conto delle diverse esigenze anche sul valore di $m$:
- se abbiamo chiavi naturali con $m$ numero primo, lontano dalla potenza di 2, allora usiamo il metodo della **divisione**;
- se invece abbiamo chiavi naturali, con $m = 2^p$ per qualche $p$, allora usiamo il metodo della **moltiplicazione**;
- infine, se abbiamo chiavi che sono stringhe o oggetti complessi, con $m$ numero primo, lontano da una potenza di 2, possiamo usare il metodo dell'**addizione**.

### Funzioni di hash per il chaining: divisione
Nel caso dei numeri naturali, una soluzione molto intuitiva e' usare il resto della divisione per $m$:
$$
h(k) = (k \cdot mod(m)) + 1
$$

Quando una funzione e' una funzione di hash uniforme semplice?
- Quando si verifica che $m$ e' un numero primo e lontano da una potenza di 2. Infatti quando $m$ non e; un numero primo, i suoi divisori danno luogo a liste di chaining particolarmente lunghe. Inoltre, se $m=2^p$ per qualche $p$, allora `k mod(m)` dipende unicamente dagli ultimi $p$ bits della rappresentazione binaria di 
  $k$. Se le chiavi $k$ sono uniformemente distribuite, nessuno di questi due problemi è realmente importante. Ma nella realtà le chiavi raramente sono uniformemente distribuite, e quindi usiamo questi accorgimenti per rimediare. 

### Funzioni di hash per il chaining: moltiplicazione
Se non vogliamo che la scelta di $m$ influenzi le prestazioni, possiamo usare il metodo della moltiplicazione.
Scegliamo una costante $A$ tra 0 e 1 reale e costruiamo:
$$
h(k) = \lfloor m \cdot (k \cdot A - \lfloor k \cdot A) \rfloor ) \rfloor +1
$$

Questo metodo funziona bene per ogni costante $A$, esistendo pero' certe costanti 'note' per cui la letteratura riporta ottimi comportamenti, come ad esempio $A = \frac{\sqrt{5} - 1}{2}$.

### Funzioni di hash per il chaining: stringhe (addizione)
Come ci comportiamo quado le chiavi sono stringhe di un alfabeto $\sum$?
- Una soluzione naturale consiste nell'interpretare ogni stringa come un numero naturale. Questo puo' essere ottenuto utilizzando la codifica ASCII, quindi una stringa diventa un numero in base 127 che viene trasformato in base 10:
  $$
	ab7 = 97 \cdot (127)^2 + 98 \cdot (127) + 55 = 1564513 + 12446 + 55 = 1577014
   $$
   
> Un'altra funzione di hash possibile e' 'mid square':
> - $h(k) = \text{le R cifre centrali del ... } k^2$ 

E' davvero efficiente questo calcolo?
- Trasformare una stringa in un numero intero e' un'operazione che costa $\Theta(d)$ dove $d$ e' il numero di caratteri della stringa.
Infatti, data una stringa:
$$ a_1 a_2 ... a_n $$
e la cardinalita' $B$ di $\sum$, la trasformazione corrisponde a calcolare:
$$a1 · B^{d−1} + a2 · B^{d−2} + . . . + ad · B^0$$
cioe':
$$ ((a_1 \cdot B + a_2) \cdot B + a_3) \cdot B + ... a_d $$

Il problema che emerge nelle applicazioni reali e' quello della dimensione dei numeri ottenuti. Questi devono essere sottoposti poi ad operazioni aritmetiche, come il modulo o il confronto. Queste ultime non possono essere effettuate quando i numeri hanno dimensione troppo grande e hanno costo $\Theta(1)$. 
Ricordiamo che l'operazione e il modulo e' invariante rispetto all'addizione e, se $m$ e' primo, anche rispetto alla moltiplicazione. Allora abbiamo che:
$$
\begin{align}
(a_i \cdot B + a_{i+1}) &=_m (z \cdot B +  a_{i+1}) \qquad &\text{se e solo se} \\
(a_i \cdot B) &=_m (z \cdot B) &\text{se e solo se} \\
a_i &=_m z
\end{align}
$$

Poiche' questa puo' essere generalizzata, ci da un metodo semplice per calcolare 
`h(k) = (k mod(m)) + 1`, con $k$ **molto grande** senza mai dover memorizzare $k$.

#### Funzione modulo, correttezza e complessita'
``` Pseudocodice
proc HashComputeModulo(w, B, m) {
	let d = |w|
	z_0 = 0
	for (i=1 to d) z_i+1 = ((z_i * B) + a_i) mod m
	return z_d + 1 
}
```

`HashComputeModulo()` prende parametri $w = a_1 a_2 ... a_d$, $B$ (dimensione dell'alfabeto) e $m$.
Evidentemente questa funzione **termina** sempre, ha **complessità** $\Theta(d)$, ed e' **corretta** perche' utilizza le proprieta' dell'aritmetica modulare per $m$ numero primo. 

## Tabelle hash: open hashing
Questa tecnica ha le seguenti caratteristiche importanti::
- si eliminano le liste e quindi i chaining
- una tabella hash di $m$ elementi potra' ospitare al massimo $m$ elementi e per ottenere questo risultato si rinuncia ad implementare la funzione di cancellazione.
L'indirizzamento aperto si basa sull'idea di provare piu' di una posizione sulla tabella, finche' se ne trova una libera oppure si ha la certezza che la tabella e' piena.

### Open hashing: operazioni
Data una chiave $k$ da inserire in una tabella ad indirizzamento aperto per ottenere una posizione $h(k)$. Se questa e' libera, la chiave puo' essere inserita. Se invece la posizione e' gia' occupata, proviamo **un'altra posizione**: questa sequenza di tentativi si chiama **sequenza di probing**. Non tutte le sequenze di probing funzionano bene: la condizione e' che **tutte le posizioni della tabella devono essere provate** prima o poi.
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
	- $h'$ e' qualunque funzione di hashing uniforme semplice. Si vede chiaramente che le proprieta' di uniformita' e' rispettata per ogni chiave.
- probing quadratico
	- si scelgono due costanti $c_1$ e $c_2$ e si impone: $$ h(k, i) = ((h'(k) + c_1 \cdot i + c_2 \cdot i^2) \cdot  mod(m)) + 1 $$
	- anche in questo caso la sequenza e' uniforme.

Non studiamo in maniera esplicita e formale la **correttezza, complessità e terminazione** delle due operazioni in questo caso. La letteratura ci dice che tendenzialmente l'indirizzamento si comporta meglio del chaining e che il probing quadratico si comporta meglio di quello lineare. In quanto alla funzione di cancellazione, succede che eliminare un elemento sostituendolo con **nil** puo' rendere scorretta l'operazione di ricerca, dovuto al probing. Pertanto, quando si utilizza l'indirizzamento aperto, l'eliminazione di un elemento tipicamente avviene in forma virtuale (utilizzando una flag).