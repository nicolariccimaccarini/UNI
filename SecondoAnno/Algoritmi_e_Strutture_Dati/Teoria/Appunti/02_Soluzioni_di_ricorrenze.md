Per ogni $T(n)$ **esplicita** (come ad esempio `InsertionSort()`) possiamo trovare il suo ordine di grandezza (`InsertionSort()` nel caso peggiore ha complessita' $Θ(n^2)$).
Esistono vari metodi per trasformare una ricorrenza da **implicita** a **esplicita**. L'esempio che dobbiamo ancora risolvere e' quello di `RecursiveBinarySearch()`, che ha complessita', nel caso peggiore, $T(n) = T(\frac{n}{2})+1$. 
Esistono tre modi per la soluzione di ricorrenze:
- il metodo **dell'albero di ricorsione** (o sviluppo in serie)
- il **Master Theorem**
- il metodo della **sostituzione** (o induzione)

## Soluzione di ricorrenze: sviluppo
Il metodo dello sviluppo si basa sull'idea di **sviluppare** la ricorrenza per cercare di estrarne il comportamento.
Se ad esempio $T(n) = T(\text{frazione di n}) + f(n)$, allora per calcolare *T(frazione di n)* posso ri-applicare la stessa ricorrenza, e ottenere una forma, ancora implicita, ma con un termine in piu'.

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
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &= O(\sum_{i=0}^{log_b(n)-1} a^i · (\frac{n}{b^i})^{log_b(a) - ε)} \qquad &&\text{sostituzione} \\ 
&=O(\sum_{i=0}^{log_b(n)-1} \frac{a^i · n^{log_b(a)-ε}}{b^{i · (log_b(a)-ε)}}) &&\text{moltiplicazione} \\
&= O(n^{log_b(n)-ε} · \sum_{i=0}^{log_b(n)-1} \frac{a^i·b^{i·ε}}{b^{i·log_b(a)}})  &&\text{estrazione} \\
&= O(n^{log_b(n)-ε} · \sum_{i=0}^{log_b(n)-1} (\frac{a·b^ε}{b^log_b(a)})^i) \qquad &&\text{esponente} \\
&= O(n^{log_b(n)-ε} · \sum_{i=0}^{log_b(n)-1} (b^ε)^i) \qquad &&\text{prop. logaritmi} \\
\end{align*}
$$
Ci accorgiamo che la sommatoria e' nota: si tratta di una sommatoria geometrica, che sappiamo sempre valutare.

Quindi:
$$
\begin{align*}
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &= O(n^{log_b(n)-ε} · \frac{b^{ε·log_b(n)}-1}{b^ε-1} ) \qquad &&\text{somma geometrica} \\
&= O(n^{log_b(n)-ε} · \frac{n^ε-1}{b^ε-1}) \qquad &&\text{prop. logaritmi} \\
&= O(n^{log_b(a)}) \qquad &&\text{moltip. e diff. di O()}
\end{align*}
$$

Mettendo tutto nell'espressione dalla quale siamo partiti:
$$ T(n) = O(n^{log_b(a)}) + Θ(n^{log_b(a)}) = Θ(n^{log_b(a)}) $$ 
Cosa succederebbe se $f(n) = Θ(n^{log_b(a)})$? Avremmo che
$$
\begin{align*}
\sum_{i=0}^{log_b(n)-1} a^i · f(\frac{n}{b^i}) &= Θ(\sum_{i=0}^{log_b(n)-1} a^i · (\frac{n}{b^i})^{log_b(a)}) \qquad &&\text{sostituzione} \\
&= Θ( n^{log_b(a)} · \sum_{i=0}^{log_b(n)-1} \frac{a^i}{b^{i·log_b(a)}} ) \qquad &&\text{moltiplicazione} \\
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
&≤ f(n) · \sum_{i=0}^\infty c^i \qquad &&\text{maggiorazione} \\
&= f(n) \cdot \frac{1}{1-c} \qquad &&\text{somma di serie geometrica} 
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
T(n) &\le c \cdot log(\lceil\frac{n}{2}\rceil) + 1 \qquad &&\text{ipotesi induttiva} \\
&\le c \cdot log(\frac{n+1}{2}) + 1 \qquad &&\text{maggiorazione} \\
&\le c \cdot log(n+1) - c \cdot log(2) + 1 \qquad &&\text{proprieta' logaritmi} \\
&\le^? c \cdot log(n) \qquad &&\text{NON FUNZIONA}
\end{align}
$$
Non funziona perche' non c'e' nessuna scelta di *c* che rende vero $c \cdot log(n+1) -c+1 \le c \cdot log(n)$. 

Non esiste una strategia sicura per costruire la giusta ipotesi. Immaginiamo che $T(n) \le c \cdot log(n-1)$; allora si ha:
$$
\begin{align}
T(n) &= T(\lceil\frac{n}{2}\rceil) + 1 \\
T(n) &\le c \cdot log(\lceil\frac{n}{2}\rceil -2) + 1 \qquad &&\text{ipotesi induttiva} \\
&\le c \cdot log(\frac{n+1}{2} + \frac{1}{2} -2) + 1 \qquad &&\text{maggiorazione} \\
&\le c \cdot log(\frac{n-2}{2}) + 1 \qquad &&\text{algebra} \\
&\le c \cdot log(n-2) -c \cdot log(2) + 1 \qquad &&\text{algebra} \\
&\le c \cdot log(n-2) \qquad &\text{maggiorazione}
\end{align}
$$

Come altro esempio, cerchiamo di mostrare che $T(n)=2 \cdot T(\lfloor\frac{n}{2}\rfloor) + n$ ha come soluzione $O(log(n))$. Assumiamo che $T(n) \le c \cdot n \cdot log(n)$ per qualche $c \gt 0$, e si ha:
$$
\begin{align}
T(n) &= 2 \cdot T(\lfloor\frac{n}{2}\rfloor) + 2 \qquad &&\text{ricorrenza} \\
&\le 2 \cdot (c \cdot \lfloor\frac{n}{2}\rfloor \cdot log(\lfloor\frac{n}{2}\rfloor)) + n \qquad &&\text{ipotesi induttiva} \\
&\le c \cdot n \cdot log(\frac{n}{2}) + 2 \qquad &&\text{algebra e maggiorazione} \\
&= c \cdot n \cdot log(n) - c \cdot n \cdot log(2) + n \qquad &&\text{proprieta' di log()} \\
&= c \cdot n \cdot log(n) - c \cdot n + n \qquad &&\text{log(2) = 1} \\
&\le c \cdot n \cdot log(n) \qquad &&\text{maggiorazione, con } c \ge 1
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