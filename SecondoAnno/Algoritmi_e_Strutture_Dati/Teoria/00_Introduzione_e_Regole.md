## Algoritmi e problemi
Le prime tracce degli **algoritmi** risalgono ai Babilonesi, nel VI secolo A.C.
La parola algoritmo deriva dal nome di un matematico persiano del nono secolo, Abu Abd Allah Muhammad ibn Musa al-Khwarizmi. 

Un **algoritmo**, per noi, e' una qualsiasi procedura computazionale che da un **input** produce un **output**. E' necessario un algoritmo per **comunicare** la soluzione a un problema in una maniera formale, verificabile, riproducibile ed implementabile. Inoltre, la relazione tra problema e algoritmo non e' uno a uno! Ci possono essere problemi risolvibili da tanti algoritmi diversi, e problemi per i quali non esiste un algoritmo che lo risolve.

Noi diciamo che un **problema** e' cio' che dobbiamo risolvere, e chiameremo **problema risolvibile** un problema per il quale esiste un algoritmo (sotto certe condizioni) che lo risolve. Un **istanza** e' un particolare input da un problema e una **soluzione** e' l'output che corrisponde ad un particolare input.

## Scrivere algoritmi
In **linguistica** si distinguono tre aspetti di un linguaggio: 
- La **sintassi** $\rightarrow$ come strutturo una frase;
- La **semantica** $\rightarrow$ cosa significa una frase;
- La **pragmatica** $\rightarrow$ lo studio di qual'e' il miglior modo di esprimere un concetto.

Gli algoritmi non sono programmi. I programmi **descrivono** gli algoritmi, nascondendo, di fatto, l'intuizione che soggiace all'algoritmo stesso.
**Tutti i linguaggi di programmazione sufficientemente espressivi sono ugualmente espressivi.** Questo significa che possiamo scegliere un linguaggio o un altro e non perdere nulla in termini di capacita' di risolvere il problema.

Facciamo le seguenti ipotesi. Le istruzioni semplici si eseguono in tempo costante (una unità di tempo): operazioni algebriche sui numeri interi e non, assegnamenti, controlli di condizioni logiche, movimenti semplici in memoria. Tutto il resto non è costante, in particolare i cicli e le chiamate ricorsive o a funzioni da noi precedentemente definite o assunte. I numeri sono rappresentati in base binaria, per cui il numero n occupa c · log(n) bit di memoria per qualche costante c - la parola di memoria è di lunghezza costante. La dimensione dell'input si misura, in generale, come il numero di bit che l'input occupa. Il tempo di computazione è il numero di passi semplici che si impiegano espresso in termini della dimensione dell'input, e tiene conto di costanti che nascondono i dettagli implementativi. Infine, gli array cominciano dalla posizione 1 e non 0,
se non esplicitamente detto il contrario.

## Caratteristiche degli algoritmi
L'esistenza di un algoritmo per un problema ci da un tetto alla complessita' del problema stesso. Per esempio, un algoritmo di ordinamento potrebbe avere complessita' di tempo $n \cdot log(n)$: cio' significa che se una particolare istanza e' lunga *n*, ci si mette circa $n \cdot log(n$) passi **elementari** per risolverlo