## Algebra Relazionale
- L'insieme delle operazioni per il modello relazionale e' l'**algebra relazionale**. Le operazioni dell'algebra consentono all'utente di specificare le interrogazioni fondamentali.
- Il risultato di un interrogazione e' una **nuova relazione**, che puo' essere stata formata a partire da una o piu' relazioni. Le operazioni dell'algebra producono nuove relazioni che possono essere ulteriormente manipolate usando le operazioni della stessa algebra.
- Una sequenza di operazioni dell'algebra relazionale forma un'**espressione dell'algebra relazionale**, il cui risultato sara' ancora una relazione che rappresenta il risultato di un'interrogazione del database.

## Operazioni Relazionali Unarie 

### Operazione di selezione (SELECT)
L'operazione di selezione e' usata per selezionare un *sottoinsieme* di tuple di una relazione che soddisfano una condizione di selezione. E' una sorta di filtro che trattiene solo quelle tuple che soddisfano una *condizione qualificante*. Puo' essere vista come una *partizione orizzontale* della relazione in due insiemi di tuple: quelle che soddisfano la condizione e vengono selezionate e quelle che non la soddisfano e vengono scartate.

L'operazione di selezione e' indicata con: $$ \sigma_{\text{<condizione di selezione>}}(R) $$
Dove il simbolo $\sigma$ (sigma) e' usato per denotare l'operatore di selezione e la condizione di selezione e' un'*espressione booleana* specificata sugli attributi della relazione $R$.

#### Proprieta' dell'operazione SELECT
- L'operazione SELECT $\sigma_{\text{<condizione di selezione>}}(R)$ produce una relazione $S$ che ha lo **stesso** schema della relazione $R$.
- L'operazione SELECT e' **commutativa**: $$ \sigma_{\text{<cond1>}}(\sigma_{\text{<cond2>}}(R)) = \sigma_{\text{<cond2>}}(\sigma_{\text{<cond1>}}(R)) $$
- Operazioni di SELECT in **cascata** possono quindi essere eseguite in **qualunque ordine**: $$ \sigma_{\text{<cond1>}}(\sigma_{\text{<cond2>}}(\sigma_{\text{<cond3>}}(R))) = \sigma_{\text{<cond2>}}(\sigma_{\text{<cond3>}}(\sigma_{\text{<cond1>}}(R))) $$
- Operazioni di SELECT in cascata possono essere sostituite da una singola operazione di SELECT avente come condizione la **congiunzione** di tute le condizioni precedenti: $$ \sigma_{\text{<cond1>}}(\sigma_{\text{<cond2>}}(\sigma_{\text{<cond3>}}(R))) = \sigma_{\text{<cond1> AND <cond2> AND <cond3>}}(R) $$

### Operazione di proiezione (PROJECT)
L'operazione di proiezione seleziona alcuni **attributi** (colonne) da una relazione (tabella) e scarta gli altri. L'operazione PROJECT puo' essere vista come una *partizione verticale* della relazione in due relazioni: una con gli attributi richiesti, contenente il risultato dell'operazione e l'altra con quelli non richiesti.

L'operazione di proiezione e' indicata con: $$ \pi_{\text{<elenco attributi>}}(R) $$
dove $\pi$ (pi-greco) e' il simbolo utilizzato per denotare l'operazione e `<elenco attributi>`  e' l'elenco degli attributi *desiderati* tra gli attributi della relazione R.
L'operazione di proiezione **rimuove** eventuali **duplicati** delle tuple, in modo che il risultato dell'operazione sia un'insieme di tuple, e quindi una relazione **valida**.

#### Proprieta' dell'operazione PROJECT
- Il numero di tuple nel risultato di un'operazione di proiezione $\pi_{\text{<elenco attributi>}}(R)$ e' sempre **minore o uguale** al numero di tuple in $R$.
- Se l'elenco degli attributi include una **chiave** di $R$, allora il numero di tuple nel risultato della proiezione e' **uguale** al numero di tuple in $R$.
- Vale la seguente uguaglianza: $$ \pi_{\text{<attributi1>}}(\pi_{\text{<attributi2>}}(R)) = \pi_{\text{<attributi1>}}(R) $$
  se `<attributi2>` contiene gli attributi elencati in `<attributi1>`.

### Operazione di ridenominazione (RENAME)
E' probabile che si vogliano eseguire piu' operazioni di algebra relazionale una di seguito all'altra. E' possibile scrivere le operazioni desiderate come una singola espressione dell'algebra relazionale nidificando le operazioni, oppure applicare un'operazione alla volta e cercare relazioni contenenti i risultati intermedi. In quest'ultimo caso occorre dare un nome alle relazioni intermedie.

L'operazione di ridenominazione e' $\rho$ (rho).

In generale un'operazione RENAME puo' essere espressa in una delle seguenti forme:
- $\rho_{S(B1, B2, ..., Bn)}(R)$ 
	  e' una relazione ridenominata $S$, basata su $R(A1, A2, An)$, con gli attributi ridenominati in $B1, B2, ..., Bn$.
- $\rho_S(R)$
  e' una relazione ridenominata $S$, basata su $R$, che non specifica i nomi degli attributi.
- $\rho_{(B1, B2, ..., Bn)}(R)$
  e' una relazione con attributi ridenominati $B1, B2, ..., Bn$ che non specifica il nuovo nome della relazione.

## Operazioni "insiemistiche"

### Operazione di unione (UNION)
Il risultato di questa operazione, indicata con **R** $\cup$ **S**, e' una relazione che include tutte le tuple presenti in $R$, oppure in $S$, oppure in entrambe le relazioni. Eventuali duplicati vengono eliminati.
I due operandi devono essere **compatibili all'unione**, cioe' devono avere lo stesso **tipo di tuple**.

#### Compatibilita' all'unione (di tipo)
- Le relazioni operande $R1(A1, A2, ..., An)$ e $R2(B1, B2, ..., Bn)$ devono avere lo stesso numero di attributi, ed il dominio degli attributi corrispondenti deve essere corrispondente; si deve cioe' avere che $dom(A_i) = dom(B_i)$ per ogni $i = 1, 2, ..., n$.
- La relazione risultante dalle operazioni $\text{R1} \cup \text{R2}$, $\text{R1} \cap \text{R2}$, o $\text{R1} - \text{R2}$ avra' gli stessi nomi di attributo della prima relazione operanda `R1` (per convenzione).

### Operazione di intersezione (INTERSECRTION)
Il risultato di questa operazione, indicata con **R** $\cap$ **S**, e' una relazione che include tutte le tuple che sono presenti sia nella relazione `R`, sia nella relazione `S`. I due operandi devono essere *compatibili all'unione*.

### Operazione di differenza (MINUS)
Il risultato di questa operazione, indicata con **R - S**, e' una relazione che include tutte le tuple che sono presenti in `R` ma non in `S`. I due operandi devono essere *compatibili all'unione*

### Proprieta' della operazioni UNION, INTERSECTION E MINUS
- Sia l'operazione di unione, sia quella di intersezione sono operazioni commutative; quindi: $$ \text{R} \cup \text{S} = \text{S} \cup \text{R} \text{, e R} \cap \text{S} = \text{S} \cap \text{R} $$
- Sia le operazioni di unione, sia quelle di intersezione possono essere trattate come operazioni n-arie, applicabili a qualunque numero di relazioni. Sono entrambe operazioni associative; quindi $$ \text{R} \cup (\text{S} \cup \text{T}) = (\text{R} \cup \text{S}) \cup \text{T, e } (\text{R} \cap \text{S}) \cap \text{T} = \text{R} \cap (\text{S} \cap \text{T}) $$
- L'operazione differenza **non** e' commutativa; in generale quindi: $$ \text{R} - \text{S} \ne \text{S} - \text{R}$$

### Operazione di prodotto cartesiano (prodotto incrociato)
Questa operazione viene utilizzata per unire tuple di due relazioni in modo combinatorio. In generale, il risultato dell'operazione `R(A1, A2, ..., An) x S(B1, B2, ..., Bm)` e' una relazione `Q` di grado $n+m$ attributi `Q(A1, A2, ..., An, B1, B2, ..., Bm)`, nell'ordine indicato.
La relazione risultante `Q` ha una tupla per ogni possibile combinazione tra una tupla di `R` e una di `S`.
Quindi, se `R` ha $n_R$ tuple (indicate come $|R| = n_R$), e `S` ha $n_S$ tuple, allora $$ |R \times S| \quad \text{avra' } \space n_R \cdot n_S \quad \text{tuple} $$ I due operandi **non** devono essere compatibili all'unione.

## Operazioni Relazionali Binarie

### Operazione di JOIN
La sequenza di un'operazione di prodotto cartesiano seguita da una di selezione, e' usata molto comunemente per identificare e selezionare tuple correlate da due relazioni. Si tratta di un'operazione speciale, chiamata JOIN, che viene indicata con il simbolo $\bowtie$ .

Questa operazione e' molto importante per tutti i database relazionali con piu' di una relazione perche' permette di eseguire associazioni tra relazioni.

In generale l'operazioine di join tra due relazioni `R(A1, A2, ..., An) x S(B1, B2, ..., Bm)` e' indicata: $$ \bf{R} \bowtie_{\text{<condizione di join>}} \bf{S} $$ dove `R` e `S` possono essere qualunque relazione che risulti da un'espressione dell'algebra relazionale generale.

#### THETA-JOIN: $R \bowtie_{<Ai \space \theta \space Bl \space ... \space Ak \space \theta \space BI>} S$ 
Il risultato di un'operazione di join sulle relazioni `R(A1, ..., An)` e `S(B1, ..., Bm)` e' una relazione `Q` con $n+m$ attributi `Q(A1, ..., An, B1, ..., Bm)`.

Le tuple di `Q` sono formate dalle combinazioni delle tuple di `R` ed `S` per le quali viene soddisfatta la condizione di join.

Una condizione di join assume la forma generale: $$ \text{<condizione> AND <condizione> AND ... AND <condizione>} $$ in cui ogni condizione assume la forma $Ai \space \theta \space Bj$ con $\theta$ uno degli operatori di confronto 
{$=,\space <,\space \le,\space >,\space \ge,\space \ne$} .

Le tuple i cui attributi di join sono NULL **non** compaiono nel risultato. Per questo l'operazione di join **non** conserva necessariamente tutte le informazioni presenti nelle relazioni partecipanti.

#### EQUI-JOIN
L'uso piu' comune delle operazioni di join utilizza condizioni di sola **uguaglianza**. Questo tipo particolare di theta-join, in cui l'unico operatore di comparazione usato e' =, viene chiamata **equi-join**. Nel risultato di un equi-join si avranno sempre una o piu' coppie di attributi con valori identici in ciascuna tupla.

#### JOIN NATURALE
Poiche' uno degli attributi nelle coppie con valori identici e' superfluo, e' stata creata una nuova operazione, chiamata **join naturale** ed indicata con $*$, che non include il secondo attributo ripetuto in una condizione di uguaglianza.

La definizione comune di una join naturale richiede che i due attributi di join, o ciascuna coppia di attributi di join, abbiamo lo **stesso nome** in entrambe le relazioni. Altrimenti viene applicata una ridenominazione.

### Operazione di divisione (DIVISION)
Un'operazione di divisione si applica a due relazioni $\bf{R(z) \div S(x)}$, in cui $X \subseteq Z$ . Sia $Y = Z-X$ (e quindi $Z = X \cup Y$); sia `Y` l'insieme degli attributi di `R` che non sono attributi di `S`.

Il risultato della divisione e' una relazione $T(Y)$ che include una tupla `t` se in `R` sono presenti tuple con $t_R[Y] = t$, e con $t_R[X] = t_s$ per ogni tupla $t_s$ di `S`.

Cio' significa che, affiche' una tupla `t` appaia nel risultato `T` della divisione, in `R` devono comparire i valori di `t` in combinazione con ogni tupla di `S`. 

## Insieme completo di operazioni relazionali
- L'insieme completo delle operazioni dell'algebra relazionale formato dalla sezione $\sigma$, proiezione $\pi$, unione $\cup$, differenza $-$ e prodotto cartesiano $\times$ e' detto insieme completo perche' ogni altra espressione dell'algebra relazionale puo' essere espressa come combinazione di queste cinque operazioni.
  
- Ad esempio: $$ R \cap S = (R \cap S) - ((R - S) \cup (S - R)) $$ $$ R \bowtie_{\text{<condizione di join>}}S = \sigma_{\text{<condizione di join>}}(R \times S) $$
## Altre Operazioni Relazionali

### Funzioni aggregate e raggruppamento
- Un tipo di interrogazione che non puo' essere espressa in termini di algebra relazionale d base consiste nello specificare **funzioni aggregate** matematiche su collezioni di valori del database.
  
- Esempi di queste funzioni prevedono il recupero dello stipendio o complessivo di tutti gli impiegati, oppure il numero totale delle tuple di impiegato. Queste funzioni vengono usate in semplici interrogazioni statistiche che riassumono le informazioni provenienti dalle tuple del database.
  
- Funzioni comuni applicate a collezioni di valori numerici sono SUM, AVERAGE, MAXIMUM e MINIMUM. La funzione COUNT viene utilizzata per contare le tuple o valori.

### Uso dell'operatore funzionale $\mathcal{F}$ 
Operatore di funzione aggregata, indicato con $\mathcal{F}$, nella forma: $$ \text{<attributi raggruppamento>} \quad \mathcal{F} \space \text{<lista funzioni>}(R) $$ dove $\text{<attributi raggruppamento>}$ e' una lista di attributi della relazione `R` e $\text{<lista funzioni>}$ e' una lista di coppie ($\text{<funzione>, <attributo>}$). La relazione risultante presenta gli attributi di raggruppamento piu' un attributo per ogni elemento nella lista di funzioni.

- $\mathcal{F} \space \text{MAX}_{\text{Stipendio}} \space (\text{DIPENDENTE})$ ricava il valore dello stipendio massimo dalla relazione DIPENDENTE
- $\mathcal{F} \space \text{MIN}_{\text{Stipendio}} \space (\text{DIPENDENTE})$ ricava il valore dello stipendio minimo dalla relazione DIPENDENTE
- $\mathcal{F} \space \text{SUM}_{\text{Stipendio}} \space (\text{DIPENDENTE})$ ricava la somma degli stipendi dalle relazioni DIPENDENTE
- $\text{DNO} \space \mathcal{F} \space \text{COUNT}_\text{SSN}, \space \text{AVERAGE}_\text{Stipendio} \space (\text{DIPENDENTE})$ raggruppa i dipendenti per DNO (numero di dipartimento) e calcola il numero di dipendenti e lo stipendio medio per ciascun dipartimento $[$Nota: COUNT conta semplicemente il numero di righe, senza rimozione dei duplicati$]$. 

### Operazioni di Chiusura Ricorsiva
- Un altro tipo di operazione che, in generale, non puo' essere specificato in algebra relazionale di base e' la chiusura ricorsiva. Questa operazione si applica ad un'associazione ricorsiva.
  
- Un esempio di operazione ricorsiva e' la ricerca di tutti i supervisori di un dipendente `e` a tutti i livelli: cioe' di tutti i dipendenti `e`' direttamente sottoposti ad `e`; di tutti i dipendenti `e`" direttamente sottoposti agli `e`'; di tutti i dipendenti `e`" direttamente sottoposti agli `e`", etc...
  
- Anche se e' possibile trovare gli impiegati a ciascun livello e poi utilizzare un'unione, non e' possibile, in generale, specificare un'interrogazione di questo tipo senza utilizzare un ciclo.
  
- Lo standard SQL3 include una sintassi per la chiusura ricorsiva.

### L'operazione di join esterna (OUTER JOIN)
Nelle join naturali, le tuple senza corrispondenze sono eliminate dal risultato dell'operazione. Anche le tuple con valori NULL negli attributi di join sono eliminate. Cio' causa perdita di informazione.

Un insieme di operazioni chiamate **join esterne**, possono essere usate quando si vuole tenere nel risultato di una join tutte le tuple di `R`, oppure tutte le tuple di `S`, oppure quelle di entrambe le relazioni, anche nel caso non si abbiamo corrispondenze negli attributi di join.

- L'operazione di join esterna sinistra (LEFT OUTER JOIN) mantiene tutte le tuple della prima relazione (di sinistra) `R` in $\bf{R} \bowtie_\text{LEFT} \bf{S}$. Se non c'e' una corrispondenza con una tupla di `S`, gli attributi di `S` del risultato di join vengono riempiti con valori NULL.
- Un'operazione analoga, join esterna destra (RIGHT OUTER JOIN), mantiene tutte le tuple della seconda relazione (di destra) `S` nel risultato di $\bf{R} \bowtie_\text{RIGHT} \bf{S}$.
- Una terza operazione, join esterna totale (FULL OUTER JOIN), mantiene tutte le tuple di entrambe le relazioni in $\bf{R} \bowtie_\text{FULL} \bf{S}$.