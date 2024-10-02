## Cos'e' il Software?
- Insieme di programmi di computer e la loro documentazione

Due tipi di software:
- **Generic** software $\rightarrow$ sviluppato per essere venduto a un ampio spettro di utenti (la maggioranza del software)
- **Custom** software $\rightarrow$ sviluppato per un utente specifico sulla base dei suoi bisogni (richiede piu' lavoro di sviluppo)

## Caratteristiche del software
- Il software e' sviluppato, non e' costruito (nel senso classico del termine)
- Il software non si "consuma" (pero' puo' invecchiare)
- L'industria del software produce una grossa quantita' di custom

## Il dilemma dello sviluppo software
- I costi per sviluppare software salgono mentre i costi dell'hardware scendono
- Il tempo necessario a sviluppare si allunga e i costi per mantenere il software aumentano
- Gli errori del software aumentano mentre i guasti dovuti all'hardware scendono

## Attributi essenziali di un software di qualita'
Le qualita' su cui si basa la valutazione di un software possono essere:
- **interne**: riguardano caratteristiche legate alle scelte implementative e non sono visibili agli utenti finali
- **esterne**: riguardano funzionalita' fornite dal sistema e sono visibili all'utente finale

## Attributi essenziali per un software di qualita'
- **Maintainability** $\rightarrow$ software deve essere scritto in modo da poter evolvere seguendo i cambiamenti dei requisiti. E' un punto fondamentale: la possibilita' di modificare il software e' un requisito inevitabile in un business in evoluzione
- **Dependability** $\rightarrow$ insieme di caratteristiche, tra cui reliability, security e safety. Dependable software deve non causare danni fisici o economici nell'eventualita' di un system failure. Malicious users non devono poter accedere o danneggiare il sistema.
- **Efficiency** $\rightarrow$ software non deve specificare risorse. Stiamo parlando di responsiveness, processing time, memory utilization, ...
- **Acceptability** $\rightarrow$ software deve essere accettabile dagli utenti per cui e' stato sviluppato: deve essere comprensibile, usabile e compatibile con gli altri sistemi che usano

## Cos'e' l'ingegneria del software?
- Software engineering e' una disciplina ingegneristica che riguarda tutti gli aspetti della produzione di software, dai primissimi stadi delle specifiche di sistema fino alla manutenzione del sistema una volta che e' in uso

## Layer
![[Layer.png]]

- Il layer dei processi e' la base per la gestione di ogni processo software
- Il layer dei metodi descrive come all'interno di un singolo processo si realizzano i singoli passi che lo compongono
- Il layer dei tools descrive strumenti che supportano lo sviluppo delle attivita'

## Ingegneria del software e informatica
- L'informatica e' una scienza: il "cuore" sono i fondamenti teorici (linguaggi, algoritmi, complessita', formalismi, ecc.)
- L'ingegneria del software ha a che fare con aspetti piu' pratici: come pianificare e sviluppare la produzione di software di qualita'
- Ad un ingegnere del software le conoscenza di base dell'informatica servono quanto la fisica ad un ingegnere elettrico

## Ingegneria del software e ingegneria di sistema
- L'ingegneria di sistema ha come oggetto tutti gli aspetti dello sviluppo di un sistema basato su computer, inclusi gli aspetti hardware, software e di processo
- L'ingegneria del software puo' essere vista come una parte dell'ingegneria di sistema
- Gli ingegneri del software collaborano
	- alla specifica del sistema
	- alla progettazione architetturale
	- all'integrazione con le altre componenti

## Il processo di produzione del software
- Il process di produzione software e' un insieme di attivita' il cui fine e' lo sviluppo oppure la modifica di un prodotto software
- Attivita' generiche di tutti i processi di produzione del software:
	- Specifica: cosa deve fare il sistema e quali sono i vincoli per la progettazione
	- Sviluppo: produzione del sistema software
	- Validazione: verifica che il software faccia cio' che il cliente richiede
	- Evoluzione: modificare il software in base alla modifica delle esigenze

## Costi legati alla produzione di software
- All'incirca il 60% dei costi e' legato allo sviluppo, il 40% sono costi per la verifica e validazione (testing)
- I costi variano a seconda del tipo di sistema che deve essere sviluppato e da requisiti quali la performance o l'affidabilita' del sistema
- La distribuzione di costi nelle varie fasi del processo di produzione del software dipende dal modello di processo

### Costo di una modifica
- 1X $\rightarrow$ definizione
- 1,5-6X $\rightarrow$ sviluppo
- 60-100X $\rightarrow$ dopo la consegna 

## Forse ce la facciamo?
3 categorie di progetti software:
- **success** $\rightarrow$ progetto terminato in tempo, secondo i costi e funziona
- **challenged** $\rightarrow$ progetto non completamente funzionante, in ritardo e sforati i costi
- **impaired** $\rightarrow$ progetto cancellato durante lo sviluppo (spesso a causa di costi o tempi)

Distribuzione: success=16,2% challenged=52,7% impaired 31,1%

## Metodi dell'ingegneria del software
Approcci strutturati di sviluppo software che includono:
- Descrizione di modelli
	- descrizione di modelli che devono essere prodotti

- Regole
	- vincoli applicati ai modelli di sistema

- Suggerimenti di design
	- suggerimenti su buone pratiche di design del codice

- Guide al processo di sviluppo
	- quali attivita' seguire

## Quali sono le sfide da affrontare
- **Legacy system**
	- sistemi vecchi ma tuttora molto utilizzati che devono essere mantenuti e aggiornati
- **Eterogeneita'**
	- sistemi distribuiti, che includono una varieta' di componenti hardware e software diversi
- **Tempi di consegna**
	- pressione sempre maggiore per ottenere software di qualita' in tempi sempre piu' rapidi

## Miti da sfatare
- **Management**:
	- "se sforiamo i tempi previsti, basta aggiungere programmatori e ce la faremo!" $\rightarrow$ non basta aggiungere risorse ad un progetto per accelerarne la realizzazione
	- un programmatore per 3 anni e' equivalente a 3 programmatori per un anno
	- "abbiamo standard e procedura da seguire nello sviluppo. Non serve altro" $\rightarrow$ la qualita' di processo e' condizione necessaria ma non sufficiente alla qualita' di prodotto
	- "abbiamo i piu' moderni sistemi di sviluppo e i computer piu' recenti" $\rightarrow$ gli strumenti (hw e sw) sono importanti ma gli sviluppatori lo sono di piu. sviluppare non e' un'attivita' facilmente automatizzabile per cui investire negli strumenti non e' sufficiente 

- **Cliente**:
	- "Se sappiamo a grandi linee cosa deve fare il sistema, possiamo gia' iniziare a programmare" $\rightarrow$ avere chiari gli obiettivi e' un buon inizio, ma l'attivita' di codifica inizia molto piu' tardi
	- "I requisiti variano continuamente, ma i cambiamenti si gestiscono facilmente perche' il software e' flessibile" $\rightarrow$ la modifica delle specifiche e' tanto piu' difficile/costosa quanto piu' tardi avviene nella fase di sviluppo

- **Programmatore**:
	- "Una volta messo in opera il programma, il nostro lavoro e' finito" $\rightarrow$ scrivere programmi e' una parte piccola dell'intero processo. Una buona parte del lavoro avviene dopo la consegna della prima versione al cliente
	- "Fino a quando il programma non gira non c'e' modo di valutarne la qualita'" $\rightarrow$ L'attivita' di revisione durante il processo di progettazione e' una condizione indispensabile per garantire la qualita' del prodotto
	- "L'ingegneria del software ci fara' scrivere una inutile e voluminosa documentazione e ci rallentera'" $\rightarrow$ Lo scopo dell'ingegneria del software e' creare qualita' fin dall'inizio del processo di sviluppo, per avere benefici a lungo e medio termine.

## Responsabilita' professionale
Oltre agli aspetti tecnici ci sono anche risvolti etici, sociali e legati alla responsabilita' professionale
- confidenza
- competenza
- diritti di proprieta' intellettuale
- uso inappropriato dei computer
