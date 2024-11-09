## VCS
- Version Control System $\rightarrow$ sistema di controllo di versione
- Un VCS e' una combinazione di tecnologie e procedure utilizzate per tenere traccia nel tempo delle modifiche fatte ad un file o ad un insieme di file 
- I VCS vengono utilizzati da un singolo sviluppatore per tenere traccia delle modifiche apportate al proprio software e per recuperare una vecchia versione di un determinato file
- Per un singolo sviluppatore, utilizzare un sistema di controllo di versione e' utile per organizzare lo sviluppo di un progetto in evoluzione

## git
- E un moderno sistema di controllo di versione distribuito, Open Source e quindi gratuito, progettato per gestire dati in modo veloce ed efficiente.

### Dizionario
- **Working Directory**
	- si intende le directory in cui sono presenti tutti i file su cui stiamo lavorando. E' quella directory che di solito contiene i file del nostro progetto
- **Repository**
	- e' l'insieme di tutte le versioni dei nostri file. E gestito dal sistema di versionamento ed e' solitamente "invisibile" all'utente
- **Registrazione (commit)**
	- si intende la creazione di una nuova versione di uno o piu' file all'interno del repository

### Area di Stage
E' importante tenere sempre la Working directory ed il Repository sono due entita' separate e possono anche non coincidere.

Es. si sta eseguendo una ricerca per raccogliere il valore delle aree di diversi rettangoli.
- `iArea.c` (file sorgente del mio codice)
- `iArea.bin` (file sorgente compilato)
- `Relazione.txt` (file di testo che contiene i risultati)

**git** prevede una terza area (detta "di Stage") che raggruppa tutti i file pronti per essere registrati nel repository

- La **Working directory** rappresenta la directory di lavoro, quella in cui sono presenti TUTTI i file su cui stiamo lavorando.
- Lo **Stage** è l’insieme dei file tracciati e pronti per essere registrati nel repository.
- Il **Repository** è l’insieme di tutte le versioni dei files che vengono tracciati con il VCS.

### Add
Per includere un nuovo file all'area di Stag, uso il comando:
``` bash
git add <nome_file>
```

### Commit
Per effettuare una nuova registrazione dei file presenti all'area di Stage, uso il comando:
``` sh
git commit
```
Allegando un messaggio che descrive i cambiamenti

Ad ogni nuovo commit il VCS memorizza solamente i cambiamenti di ogni file rispetto alla loro ultima versione registrata ed associata ad ogni commit un identificativo univoco rendendo cosi' possibile identificare ogni singola registrazione.

Attraverso il comando:
``` sh
git log
```
Possiamo verificare l'elenco degli ultimi commit eseguiti e le informazioni sintetiche di ognuno, compreso l'identificatore

### Checkout
E' possibile recuperare una seguente versione di un singolo file oppure della intera Working directory attraverso il comando:
``` sh
git checkout <commit_id> [<nome_file>]
```

### Commit
- **Quando eseguire una registrazione?**
	- L'ideale sarebbe mantenere i commit concentrati su "piccoli" step di avanzamento del progetto
- **Cosa scrivere nel messaggio?**
	- Per mantenere i messaggi di commit piu' descrittivi possibile dell'evoluzione, e' meglio scrivere il perche' delle modifiche invece del cosa e' stato modificato

### VCS
I sistemi di controllo di versione sono diventati ormai strumenti indispensabile soprattutto per organizzare il codice prodotto da **gruppi di sviluppatori** che lavorano insieme ad uno stesso progetto.

In questo caso, l'utilizzo di un sistema di controllo di versione diventa indispensabile per:
- tenere traccia di chi, nel gruppo di sviluppo, ha effettuato una modifica su un file
- unire le modifiche di ogni membro del gruppo di sviluppo
Per questi scopo, non basta piu' avere solamente il sistema di controllo di versione installato sul proprio computer, serve anche un "punto" centralizzato raggiungibile da tutti i membri del gruppo, che collezioni le modifiche di ogni sviluppatore e le renda disponibile a tutti gli altri.

### Come funziona un VCS
Ogni membro del gruppo ha una copia dell'area di lavoro sulla propria macchina chiamata **repository locale**.
L'area di lavoro centralizzata viene chiamata **repository remoto**.

### Bitbucket
Bitbucket e' un servizio online che offre soluzioni per la gestione di repository **git** remoti.

Se abbiamo gia' una repository locale e vogliamo collegarlo ad un repository remoto, possiamo farlo con il comando:
``` sh
git remote add origin <url_repository>
```

Se abbiamo creato il repository remoto, possiamo crearne una copia locale attraverso il comando:
``` sh
git clone <url_repository>
```

Una volta che il nostro repository locale e' collegato al repository remoto, attraverso i comandi
``` sh
git push
git pull
```

