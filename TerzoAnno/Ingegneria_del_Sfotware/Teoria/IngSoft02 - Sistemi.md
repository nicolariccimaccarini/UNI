## Ingegneria del sistema
- Un sistema e' un insieme di componenti correlate tra loro
	- software 
	- hardware
	- risorse umane
	- dati (informazioni)
- tutti insieme sono finalizzati con un obiettivo comune

## Un sistema per volare
- Primo approccio $\rightarrow$ decomposizione strutturale
	- Mitologia greca $\rightarrow$ Icaro cerca di volare imitando gli uccelli, creando un sistema i cui elementi corrispondevano alle parti fisiche dei volatili
	- Fallimento...
- Secondo approccio $\rightarrow$ decomposizione funzionale
	- I fratelli Wright capiscono che restare in quota e muoversi ono due funzioni diverse, e che possono essere assegnate a due diversi componenti: ali e motore
	- Successo...

## Ingegneria di sistema
- Trasformare un bisogno operativo in una descrizione di parametri operativi e una configurazione di sistema attraverso un processo iterativo di analisi, sintesi, ottimizzazione, progetto e valutazione
- Integrare parametri tecnici correlati e assicurare la compatibilita' di tuette le interfacce fisiche e funzionali al fine di ottimizzare al meglio il progetto complessivo
- Integrare affidabilita', manutenibilita', supporto logistico, sicurezza, fattibilita', integrita' strutturale, fattori umani con l'obiettivo di ottimizzare il risultato
- Significa
	- progettare 
	- implementare
	- installare
- I sistemi che includono
	- hardware (meccanico, elettronico, ...)
	- software
	- personale

### Complessita' dei sistemi
- Numero di sottoinsiemi
	- ampiezza dell'albero di decomposizione del sistema
	- profondita' dell'albero di decomposizione del sistema
- Eterogeneita' di tipo dei sottoinsiemi
![[ComplessitaSistemi.png]]
Difficolta' teorica nel definire la relazione tra i sottoinsiemi

### Modellazione di un sistema
- Puo' tornare utile per valutare in anticipo le caratteristiche quantitative o qualitative
- Come si procede:
	- si definisce una serie di processi che rappresentano entita' della realta' fisica
	- si definisce il comportamento di ciascun processo
	- si definiscono i dati che guidano il sistema
		- dati esogeni (che provengono dall'esterno)
		- dati endogeni (che il sistema si scambia al proprio interno)

### Simulazione di sistemi
- Gli strumenti di modellazione e simulazione di sistemi aiutano a eliminare le sorprese nella costruzione dei sistemi basati su computer
- Questi strumenti sono applicati durante il processo di ingegneria del sistema
	- si puo' fare dal momento in cui specifica il ruolo dell'hardware, del software e delle persone

### Affidabilita' di un sistema
- L'interpretazione delle componenti fa si che gli errori possano propagarsi in tutto il sistema
- I fallimenti possono essere dovuti anche a interrelazioni tra componenti di cui non si e' tenuto conto
- L'affidabilita' complessiva dipende da quella dell'hardware, quella del software e quella degli operatori
- L'affidabilita' e' un parametro sempre piu' importante: la proporzione della componente software in sistemi complessi e' in forte aumento. Il sw fa molto di cio' che si faceva fare all'hw
- Il software spesso viene visto come il problema. Molti sistemi complessi sono considerati un fallimento a causa di problemi col software
- Resilience $\rightarrow$ abilita' del sistema a continuare a operare correttamente in presenza di fallimenti di uno o piu' comportamenti

### Fattori che influenzano l'affidabilita'
- Affidabilita' dell'hw
	- quanto e' probabile un fallimento hw e quanto tempo e' richiesto per ripararlo?
- Affidabilita' del sw
	- quanto e' probabile che una componente sw produca un output sbagliato?
	- si differenzia da quella hw perche' il sw non e' soggetto a usura
- Affidabilita' operatore
	- quanto e' probabile che un utilizzatore commetta un errore?

### Il tutto non e' la somma delle parti
- Le componenti di un sistema possono operare in modo indipendente, ma quando sono integrate in un sistema dipendono dalle altre componenti
- Esempi:
	- sistema di gestione del traffico aereo
	- sistema di allarme
	- sistemi HVAAC (Heating, Ventilation and Air Conditioning)

### Esempi di proprieta' di un sistema
- Peso complessivo del sistema
	- puo' essere calcolato a partire dalle proprieta' delle componenti del sistema
- L'affidabilita' del sistema
	- dipende dall'affidabilita' delle singole componenti e dalle relazioni che intercorrono tra le componenti
- L'usabilita' di un sistema
	- non dipende solo dalle componenti hw e sw, ma anche dall'ambiente e dagli operatori

### Tipi di proprieta' emergenti
- **Proprieta' funzionali**
	- queste appaiono quando le varie componenti vengono assemblate e lavorano per uno scopo comune
- **Proprieta' non-funzionali**
	- queste proprieta' hanno a che fare con il comportamento del sistema nel suo ambiente operativo; possono anche essere vincoli sul sistema

### Affidabilita' di un sistema
- A causa delle interdipendenze all'interno di un sistema gli errori si possono propagare da una componente all'altra
- La causa di un problema puo' anche essere una imprevista interrelazione tra componenti

### Relazione tra l'affidabilita' delle componenti
- Guasti hw possono causare segnali "spuri" nel software che a loro volta causano la produzione di risultati non corretti
- Errori nel software possono causare "stress" nell'operatore, aumentando la sua propensione a commettere errori
- L'ambiente in cui il sistema opera puo' influenzare la sua affidabilita'

### I sistemi e il loro ambiente
- I sistemi non sono indipendenti, ma sono inseriti in un ambiente la cui conoscenza va inclusa nella specifica
- L'obiettivo di un sistema puo' essere di modificare il proprio ambiente
- l'ambiente puo' condizionare il comportamento del sistema
- Sull'ambiente si devono fare delle assunzioni

### Tipi di proprieta': proprieta' in negativo
- Proprieta' quali performance, reliability possono essere misurate
- Tuttavia altre proprieta' sono quelle che il sistema non deve esibire
	- safety $\rightarrow$ il sistema non deve comportare rischi
	- security $\rightarrow$ il sistema non deve consentire l'uso non autorizzato
- Misurare questo tipo di proprieta' puo' essere molto complicato

### Acquisizione di un sistema
- Un sistema puo' essere costruito o acquisito
- Per acquisire un sistema per un'azienda per soddisfare una particolare necessita' e' necessario dare almeno la specifica del sistema e l'architettura del progetto
- Occorre scegliere tra sistemi/sottosistemi quali comprare "off the shelf" e quali sviluppare in modo specifico su contratto
- Fornitore e sotto-fornitori...

![[processoAcquisizione.png]]

#### Problemi del processo di acquisizione
- Un prodotto off-the-shelf potrebbe non adattarsi perfettamente alle richieste
- La specifica dei requisiti puo' far parte del contratto stipulato con il produttore software
- Di solito si prevede un certo intervallo di tempo per modificare i requisiti: trascorso questo intervallo si inizia il processo di sviluppo

### Progettazione di un sistema
- Coinvolge inevitabilmente tecnici e aree diverse, con problemi di "vocabolario" e di metodologia
- Di solito segue un modello di sviluppo a cascata, per poter sviluppare parallelamente le diverse componenti del sistema
- C'e' poco spazio per iterazioni tra le diverse fasi, dati gli altri costi di modifica
- Il sottosistema "software" e' quello piu' flessibile (viene visto come collante)
	- modifiche hardware sono in generale molto costose e complesse: il sw puo' dover compensare i problemi hw

### Definizione dei requisiti
- Quali sono i requisiti globali di un sistema?
- **Requisiti funzionali** $\rightarrow$ cosa il sistema deve fare
- **Requisiti non funzionali**:
	- proprieta' del sistema $\rightarrow$ sicurezza, efficienza, ...
	- vincoli sui sistemi $\rightarrow$ vincoli sull'ambiente, piattaforme da usare, ...

### Disegno del sistema
- Questa fase definisce il modo in cui le funzionalita' del sistema devono essere fornite dalle diverse componenti
	- organizzare i requisiti, separandoli in gruppi correlati
	- identificare i sottosistemi $\rightarrow$ ogni sottosistema di solito soddisfa un gruppo di requisiti
	- assegnare i requisiti ai sottosistemi
	- specificare le funzionalita' dei sottosistemi (di solito incluso fase di specifica requisiti)
	- definire le interfacce dei sottosistemi
		- punto critico per parallelizzazione
![[disegnoSistema.png]]

### Sviluppo dei sottosistemi
- Implementare ciascuno dei sottosistemi individuati nella fase precedente
	- spesso viene fatto in parallelo da parte di team diversi
	- problema di comunicazione tra i team
- Puo' richiedere a sua volta un nuovo processo di sviluppo
- I sottosistemi possono essere componenti off-the-shelf che vengono integrati
	- non e' detto che sia banale 

### Integrazione del sistema
- E' il processo di mettere insieme hw, sw e risorse umane
	- non e' possibile un approccio "big bang"
- Conviene integrare i sottosistemi in modo incrementale
	- i sottosistemi vanno integrati uno alla volta 
	- questo riduce i costi di individuazione degli errori
- In questa fase possono emergere problemi di interfaccia tra le diverse componenti
- Spesso non e' possibile completare lo sviluppo di tutti i sottosistemi allo stesso tempo

### Installazione del sistema 
- Mettere il sistema completo nel suo ambiente operativo
- Possibili problemi
	- l'ambiente finale puo' essere diverso da quello di sviluppo/test
	- resistenza da parte utilizzatori al nuovo sistema
	- coesistenza del nuovo sistema con il precedente
	- problemi "pratici" (cablaggio, corrente, condizionamento)
	- formazione utenti

### Messa in opera
- Mette in luce i requisiti non contemplati
- Gli utenti possono utilizzare il sistema in modo diverso da quello previsto dai progettisti
- Si possono verificare problemi con l'interazione con altri sistemi
- Problemi nell'uso di interfacce diverse che possono indurre in errore gli operatori

### Mantenimento e smantellamento
- Qualunque sistema ha una sua durata (di solito legata alla dimensione)
	- deve evolvere per soddisfare l'evoluzione dei requisiti
- l'evoluzione e' intrinsecamente costosa
	- ogni cambiamento deve essere accuratamente esaminato
	- va verificata l'interdipendenza dei sottosistemi
	- piu' passa il tempo e piu' i cambiamenti effettuati si accumulano
- Obsolescenza dei sistemi
- Puo' richiedere la riconversione dei dati per l'uso con altri sistemi

### Modellare l'architettura di un sistema
- Il modello architetturale mostra in modo astratto la struttura in sottosistemi
- Modelli gerarchici $\rightarrow$ organizzazione ad albero 
- Modelli funzionali $\rightarrow$ rappresentano i flussi di informazione tra i vari sottosistemi
	- di solito mediante un diagramma a blocchi
	- dal modello si dovrebbero identificare i diversi tipi di componenti del sistema
	- vari modi $\rightarrow$ data flow diagram (DFD), behavior diagram, ...