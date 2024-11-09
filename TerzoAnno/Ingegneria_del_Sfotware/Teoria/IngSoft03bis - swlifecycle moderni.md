## Sviluppo Agile: perche'?
- Problemi complessi richiedono contributo di molte persone
- Soluzione inizialmente non chiara
- Requisiti che molto probabilmente cambierebbero
- Posso pensare di sviluppare il programma in "incrementi"
- E' necessaria una collaborazione stretta e un feedback rapido con gli stakeholders

## Metodologie di sviluppo Agile
I principi su cui si basa una metodologia leggera che segue i punti indicati dall'Agile Manifesto sono quattro:
- le persone e le interazioni sono piu' importanti dei processi e degli strumenti
- e' piu' importante avere software funzionante che documentazione
- bisogna collaborare con i clienti al di la del contratto
- bisogna essere pronti a rispondere ai cambiamenti piu' che aderire al progetto

Tutti i metodi agili hanno in comune:
- rilasci frequenti del prodotto sviluppato
- collaborazione continua del team di sviluppo con il cliente
- documentazione di sviluppo ridotta
- continua e sistematica valutazione dei valori e dei rischi dei cambiamenti

Metodi agili: Extreme Programming (XP), Scrum, Feature-Driven Development (FDD), Adaptive Software Process, Crystal Light Methodologies, Dynamic System Development Method (DSDM), Lean Development, ...

### Minimal Viable Product (MVP)
- Prodotto Minimo Funzionante $\rightarrow$ e' il prodotto con il piu' alto ritorno sigli investimenti rispetto al rischio
- E' una strategia che mira a evitare di costruire prodotti che i clienti non vogliono (cerco di massimizzare le informazioni apprese dal cliente per ogni euro speso)
- MVP quindi non e' un prodotto minimo, ma un processo iterativo di idee, prototipazione, presentazione, raccolta dati, analisi e apprendimento
- MVP e' la base del movimento "Lean", che e' alla base di Agile
![[MVP.png]]

### Extreme Programming (XP): move light and move fast
- Approccio recente (1999)
- Il codice e' il prodotto
- Il codice e' la documentazione
- Codifica con un amico
- Scrivi prima i casi prova e prosegui con la codifica finche' questi non sono stati superati
- Punta sempre al progetto piu' semplice per superare i test
- Inizia con il minimo numero di funzioni desiderate e migliora il risultato mano a mano che procede il lavoro

#### Team di sviluppo XP
- Di solito meno di 10 persone, riunite nello stesso locale
- Un rappresentante del cliente e' sempre presente per rispondere a domande sui requisiti
- Il team usa comportamenti di sviluppo semplici e allo stesso modo capaci di informare tutti sullo stato del progetto
- il team e' pronto ad adattare i comportamenti alla situazione specifica

#### XP core practice
- XP e' definito dalle pratiche usate
- Le pratiche variano nel tempo e a seconda del progetto in cui vengono utilizzate
	1. Planning the game
	2. Simple design
	3. Pair programming
	4. Testing
	5. Refactor
	6. Short releases
	7. Coding standard

##### XP: planning the game
- Sviluppo dell'applicazione accompagnato dalla stesura di un piano di lavoro
- Piano definito e aggiornato a intervalli brevi e regolari dai responsabili del progetto, secondo le priorita' aziendali e le stime dei programmatori
- I programmatori partecipano attivamente alla pianificazione
- La pianificazione coinvolge sia utenti responsabili del progetto sia sviluppatori per stabilire un equilibrio dinamico tra le esigenze di tutti
- Gli utenti finali presentano gli obiettivi descrivendo scenari (storie)
- Gli sviluppatori stimano il tempo necessario a realizzare ogni storia
- Le storie vengono ordinate da utenti e responsabili secondo la loro priorita' di realizzazione, dopo che ne e' stata stimata la rispettiva difficolta'
- Dalla sintesi delle valutazioni dei responsabili del progetto pianificato l'attivita': l'insieme delle storie da realizzare per il prossimo rilascio e le date previste

##### XP: simple design and pair programming
- La struttura dell'applicazione deve essere la piu' semplice possibile
- L'architettura del sistema deve essere comprensibile da tutte le persone coinvolte nel progetto
- Non devono esserci parti superflue o duplicazioni
- Le parti che compongono il sistema devono essere solo quelle strettamente necessarie alle esigenze concorrenti
- Solo se richiesto da nuove circostanze si progettano nuovi componenti o si riprogettano quelli gia' esistenti
- La scrittura del codice e' fatta da coppie di programmatori che lavorano al medesimo terminale
- Le coppie non sono fisse ma sono formate associando le migliori competenze per risolvere il problema specifico
- Il lavoro in coppia permette, scambiandosi periodicamente i ruoli, di mantenere mediamente un alto livello di attenzione
- Il locale di lavoro deve permettere senza difficolta' di lavorare a coppie

##### XP: testing
- Ogni funzionalita' va sottoposta a verifica, in modo che si possa acquisire una ragionevole certezza della sua correttezza
- Test di sistema costruiti sulla base delle storie concordate con il committente
- Test di unita' che devono poter essere rieseguiti automaticamente, con tempi dell'ordine dei minuti
- Ogni ristrutturazione o modifica del codice deve mantenere inalterato il risultato dei test gia' considerati
- I test vengono, solitamente, scritti prima della codifica della funzionalita'

##### XP: refactor
Refactoring: migliorare il codice esistente senza cambiarne le funzionalita'
- specie dopo molti cambiamenti nel tempo il codice diventa poco maneggevole
- i programmatori spesso continuano a utilizzare codice non piu' mantenibile perche' continua a funzionare
- quando stiamo rimuovendo ridondanza, eliminando funzionalita' non utilizzate e rinnoviamo un design obsoleto, stiamo rifattorizzando
- il refactoring mantiene il design semplice, evita' inutili complessita', mantiene il codice pulito e coinciso in modo che sia facilmente comprensibile, modificabile e estensibile

#### XP: problemi tipici
- Il codice non viene testato completamente
- Il team produce pochi test utili
- Il cliente non partecipa a testare il sistema
- I test non funzionano prima dell'integrazione
- I test sono troppo lenti
- Le storie sono troppo complicate
- Il manager vuole la documentazione di sistema, o la specifica dei requisiti
- Il team e' sovraccarico di compiti
- Cowboy coders $\rightarrow$ alcuni membri scelgono di ignorare la metodologia del team

### SCRUM
- Scrum $\rightarrow$ nel gioco del rugby e' la mischia
- Scrum $\rightarrow$ nello sviluppo sw e' un processo in cui un insieme di persone si muove all'unisino per raggiungere un obiettivo predeterminato.
- E' un processo:
	- per gestire e controllare lo sviluppo del sw
	- iterativo, incrementale, si puo' applicare allo sviluppo e gestione di ogni tipologia di prodotto
	- fornisce alla fine di ogni iterazione un set di funzionalita' potenzialmente rilasciabili

Scrum $\rightarrow$ avanzare come squadra.

#### SCRUM: fasi
- **Pre-game pase**
	- Planning sub-phase $\rightarrow$ include la definizione del sistema che deve essere sviluppato. Viene creata una Product Backlog List, che contiene i requisiti attualmente conosciuti
	- Architecture sub-phase $\rightarrow$ viene pianificato un design di alto livello per il sistema, inclusa l'architettura, in base agli elementi contenuti nel Product Backlog
- **Development phase**
	- si sviluppa il sistema attraverso una serie di "sprint"
		- cicli iterativi nei quali vengono sviluppate o migliorate una serie di funzionalita'
		- ogni sprint include le tradizionali fasi di sviluppo del sw
		- l'architettura del sistema evolve durante lo sviluppo negli sprint
		- uno sprint tipicamente dura da una settimana a un mese
- **Post-game phase**
	- chiusura definitiva della release

#### SCRUM: i ruoli
- **Product Owner**
	- La persona a cui fanno riferimento tutti i soggetti interessati al progetto, incluso il cliente finale $\rightarrow$ rappresenta gli stakeholders
	- Scrive il product backlog in forma di user stories, e' responsabile della definizione di "Fatto"
	- figura di raccordo $\rightarrow$ effettua stime, aggiusta i processi che rappresentano problemi, gestisce l'intero procedimento secondo la pianificazione inizialmente fatta
	- Poteri:
		- accettare o rifiutare i risultati del lavoro
		- terminare uno sprint se necessario
- **Membro del team**
	- Responsabilita':
		- costruire il prodotto
		- decidono cosa fare in ciascuno sprint
	- Caratteristiche:
		- cross-functional
		- team organizzati indipendentemente
		- senza project o team manager
		- ognuno fa una cosa alla volta (no multitasking), di solito full-time
	- Team:
		- idealmente 7($\pm2$) persone
		- se e' possibile nella stessa sede/ufficio
		- stabile durante lo sprint
		- Il team si autogestisce:
			- auto-organizzato $\rightarrow$ nessuno puo' dire al team come trasformare il product backlog in incrementi di funzionalita' potenzialmente rilasciabili, nemmeno Scrum Master
			- cross-funzionale $\rightarrow$ include tutte le abilita' necessarie a creare un incremento di prodotto alla fine dello sprint
			- alla pari $\rightarrow$ i membri sono tutti sviluppatori allo stesso livello
			- unitario $\rightarrow$ non ci sono sottogruppi, senza eccezioni per attivita' o domini particolari
			- l'intero team e' responsabile dello sviluppo
- **Scrum Master**:
	- Si occupa di supportare il team (ne fa parte), garantendo le condizioni ambientali e motivazionali necessarie ad eseguire al meglio il lavoro commissionato
	- Non ha autorita' sul team $\rightarrow$ serve il team, non lo dirige
	- Soprassiede ai rituali dello scrum (ne e' responsabile)
		- non "cosa" bisogna fare ma il come lo stiamo facendo
		- tiene aggiornato lo stato del progresso di lavoro, in modo che tutto il team sappia come sta andando il progetto
	- Meglio se non partecipa come programmatore

#### SCRUM: i rituali 
- **Spirit Planning**
	- Pianificazione iniziale di gruppo $\rightarrow$ circa 1 ora per ogni settimana di sprint
	- Team decide cosa entra a far parte dello Sprint Backlog $\rightarrow$ sceglie le user stories insieme al PO
	- Team e PO concordano la "Definition of Done" per ogni elemento dello Sprint Backlog
	- il Team fa una stima di quanto lavoro riuscira' a fare nello sprint basandosi sulla "velocita'" di sviluppo
- **Daily Scrum**
	- Max 15 min, in pedi, ogni membro del team dice cosa ha fatto ieri, cosa pianifica oggi, che impedimenti ha trovato
- **Sprint Review** 
	- Max 4h, riguarda il prodotto $\rightarrow$ cosa e' stato o non e' stato completato in questo sprint
- **Sprint Retrospective** 
	- Max 3h, riguarda il processo $\rightarrow$ cosa e' andato bene e quali impedimenti si sono incontrati

#### SCRUM: fail fast technique
- Scrum vuole che tu fallisca $\rightarrow$ e' conosciuto per lo slogan "fail fast"
- Sembra una presa in giro, ma c'e' un'ottima ragione e nello slogan il punto chiave e' "fast" ...
- I progetti falliscono
- Scrum vuole che le magagne saltino fuori in fretta, perche' prima vengono trovate, prima impari e i costi del progetto sono minori

#### SCRUM: Quali sono i patti
- **Il Team promette agli Stakeholders**
	- Il PO del Team difendera' gli interessi degli stakeholders
	- Il tempo degli stakeholders sara' usato saggiamente, saranno proposte solo questioni utili allo sviluppo
	- Il lavoro sara' della massima qualita' compatibile, coi vincoli imposti
	- Alla fine di ogni sprint saranno consegnate nuove funzionalita' che potranno essere valide dagli stakeholders
- **Gli Stakeholders promettono al Team**:
	- Gli stakeholders saranno disponibili per aiutare il team quando necessario
	- Lo Scrum Master sara' coadiuvato a rimuovere gli ostacoli
	- I vincoli e le proprieta' non cambieranno durante un sprint senza il consenso del Team
	- Partecipare a uno sviluppo Scrum non danneggera' la carriera dei membri del Team

#### SCRUM: problemi tipici
- Ignoranza dei valori agili e di SCRUM
- prodotto non testato alla fine dello sprint
- Backlog non pronto all'inizio dello sprint
- Mancanza di/Cattivi facilitazione
- Mancanza di supporto da parte dei manager
- Mancanza di supporto da parte degli stakeholders
- Gestione caotica di "scrum di scrum"