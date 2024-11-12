## Le regole d'oro
- 3 regole d'oro per guidarci nella progettazione dell'UI
	1. Lascia che il controllo sia nelle mani dell'utente
	2. Limita la necessita' per l'utente di usare la propria memoria
	3. Usa una interfaccia uniforme per tutta l'applicazione
- Queste regole generali si traducono in un insieme di principi che e' bene rispettare quando si disegna una UI
- Mai dimenticare che "A common mistake people make when trying to design something completely foolproof is to underestimate the ingenuity of complete fools" (D. Adams)

### Controllo nelle mani dell'utente
- Durante la raccolta dei requisiti di un grosso progetto, si chiede ad un utente che interfaccia vuole: "... desidero un sistema che mi legga nel pensiero, sappia quello che voglio fare prima che ne abbia bisogno, deve rendermi facile farlo. Tutto qui"
- La richiesta non e' sbagliata: l'utente vuole qualcosa che reagisca alle sue esigenze, lo aiuti a ottenere il risultato
- L'utente vuole controllare lui il computer, non essere lui sotto il controllo del computer
- Quando poniamo dei vincoli su una user interface, a chi stiamo semplificando la vita: al programmatore o all'utilizzatore?
- Definire la modalita' di interazione in modo da non costringere l'utente ad azioni inutili o indesiderate
- Offrire sempre una interazione flessibile
- Ogni azione deve poter essere interrotta o annullata
- Prevedere modalita' d'uso abbreviate (macro o altro) per utenti esperti se serve svolgere ripetutamente certe azioni
- Nascondere all'utente casuale i dettagli tecnici
- Progetta il sistema in mood che non consenta la manipolazione diretta degli oggetti che compaiono sullo schermo

### Limitare il ricorso alla memoria
- Piu' cose l'utente deve ricordare, maggiori saranno anche le probabilita' di errore nelle interazioni con il sistema
- Bisogna non fare molto affidamento sulla memoria dell'utente
- Ridurre la necessita' di memoria a breve termine
	- se operazione complessa, la UI dovrebbe ridurre la necessita' di ricordare le azioni fatte e i risultati ottenuti finora
- Definire delle impostazioni predefinite di validita' di genere
- Definire scorciatoie intuitive
	- short-cut mnemonico associato all'operazione
- L'aspetto visivo della UI deve essere una metafora del mondo reale
- Fornire le informazioni in modo progressivo
	- le informazioni relative a una operazione, un oggetto, ... devono partire da un alto livello di astrazione e raffinarsi quando l'utente manifesta il suo interesse

### Rendere uniforma la UI
"Le cose che sembrano differenti dovrebbero comportarsi in modo differente; le cose che sembrano uguali dovrebbero comportarsi nello stesso modo"
- Tutte le informazioni visuali vanno organizzate secondo uno standard di progettazione che deve essere mantenuto in tutte le situazioni di visualizzazione
- I meccanismi di input devono essere un insieme limitato e usato uniformemente nell'applicazione
- I meccanismi di navigazione da operazione a operazione devono essere definiti e implementati in modo uniforme
- In ogni istante deve essere evidente il contesto in cui ci si trova
	- in molti casi e' anche utile che l'utente sappia come il contesto da cui proviene e le alternative per seguire una transizione verso una nuova operazione
- Se possibile, mantenere l'uniformita' all'interno di una famiglia di applicazioni
- Se esistono modelli interattivi preesistenti e ben consolidati non apportarvi modifiche se non per motivi molto importanti

## Modelli per l'analisi e il design delle UI
- Di una interfaccia utente bisogna considerare 4 visite
	- il software eng. crea il modello del design
	- l'esperto "ergonomico" crea il modello utente (vista centrata sull'utente)
	- l'utente finale sviluppa un modello mentale (vista centrata sulla percezione)
	- i programmatori creano un modello dell'implementazione (vista centrata sull'implementazione o "immagine del sistema")
- Quasi sempre queste visite differiscono, anche in modo sostanziale e i vincoli che ne derivano sono costanti
- Compito del progettista di interfacce e' arrivare a un compromesso tra le varie esigenze

### Modelli di Utente
- Per fare una UI efficace, bisogna sapere cosa aspettarsi dagli utenti in termini di conoscenza del sistema o dell'ambito
- Tipicamente possiamo catalogare l'utente in 3 categorie
	- **principiante** $\rightarrow$ nessuna conoscenza sintattica e scarse conoscenze semantiche sull'uso del sistema
	- **utente casuale** $\rightarrow$ ragionevole conoscenza semantica dell'applicazione, conoscenza sintattica limitata
	- **utenti costanti** $\rightarrow$ buona conoscenza semantica e sintattica, sindrome dell'utente evoluto

### Immagine e percezione
- La "percezione del sistema" e' l'immagine mentale che l'utente finale si crea nella propria testa
	- dipende molto da tipo di utente
	- dipende molto dalla familiarita' con il dominio di applicazione
- L' "immagine del sistema" comprende sia la manifestazione de sistema, sia tutte le informazioni di supporto che descrivono sintassi e semantica del sistema
- L'utente si trova a proprio agio con il sw e lo usa in modo efficace quando il modello mentale dell'utente e quello dell'implementazione tendono a coincidere
	- il progetto deve tenere conto dell'input proveniente dal modello di utenza

## Processo di progettazione della UI
- Il processo di analisi e progettazione e' iterativo e preferibilmente segue un modello a spirale in cui si distinguono 4 fasi
	1. analisi e modellazione degli utenti, delle operazioni e dell'ambiente
	2. progettazione dell'interfaccia
	3. costruzione dell'interfaccia (implementazione)
	4. validazione dell'interfaccia

![[progettazioneUI.png]]

### Analisi dell'utente e dell'ambiente
- E' sempre meglio comprendere il problema prima di tentare di progettare una soluzione. Per la UI questo significa:
	- Analizzare gli utenti
		- livello di abilita'
		- conoscenza generale del settore
		- disponibilita' ad accettare il sistema
	- Per ogni tipologia di utenza vanno individuati i requisiti dell'interfaccia cercando di capirne la percezione
	- Dell'ambiente in cui verra' usta l'interfaccia va considerato
		- dove deve essere situata fisicamente l'interfaccia
		- la situazione in cui l'utente usera' l'interfaccia
		- vincoli ambientali / richieste ergonomiche

### Analisi e modellazione delle operazioni
- L'obiettivo e' rispondere a queste domande
	- quale lavoro verra' svolto dall'utente e in quali circostanze? quali operazioni principali e secondarie verranno svolte dall'utente mentre svolge il proprio lavoro?
	- quali specifici oggetti del dominio del problema verranno manipolati dall'utente durante lo svolgimento del lavoro?
	- qual'e' la sequenza di operazioni di lavoro (workflow)?
	- qual'e' la gerarchia delle operazioni?
- E' molto semplice a quanto gia' fatto in fase di analisi del sistema, solo applicando la UI

**Use case**
- nell'analisi delle operazioni lo usiamo per mostrare come l'utente svolge alcune operazioni specifiche
- di solito scritto in modo informale
- vanno estratte le operazioni, gli oggetti e il flusso generale delle interazioni

**Elaborazione delle operazioni**
- possiamo applicare la decomposizione funzionale in due modi
	- dato che il sistema di solito sostituisce una attivita' manuale, dobbiamo comprendere le operazioni normalmente svolte e tradurle in operazioni simili da implementare nella UI
	- in alternativa, si studiano le specifiche di una soluzione computerizzata e si determinano un insieme di operazioni che troveranno posto nel modello utente, nel modello di progettazione, nella percezione del sistema

**Elaborazione degli oggetti**
- Invece di pensare alle operazioni, partiamo dallo use case e estraiamo gli oggetti con cui abbiamo a che fare 
- Cataloghiamo in classi questi oggetti
- Definiamo gli attributi di ogni classe e, dalle azioni che facciamo sugli oggetti, definiamo un elenco di operazioni
- Non vogliamo una implementazione letterale di queste operazioni, ma i dettagli dell'operazione

**Analisi del workflow**
- Se la UI e' usata da piu' utenti che hanno ruoli differenti puo' essere necessario applicare l'analisi del workflow
- Il metodo migliore per farlo e' costruire degli activity diagram in cui siano ben chiari i ruoli coinvolti

**Rappresentazione gerarchica**
- Una volta definito il workflow, per ogni ruolo si puo' definire una gerarchia di operazioni
- Di solito la gerarchia deriva da una elaborazione progressiva di ciascuna operazione identificata per l'utente

### Design della UI
- Esistono vari modelli di design, ma tutti hanno una combinazione di questi passi
	- usa le informazioni dell'analisi per definire gli oggetti della UI e le relative azioni (operazioni)
	- definisci gli eventi (azioni degli utenti) che cambiano lo stato della UI; crea un modello di questo comportamento
	- rappresenta ogni stato della UI cosi' come si presentera' all'utente finale
	- indica il modo in cui l'utente interpreta lo stato del sistema sulla base delle informazioni fornite dalla UI

### Definizione degli oggetti e delle azioni
- Dalla descrizione delle operazioni si puo' ricavare lista oggetti e azioni mediante analisi grammaticale
- Gli oggetti si distinguono in "di destinazione", "di origine" e applicativi
	- un oggetto applicativo non puo' essere manipolato direttamente ma solo attraverso azioni indirette
- Una volta individuati gli oggetti e le azioni piu' importanti, va specificata la posizione sull'interfaccia

### Problemi di design
**Tempo di risposta in termini di durata e variabilità**
- la durata non deve essere ne' troppo lunga ne' troppo corta
- conviene una bassa variabilita', anche se il tempo medio di attesa e' lungo

**Sistema di help per l'utente**
- integrato $\rightarrow$ contestuale allo stato del sistema
- esterno $\rightarrow$ manuale consultabile anche online
- I problemi per implementare un help system sono
	- disponibili per tutte le funzioni e in ogni fase dell'interazione?
	- in che modo l'utente puo' richiedere aiuto?
	- che aspetto usare?
	- come uscire dall'aiuto?
	- come strutturare le informazioni di aiuto?

**Messaggi di errore**
- comunicano all'utente situazioni patologiche
- i messaggi dovrebbero descrivere il problema con un linguaggio comprensibile all'utente
- i messaggi dovrebbero fornire informazioni utili a risolvere il problema
- i messaggi dovrebbero indicare eventuali effetti negativi dell'errore
- i messaggi dovrebbero essere accompagnati da effetti audio/video
- i messaggi non dovrebbero colpevolizzare l'utente

**Menu e comandi**
- Oggi l'uso di interfacce a finestre ha ridotto l'uso della riga di comando, ma soprattutto tra utenti esperti si preferisce quest'ultima
	- si dovra' prevedere un comando per ogni azione del menu?
	- che forma dovranno avere i comandi?
	- l'utente puo' personalizzare o abbreviare i comandi?
	- i menu sono autoesplicativi nel contesto della user interface?
	- i menu sono coerenti con la funzione indicata nel titolo?

**Accessibilità**
- settore che sta avendo sempre piu' importanza

**Internazionalizzazione**
- troppo spesso le interfacce sono pensate per una lingua e adattate per gli altri casi
- funzionalita' di localizzazione per personalizzare la UI per un determinato mercato
- esistono varie indicazioni di internazionalizzazione che affrontano problemi generali di progettazione e problemi non banali di implementazione

**Infornazione statica/dinamica**
- Fattori da considerare
	- l'utente e' interessato a una informazione precisa o a una relazione tra dati? l'informazione e' testuale o numerica? i valori relativi sono importanti?
	- quanto velocemente cambiano i dati? il cambiamento deve essere comunicato immediatamente?
	- che tipo di azione deve corrispondere al cambiamento dei dati?

### Presentazione delle informazioni
- Presentazione digitale dei dati
	- rappresentazione compatta; puo' comunicare valori precisi
- Presentazione analogica dei dati
	- con un occhiata fornisce una approssimazione dei valori
	- si possono mostrare i valori relativi ed evidenziare i valori fuori norma

### Strumenti di implementazione
- Esistono strumenti che consentono la creazione di prototipi di interfaccia in modo da consentire in modo semplice l'approccio iterativo allo sviluppo
- I sistemi di sviluppo di interfacce utente utilizzano componenti e oggetti di base per 
	- gestire dispositivi di input
	- convalidare l'input degli utenti
	- gestire gli errori e i messaggi associati
	- fornire messaggi di help
	- gestire finestre o campo di scorrimento interni
	- stabilire le connessioni tra il software applicativo e l'interfaccia
	- consentire la personalizzazione dell'interfaccia

### Valutazione del design
- Quando si e' creato un prototipo operativo della UI, occorre valutarlo per determinare se risponde alle esigenze. Vari metodi:
	- informale $\rightarrow$ un utente usa il sistema e offre il proprio parere
	- formale $\rightarrow$ gruppi di utenti usano il sistema e vengono usati metodi statistici per valutare i questionari di valutazione

![[valutzioneDesign.png]]

- La frase di valutazione puo' essere abbreviata se si effettuano delle valutazioni gia' prima di fare il prototipo di UI
	- la lunghezza delle specifiche necessarie a descrivere i requisiti della UI e' un indice della difficolta' di apprendimento degli utenti
	- il numero di operazioni degli utenti e il numero medio di azioni per ogni operazione e' un indice del tempo di interazione e dell'efficenza globale
	- il numero di azioni, operazioni, stati del sistema e' proporzionale al ricorso alla memoria da parte dell'utente
	- lo stile, la funzionalita' di help, il modo di gestione degli errori sono una indicazione della complessita' della UI e del livello di accettazione da parte degli utenti

