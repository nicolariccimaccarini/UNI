1. Cos’è lo stato di un database  
	- Si riferisce al contenuto del database in un particolare momento (solitamente adesso)

2. Cos’è l’architettura a 3 livelli?
	- Proposta per supportare le caratteristiche dei DBMS di:
		- indipendenza tra programmi e dati
		- supporto di visite multiple dell'utente
		- uso di un catalogo per memorizzare la descrizione (schema) del database
	- (Soluzione per separare applicazioni, utenti e basi dati)

3. Cosa si intende per indipendenza logica e fisica dei dati?
	- Capacita' di modificare lo schema logico senza dover cambiare gli schemi esterni e le loro applicazioni

4. Quale, in ER, non è un tipo di attributo?
	- Atomico

5. In EER, significato di vincolo di disgiunzione? (d)
	- Le sottoclassi DEVONO essere disgiunte (un'istanza puo' essere membro di solo una sottoclasse della specializzazione - denotata come **d** nei diagrammi EER)

6. Quale non è un vantaggio per i DBMS?
	- Contenimento dei costi

7. Quale affermazione non è vera? (sulla SK)
	- Ad ogni superchiave minimale è associato uno ed un solo attributo chiave

8. Perché tuple non ordinate?
	- Perché una relazione è “un insieme di tuple”

9. Per portare schema relazionale in 1a forma normale, quale operazione non server?
	- Eliminazione campi derivati

10. Operazione non insiemistica?
	- Join (La sequenza di un'operazione di prodotto cartesiano seguita da una di selezione, e' usata molto comunemente per identificare e selezionare tuple correlate da due relazioni. Questa operazione e' molto importante per tutti i database relazionali con piu' di una relazione perche' permette di eseguire associazioni tra relazioni)

11. A cosa server “Having” ?
	- Ad esprimere delle condizioni sulle funzioni calcolate di una query
	- La funzione HAVING viene utilizzata per specificare una condizione di selezione su gruppi (piuttosto che su singole tuple)

12. Cos’è il processo di normalizzazione?
	- Test a cui sottoporre la base di dati per sapere in quale forma essa sia
	- DA DEFINZIONE: 
		- processo di analisi degli schemi di relazione forniti basato sulle loro dipendenze funzionali e chiavi primarie, per raggiungere le proprieta' di minimizzazione della ridondanza e minimizzazione delle anomalie di inserimento, cancellazione e modifica.
		- Quindi il processo di normalizzazione sottopone uno schema di relazione a una serie di test per "certificare" se soddisfa una data formale normale

13. Aggiornamento della relazione “studente”
	- UPDATE studente SET nome=”Piero” WHERE id = 001

14. Parzializzazione
	- Processo di scomposizione di una tabella o modello relazionale in tabelle piu piccole

15. Differenza tra prodotto cartesiano e join?
	- Il prodotto cartesiano restituisce tutte le combinazioni possibili tra le righe di due tabelle. Join fa lo stesso ma basandosi su una condizione di associazione specificato

16. DDL
	- operazioni sulla struttura dati
	- DDL (Data Definition Language): usato dal DBA e dai progettisti per specificare lo schema logico del database. In alcuni DBMS usato anche per definire gli schemi interno ed esterno.

17. DML
	- operazioni sui dati
	- DML (Data Manipulation Language): usati per specificare le interrogazioni e gli aggiornamenti del database. 

18. Operazione non DDL?
	- Truncate

19. Quale non è un vincolo?
	- Delle associazioni

20. Cos’è una superchiave?
	- Insieme di uno o più attributi che identificano univocamente una tupla
	- DEFINIZIONE: un insieme di attributi SK di R tale che non esistano due tuple che abbiano lo stesso valore per SK in nessuno stato di relazione valido per $r(R)$.

21. Differenza tra completezza totale e parziale
	- Totale: le superclassi hanno ALMENO una sottoclasse
	- Parziale: le superclassi NON hanno sottoclassi

22. Cos’è un’associazione in ER?
	- Una rappresentazione delle relazioni tra le entità in un database
	- DEFINIZIONE: un'associazione mette in riferimento due o piu' entita' con uno specifico significato.

23. Cos’è una relazione in EER?
	- Tabella dei valori

24. Cos’è una relazione nel modello relazionale?
	- Un concetto matematico basato sugli insiemi

25. Perché è importante l’algebra relazionale?
	- Perché pone le basi formali per le operazioni nel modello relazionale

26. Nell’EER cos’è il vincolo di overlap?
	- Le sottoclassi POSSONO avere sovrapposizioni

27. Quale tra queste rappresenta la descrizione di database?
	- Lo stato

28. Cosa si intende per “Natura autodescrittiva di una base di dati”?
	- La base di dati contiene, oltre ai dati, la descrizione della sua struttura e dei suoi vincoli

29. Che cos'è un DBMS?
	- Un programma che permette la gestione di database

30. Fra i linguaggi attuali dei DBMS quale possiamo escludere?
	- DSL (Data Segmentation Language)

31. Quale delle seguenti è una associazione N:M :
	- Persona ha titolo di studio