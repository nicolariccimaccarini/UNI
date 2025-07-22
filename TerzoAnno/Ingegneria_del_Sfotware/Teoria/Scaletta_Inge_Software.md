### Modello di sviluppo a Cascata
- DPII: Definizione dei requisiti $\rightarrow$ Progettazione Sistema/Software $\rightarrow$ Implementazione/Testing unita' $\rightarrow$ Implementazione/Testing Sottosistema $\rightarrow$ Installazione/Manutenzione
- Visibilita' buona, produce deliverables ad ogni fase
- Rischio dipende dalla familiarita' del team con il problema
- Possibile passare a fase precedente, costi superiori rispetto ad Agile per cambiamenti requisiti

---
### Modello a Spirale
- CPASCrV: Comunicazione cliente $\rightarrow$ Pianificazione $\rightarrow$ Analisi Rischi $\rightarrow$ Strutturazione $\rightarrow$ Costruzione & Release $\rightarrow$ Valutazione Cliente
- Step si reiterano fino alla release
- Indicato per progetti grandi
- Visibilita' buona, ogni spicchio produce documentazione
- Rischi minimizzati da iterazioni; erronea valutazione puo' ripercuotersi

---
### Modello Evolutivo
- DSSV: Definizione di Massima $\rightarrow$ Specifica (versione iniziale) $\rightarrow$ Sviluppo (versione intermedia) $\rightarrow$ Validazione (versione finale)
- Importanza a prototipi usa-e-getta
- Visibilita' scarsa (documenti carenti), eleva il rischio
- Rischio ridotto grazie a numerosi prototipi
- Prodotto finale non deve essere un prototipo (riscrivere per release)
- Indicato per piccoli progetto o con vita attesa corta

---
### Modello Formale (Trasformazionale)
- DSTI: Definizione Requisiti $\rightarrow$ Specifica formale $\rightarrow$ Trasformazione Formale $\rightarrow$ Integrazione & Test
- Adeguato per codificare linguaggio matematico
- Rischio legato all'esperienza degli sviluppatori e necessita' di tecnologie avanzate
- Visibilita' buona (ogni fase dovrebbe produrre documentazione)

---
### Modello basato sul Riutilizzo
- AAPI: Analisi Componenti $\rightarrow$ Adattamento Requisiti $\rightarrow$ Progettazione Sistema $\rightarrow$ Integrazione
- Basato su componenti off-the-shelf
- Visibilita' buona ma macchinoso documentare componenti off-the-shelf
- Rischio ridotto (correttezza dei componenti presupposta)
- Si presta a sviluppo software ad oggetti

---
### Modello RAD (Rapid Application Development)
- CPMCD: Comunicazione $\rightarrow$ Progettazione $\rightarrow$ Modellazione $\rightarrow$ Costruzione $\rightarrow$ Deployment
- Modellazione e Costruzione ad opera di team per partizione
- Indicato per progetti facilmente partizionabili, senza interfacce appositamente definite
- Non indicato con tecnologie innovative ad alto rischio

---
### RUP (Rational Undefined Process)
- IECT: Inception $\rightarrow$ Elaboration $\rightarrow$ Construction $\rightarrow$ Transition
- Si rifa' ad UML
- Prende elementi da vari modelli generici e fornisce buone prassi
- Indicato per sistemi che usano cicli
- Milestones se non soddisfacenti, reiterare o abortire

---
### Modello Agile
- Persone e iterazioni $>$ processi e strumenti
- Software e funzioni $>$ documentazione
- Comunicazione fondamentale
- Capacita' di risposta ai cambiamenti $>$ aderire al progetto
- Sviluppo da due sviluppatori (uno scrive, uno assiste) $\rightarrow$ pair programming
- Costi ridotti per cambiamento requisiti rispetto a cascata

---
### Verifica & Validazione
- Verifica $\rightarrow$ "Si sta costruendo il prodotto nel modo giusto?" (Soddisfa specifiche cliente)
- Validazione $\rightarrow$ "Si sta costruendo il prodotto giusto" (Soddisfa richieste utente)
- Test statici $\rightarrow$ Analisi del codice prima dell'implementazione per problemi
- Test dinamici $\rightarrow$ collaudo del software
- Impossibile validazione solo in maniera dinamica

---
### Revisione VS Walkthrough
- Revisione $\rightarrow$ Riunioni per esaminare codice post-sviluppo con checklist errori
- Walkthrough $\rightarrow$ Lettura critica del codice per simularne esecuzione; processo collaborativo

---
### White-box VS Black-box
- White-box
	- Si conosce il codice
	- Controlla cammini indipendenti interni ai moduli
	- Estensivo, ad opera degli sviluppatori
	- Studia aspetti procedurali, non mancanze requisiti
- Black-box
	- Non si conosce il codice
	- Valuta conformita' specifiche dei requisiti
	- Cerca comportamenti erronei o mancanze
	- Complementare al white-box

---
### Back-to-back testing
- Usato con piu' versioni diverse dello stesso sistema
- Confronto output per errori potenziali (puo' essere automatizzato)
- Usato con prototipi, versioni multiple, upgrade/release

---
### Requisiti Funzionali e non Funzionali
- Funzionali $\rightarrow$ servizi o funzionalita' che il programma offre
- Non-Funzionali $\rightarrow$ Aspetti di programmazione (es. librerie, sistemi sviluppo), o caratteristiche (reliability, performance, security)

---
### Problemi nella raccolta dei requisiti
- Scope $\rightarrow$ limiti del sistema mal definiti, requisiti non necessari
- Comprensione $\rightarrow$ difficolta' di comunicazione cliente/sviluppatore; cliente non sa cosa vuole
	- Soluzione $\rightarrow$ Dialogo con stakeholders, studio ambiente
- Volatilita' $\rightarrow$ requisiti possono cambiare nel tempo

---
### Fase di Negoziazione durante Analisi dei Requisiti
- Scopo $\rightarrow$ Valutare requisiti clienti e vincoli stakeholders/ambiente
- Obiettivo $\rightarrow$ Trovare compromessi e risolvere conflitti per accontentare tutte le parti

---
### Analisi dei Cammini Critici
- Cammino critico $\rightarrow$ Successione di attivita' il cui ritardo causa ritardo nella consegna
- Analisi $\rightarrow$ Studio disposizione temporale compiti per ottimizzare personale, dipendenze, tempi di consegna
- Scopo $\rightarrow$ Ridurre la durata sviluppo progetto
- Strumento $\rightarrow$ Diagramma di Pert
- Costo $\rightarrow$ Oneroso a progetto gia' in esecuzione

--- 
### Piano di Progetto VS Piano Esecutivo
- Piano di Progetto $\rightarrow$ Sequenza potenziale compiti, solo vincoli di precedenza (ignora durata e risorse)
- Piano esecutivo $\rightarrow$ Sequenza effettiva delle attivita', tiene conto fattori economici, tempistiche, gestione personale

---
### Rapporto Mese-Uomo
- Valuta effort di un progetto in base al numero di sviluppatori
- Efficace solo se compito perfettamente partizionabile e senza comunicazione
- Aggiunta programmatore richiede formazione (riduce produttivita' altri)

---
### Scomposizione Modulare
- Raffinamento strutturale, sottosistemi scomposti in moduli
- Riduzione tempo di sviluppo
- Il sistema deve essere partizionabile
- Esempi:
	- Modello a oggetti $\rightarrow$ interfacce definite, modelli di controllo
	- Modello data-flow $\rightarrow$ "Pipe-and-Filter", adatto a gestione dati (non sistemi interattivi)

---
### Criteri di Mayer
- Compromesso tra costi di sviluppo e integrazione
- 5 Criteri:
	- Scomponibilita' $\rightarrow$ Meno complessita'
	- Componibilita' $\rightarrow$ Piu' produttivita'
	- Comprensibilita' $\rightarrow$ Interfacce minime, facilita' di costruzione/modificabilita'
	- Continuita' $\rightarrow$ Poche ripercussioni da modifiche requisiti
	- Protezione $\rightarrow$ Effetti anomali non si ripercuotono

---
### Altre Strategie di Modularizzazione
- Top-Down $\rightarrow$ Dal generale al dettaglio
- Bottom-Up $\rightarrow$ (o per composizione), contrario di top-down
- Sandwich $\rightarrow$ mix tra le due (soluzione naturale)

---
### Modello Client-Server
- Base $\rightarrow$ Richiesta servizi da client a server
- Three-tier architecture (MVC)
	- Presentation Layer $\rightarrow$ interfaccia grafica
	- Application processing layer $\rightarrow$ funzionalita' del sistema
	- Data management layer $\rightarrow$ comunicazione con DBMS
- Vantaggi $\rightarrow$ Semplicita', scalabilita', contenimento costi
- Svantaggi $\rightarrow$ Necessita' di replicare componenti gestione dati, calo prestazioni senza modello unico

---
### Design Pattern
- Soluzioni consolidate e accettate per problemi correnti
- Vantaggi $\rightarrow$ favorisce il concept-reuse, codice chiuso a modifiche e aperto a estensioni

---
### 3 Regole della UI
1. Il controllo all'utente $\rightarrow$ interazione flessibile, annullamento operazioni
2. L'utente deve ricorrere il meno possibile all'uso della memoria:
	- interfaccia di facile lettura, conforme a standard, riconoscimento (non apprendimento)
	- fornire info su operazioni precedenti e progressive
	- maggiore memorizzazione = maggiore propensione a errori
3. L'interfaccia deve essere uniforme $\rightarrow$ aspetto grafico consistente, rispetto canoni estetici/usabilita'

---
### Valutazione Gravita' rischio
- Valutare impatto, conseguenze e probabilita'
- Non tutti i rischi sono uguali (es. fallimento corruttivo vs transiente recuperabile)
- Chiedersi "cosa si perde qualora il rischio si verificasse?"
- Fattore rischio = probabilita' $X$ gravita'

---
### Affidabilita' di un Sistema Software
- Probabilita' che il sistema funzioni senza errori per dato intervallo, ambiente, scopo
- Difficile e costoso ottenere affidabilita' totale
- Priorita' $\rightarrow$ diminuire possibilita' di fallimenti gravi (vs recupero da transienti o fault avoidance)
- Costo $\rightarrow$ sforzi per affidabilita' possono ridurre prestazioni/efficienza 

---
### Diversita' Problemi HW e SW
- Software $\rightarrow$ non si erode, non si sostituisce componente guasta per risolvere stesso problema
- Hardware $\rightarrow$ ridondanza costruttiva per malfunzionamenti
- Ridondanza Software $\rightarrow$ diverse implementazioni stesso modulo, confronto soluzioni (non repliche)