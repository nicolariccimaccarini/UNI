## La situazione fino ad ora
- Due definizioni che useremo
	- **Durata** $\rightarrow$ estensione temporale del progetto, dall'inziio alla fine; dipende dalle dipendenze tra le attivita' del progetto; di solito misurato in settimane o mesi.
	- **Sforzo** $\rightarrow$ somma dei tempi di tutte le attivita' di progetto; di solito si misura in mesi/persona
- La dimensione del team si ricava dal rapporto sforzo/durata

## Componenti del costo del progetto
- Costo dell'hardware di sviluppo
- Costo dell'hardware del software
- Corso delle risorse umane (sforzo)
- Durata complessiva

## Come misurare le dimensioni del progetto
- La maggior parte dei modelli che stimano durata e sforzo usano come parametro la dimensione
- Esistono varie misure che stimano la dimensione di un sw
	- Linee di codice
	- I punti funzione (Function Points)
	- Le storie (per i processi agili)
- Di solito vanno "calibrate" usando lo storico per adattarle al contesto aziendale

### Linee di codice
- La misura piu' comune della dimensione di un progetto sw e' il numero di linee di codice sorgente: Lines of Code (LoC) misurato in migliaia o milioni di righe (KLoC, MLoC)
- Come considero le linee vuote o i commenti?
	- In generale LoC = CLoC + NCLoC cioe' conto i commenti
- La distinzione piu' comune e' tra LoC fisiche e logiche

Pro e contro:
- Esistono anche tutta una serie di metriche derivate 
	- $ per LoC
	- errori e difetti per KLoC
	- LoC per mese/persona
	- pagina di documentazione per KLoC
	- ...
- Esiste gia' parecchia letteratura
- MA $\rightarrow$ dipendiamo dal linguaggio di programmazione e dallo stile
- MA $\rightarrow$ sottovaluta la produttivita' (penalizza programmi concisi)
- MA $\rightarrow$ e' difficile stimare la dimensione in LoC di una nuova applicazione
- MA $\rightarrow$ il codice non consegnato (es. i test) va contato?

### Function Point
- Questo valore conta le funzionalita' del sistema dal punto di vista dell'utente
- Analisi piuttosto complicata $\rightarrow$ unita' di misura per requisiti funzionali utente (FUR) di un prodotto per ottenere una stima piu' oggettiva delle LOC dell'impegno richiesto
	- requisiti $\rightarrow$ funzionali (FUR) e non funzionali (NFR)
	- function point misura solo FUR
	- per NFR ci sono approcci e tecniche diverse
- Associazione IFPUG (International Function Point User Group) $\rightarrow$ oltre alla base esistono 4 varianti diventate una standard a loro volta

## Modelli di costo
- I modelli di costo ci danno una stima rapida dello sforzo, utile durante le fasi iniziali del progetto
- Questa prima stima viene poi raffinata lungo il ciclo di vita del progetto, mediante dei fattori detti **cost driver**
- Ci si basa sull'equazione dello sforzo
	- $E = A + B \cdot S^C$
	- dove E e' lo sforzo in mesi-uomo
	- A, B e C sono parametri dipendenti dal progetto e dall'organizzazione che lo sviluppa
	- S e' la dimensione del prodotto stimata in KLOC o FP
- Vari modelli commerciali
	- COCOMO $\rightarrow$ Constructive Cost Model (modello parametrico piu' diffuso per fare stime nei progetti software. Tre livelli, basic, intermediate, advanced/detailed)
	- COSYSMO
	- SLIM
	- COSTEXPERT
	- ...

## Stima tempi (costi) con i metodi agili
- Dato che lo sviluppo si basa sulle user stories, occorre stimare lo sforzo necessario per user story
- Tecnica del planning poker (introdotta da SCRUM)
	- i membri del team seguono stime indipendenti su piu' turni
	- In un turno ognuno puo' cambiare la propria stima sulla base del turno precedente
	- Il "migliore" e il "peggiore" sono inviati a spiegare il perche'

## Planning poker (SCRUM poker)
- Product Owner legge una user story o descrive una feature al team
- Ogni membro del team ha un insieme di "carte" indicanti la durata in ore 
- Ogni membro fa tutte le domande che ritiene necessarie e il team discute come fare il lavoro, skills necessarie, possibili problemi, ...
- Ogni membro sceglie una carta (la propria stima) e tutte le carte scelte sono rivelate allo stesso tempo
	- Se c'e' sostanzialmente consenso: bene
	- Altrimenti, il valore piu' alto e piu' basso spiegano la stima
- Nuova iterazione

![[planningPoker.png]]

- Di solito si converge rapidamente
- Se non si converge (e i valori sono distanti):
	- si potrebbe decidere che stima e planning di quella particolare storia differita finche' non si hanno informazioni addizionali
- Se qualcuno gioca la carte "non so":
	- questa carta rappresenta l'impossibilta' di dare una stima
	- necessita di una discussione approfondita

- Funziona bene? Si
- Tutti i membri del team partecipano e dicono la loro
- Mette insieme pareri di piu' esperti
- C'e' una discussione in cui il lavoro da fare viene analizzato da piu' punti di vista e alla minima indecisione viene interpellato il PO: minori incertezze
- E' provato che essere chiamati a giustificare le proprie stime risulta in stime che compensano meglio per informazioni incomplete. E nella metodologia agile spesso le user stories sono intenzionalmente vaghe, almeno all'inizio.