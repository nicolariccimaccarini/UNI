## Il fattore umano
Le persone sono diverse, non sono tutte uguali, alcune differenze possono essere significative, altre meno.

Progettare un sistema informatico che vada bene per tutti e' impossibile $\Rightarrow$ Si seleziona un sottoinsieme.
- Conoscere cio' che condiziona il sottoinsieme e' molto importante.
- Al confine con due sottoinsiemi il sistema potrebbe funzionare male o non funzionare per niente.

### La scala del tempo umano
La "scala del tempo umano", introdotta da Newell agli inizi degli anni 90, rappresenta un modello descrittivo per le azioni che compie l'uomo.
Il modello prevede 4 "bande" in cui vengono categorizzate le azioni che l'uomo compie suddividendole in base al tempo in cui avvengono

![[ScalaDelTempo.png]]

### I sensori
I 5 sensi dell'uomo hanno il compito di farci percepire le proprieta' fisiche dell'ambiente.

| Senso   | Sensore  |
| ------- | -------- |
| Vista   | Occhio   |
| Udito   | Orecchio |
| Tatto   | Pelle    |
| Gusto   | Lingua   |
| Olfatto | Naso     |

### Il colore
La vista e' un senso molto importante, la maggior parte delle persone ottiene l'80% delle informazioni sull'ambiente circostante attraverso questo senso.

L'organo che gestisce la vista e' l'occhio, il quale percepisce la luce proveniente dall'ambiente circostante convertendola in impulsi neurologici.

La proprieta' della luce che guida la percezione del colore e' la **frequenza** che viene misurata in nanometri ($nm$).
Le frequenze percepibili dall'occhio umano sono solo una parte limitata dello specchio elettromagnetico fra i $350 \space nm$ e i $750 \space nm$

#### Il modello RGB
Il modello RGB e' un sistema di codici di colori internazionale le cui specifiche sono state definite dalla "Commission internationale de l'eclairage" (CIE) nel 1931.
Il modello RGB si basa su tre colori fondamentali che sono il Rosso, il Verde ed il Blu.

Il modello RGB e' un modello detto "additivo" cioe' ogni colore e' dato dall'unione dei 3 colori fondamentali.
L'unione dei 3 colori fondamentali nella loro massima intensita' add il colore bianco.

Per ottenere un colore nel sistema RGB e' necessario specificare il valore per ogni componente (canale o banda) Red, Green, Blue.
Uno specifico colore quindi si ottiene come insieme (unione) dei 3 colori di base nel formato x, y, z assegnando per ogni canale un valore da 0 a 255 compresi.

#### Il modello HEX
Sul Web ha preso un altro modello di rappresentazione dei colori, utilizzato al posto di RGB ma non di RGBA.
La rappresentazione colore HEX (esadecimale) converte il formato RGB in un formato piu' compatto: `#XXYYZZ`
Il formato esadecimale e' composto dal simbolo # seguito da 3 valori compresa fra 0 e FF base 16 senza separatore.
Il primo rappresenta la componente rossa, il secondo la componente verde e l'ultimo la componente blu.

#### Il significato dei colori
I colori hanno un significato ed e' importante rispettare casi fondamentali:

![[significatoColori.png]]

### La lettura 
Yarbus, uno studioso russo, ha condotto alcuni studi sul modo di osservare in relazione allo scopo.
Dai suoi dati e' emerso che le persone osservano in modo diverso a seconda di alcuni fattori come eta', sesso, ecc.

Gli studi di Yarbus possono essere interessanti anche per il design dei siti web, un inserzionista potrebbe voler sapere qual'e' la posizione migliore all'interno di un sito Web per posizionare gli annunci pubblicitari.

## Le interazioni
Le interazioni uomo-macchina avvengono quando un uomo esegue un compito con l'aiuto della macchina.

I compito possono avere o non avere uno scopo:
- Inviare un'email
- Inserire la destinazione sul navigatore
- Navigare su internet
- Chattare su Instagram

Qualunque compito l'uomo compia che prevede il coinvolgimento di una macchina, allora ci sara' un'interazione che puo' posizionarsi in un range di tempi che vanno dai $100 \space ms$ ai $10 \space s$.

### Hard Controls
Prima che i computer invadessero le nostre vite, display e controllers erano dispositivi fisici separati e una volta costruiti non potevano cambiare la loro funzionalita', il loro compito era fissato e quindi il loro utilizzo molto limitato.

### Soft Controls
Con l'avvento dei computer sono nati quelli che vengono chiamati "Soft Controls".

Grazie alle GUI il bottone che alla mattina serviva per avviare una funzione, entro la pausa pranzo potrebbe aver completamente la sua funzione.

Inoltre i Soft Controls hanno spesso anche la proprieta' di display, pensiamo alle toolbar in cui quando una funzioni e' attivata, il pulsante risulta essere "illuminato".

### Relazioni Controller-Display
Quando l'utente prende il mouse e lo sposta a destra e a sinistra, il puntatore va a destra o a sinistra. Questa relazione e' qualcosa a cui l'untente non pensa molto, e' una relazione ovvia e **naturale** per l'utente, e' qualcosa che ci si aspetta.

Una buona interazione Uomo-Computer dev essere cosi', qualcosa di "ovvio"

FARE DA 41 A 48