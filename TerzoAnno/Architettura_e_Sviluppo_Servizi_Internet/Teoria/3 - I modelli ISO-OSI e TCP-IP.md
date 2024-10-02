## Layering
- Per gestire la complessita' delle comunicazioni e l'estrema eterogeneita' di hardware, software e tecnologie di canale si adotta un approccio a strati (layered)
- Il modello di riferimento e' ISO/OSI
	- Inizialmente concepito come implementazione state-of-the-art, ora rimane di interesse esclusivamente come modello teorico

- ISO/OSI propone un'architettura di soluzione per arrivare a descrivere una comunicazione ICT complessa
	- L'architettura si basa sul principio dell'astrazione che richiede di nascondere dettagli e mostrare solo le entita' utili significative per l'utente finale (cliente)
- Avendo un problema complesso, si introducono una serie di astrazioni, i livelli, che consentono di risolvere il problema separando gli ambiti in modo ordinato e ben identificato
- Si definiscono una serie di livelli per decomporre il problema e affrontare le complessita' separatamente
- Il "divide et impera" e' il principio ingegneristico 0 per la gestione di complessita' ed eterogeneita'.

### Vantaggi dell'approccio layered
- L'approccio layered permette a progettisti e sviluppatori che realizzano soluzioni di livello X di non curarsi dei protocolli di livello sottostante
	- E' sufficiente rispettare le interfacce fornite dai protocolli di livello sottostante
- A ogni livello possiamo ragionare indipendentemente
	- E' possibile impilare i protocolli come vogliamo, purché soddisfino i vincoli di interfacciamento
	- Questo consente di **sostituire, sviluppare e aggiornare i singoli protocolli senza modificare il rest dello stack**

## Il modello ISO/OSI
- Modello a 7 layer su IBM SNA
- Partendo dal livello applicazione (top-down), ogni livello ha l'obiettivo di comunicare con il pari e lo realizza tramite protocollo, ossia un insieme di passi, che realizza usando il servizio sottostante
- Ogni livello fornisce un servizio specificato e disponibile a livello superiore
- Ogni sistema a livelli si basa sul **principio della separazione dei compiti (delega) e della trasparenza della realizzazione (astrazione)**

- In genere, per un'azione di invio informazioni, possiamo avere:
	- **Mittente** $\rightarrow$ entita' che ha la responsabilita' di iniziare la comunicazione
	- **Ricevente** $\rightarrow$ entita' che accetta la comunicazione e poi la sostiene
	- **Intermediari** $\rightarrow$ eventuali nodi intermedi (access point, bridge, switch, router) che devono partecipare alla comunicazione e fornire risorse per sostenerla
- Il mittente manda dei dati a un ricevente che puo' anche rispondere all'invio con un'azione applicativa conseguente
- Ogni azione comporta una comunicazione che passa attraverso i livelli da applicativo fisico, del mittente e ricevente e almeno fino al livello di rete per gli intermediari

![[ModelloISOOSI.png]]

### Incapsulamento
- I 7 layer interagiscono tra loro solo tramite un set di interfacce rigorosamente definite
- I layer di livello piu' basso offrono dei servizi ai layer di livello superiore
- Incapsulamento delle informazioni di controllo di ciascun layer all'inizio del pacchetto dei dati trasmesso

### Livello 7 - Applicazione
- Il livello 7 fornisce protocolli utilizzati direttamente dalle applicazioni
- Esempio:
	- Trasferimento di file
	- Terminale virtuale
	- Posta elettronica

### Livello 6 - Presentazione
- Il livello 6 si occupa della rappresentazione delle informazioni in modo standard e di implementare servizi come la compressione e la cifratura delle informazioni
- Diverse modalita' di rappresentazione delle informazioni
	- Astratta (o formale)
	- Concreta (o locale)
	- Di trasferimento

### Livello 5 - Sessione
- Il livello 5 coordina il dialogo tra gli utenti basandosi sul servizio offerto dal livello di trasporto
- Funzioni svolte $\rightarrow$ gestione del dialogo, sincronizzazione, gestione attivita'
- La principale funzione del livello 5 e' quella di stabilire gli utenti delle sessioni di lavoro (gestione dialogo). Una sessione di lavoro fa uso di una connessione di trasporto, con diverse possibilita' di mapping
- Supporto a sincronizzazione e rollback

### Livello 4 - Trasporto
- Il livello 4 permette il trasferimento di dati tra due nodi
- E' il primo livello della pila ISO/OSI che opera in modello end-to-end
- Il livello 4 si occupa di:
	- Stabilire e mantenere connessioni tra nodi in comunicazione
	- Implementare meccanismi di affidabilita' end-to-end
	- Controllo di congestione

### Livello 3 - Rete
- Il livello 3 permette il trasferimento di pacchetti oltre la rete locale (LAN)
- Responsabile di:
	- Instradamento (routing) dei pacchetti
	- Conversione dei pacchetti tra vari protocolli di livello 2
	- Frammentazione dei pacchetti

### Livello 2 - Collegamento (Link)
- Il livello 2 permette il trasferimento dei pacchetti al di sopra di un canale (link) di comunicazione
- Il livello 2 si occupa di:
	- Coordinamento all'accesso del canale da parte di piu' nodi
	- Controllo errori e ritrasmissioni (per implementare comunicazioni affidabili)
	- Controllo di flusso (per evitare che trasmissioni troppo veloci mettano in difficolta' macchine lente)

### Livello 1 - Fisico
- Il livello 1 si occupa di definire come i bit di informazione vengono trasmessi sul canale fisico
- Ad esempio:
	- Livelli di tensione
	- Durata del segnale che identifica un bit
	- Modulazione e codifica
	- Trasmissione half e full-duplex

![[Livello1ISOOSI.png]]

## Modello TCP/IP
- Per diversi motivi il protocollo OSI non ha mai visto luce
- La suite di protocolli TCP/IP, invece, che regge Internet, ha avuto un successo incredibile e rappresenta la tecnologia piu' importante nel campo delle reti di comunicazioni

### ISO/OSI vs TCP/IP
![[ISOOSIvsTCPIP.png]]
Il modello TCP/IP e' privo dei livelli di sessione e presentazione.

### Layering e incapsulamento in TCP/IP
![[LayeringTCPIP.png]]

### TCP/IP: il Modello a Clessidra
Il modello TCP/IP viene detto " a clessidra" perche' e essenzialmente basato sull'idea di convogliare tutte le comunicazioni attraverso un protocollo di comunicazione comune e onnipresente: IP.

### Servizi TCP/IP
- In TCP/IP ogni servizio e' associato a un ID che si chiama *porta*
- Per accedere a un servizio, dobbiamo specificare IP e porta su cui il servizio e' in attesa di richieste
- Per interagire con il fornitore di servizio dobbiamo rispettare uno specifico protocollo di comunicazione, ovverosia il protocollo di livello applicazione (es. HTTP per il Web)
  
- A ogni tipologia di servizio e' associato un protocollo di comunicazione unico
- Ad esempio:
	- per il Web si usano il protocollo HTTP (L7) sulla porta 80 al di sopra di TCP (L4) e IP (L3)
	- per il DNS si usano il protocollo DNS (L7) sulla porta 53 al di sopra di UDP (L4) e IP (L3)

### Livello di Trasporto in TCP/IP
- Al di sopra di IP, vi sono diversi protocolli di livello trasporto, ma i principali sono TCP e UDP:
	- TCP $\rightarrow$ permette di stabilire dei flussi di comunicazione bidirezionale affidabili tra due nodi
	- UDP $\rightarrow$ permette a un nodo di mandare dei messaggi a un altro nodo.
- Usando TCP o UDP, e il livello IP sottostante, e' possibile costruire applicazioni (o servizi)

#### Transmission Control Protocol (TCP)
- TCP offre un servizio CON CONNESSIONE e AFFIDABILE
	- connessione bidirezionale
	- consegna sequenziale
		- ordine corretto dei byte
		- ritrasmissione dei messaggi persi
		- possibilita' di trasmissione dati prioritari (out of bound)
		- non rispetta la separazione tra messaggi (se richiesto dalla semantica di servizio, il framming va implementato a livello applicativo)
	- controllo di flusso o bufferizzazione
	- controllo di congestione
	- multiplexing
	- semantica at-most-once

#### TCP header
![[TCPHeader.png]]

#### TCP: Setup connessione
![[TCPSetupConnessione.png]]

#### TCP: Consegna ordinaria
![[TCPConsegnaOrdinaria.png]]

#### TCP: Consegna affidabile
![[TCPConsegnaAffidabile.png]]

#### TCP: Controllo di flusso
- Il controllo di flusso e' fondamentalmente per Internet in cui sono presenti macchine molto diverse fra loro.
- Un server molto lento potrebbe essere saturato di messaggi inviati da clienti piu' veloci (e perdere quindi i messaggi)
- Il controllo di flusso serve per permettere a un ricevitore lento di rallentare l'invio dei messaggi da parte del mittente
- Il meccanismo fondamentale per il controllo di flusso e' la *finestra scorrevole (sliding window)*
- Il ricevente comunica al mittente la dimensione della propria finestra (ovverosia la dimensione della parte ancora disponibile nel proprio buffer di ricezione)
- **Meccanismo di retroazione efficace che permette di modulare la velocita' di trasmissione del mittente sulla base delle risorse del destinatario**

#### TCP: Controllo di congestione
- In seguito all'immissione di un numero maggiore di pacchetti rispetto a quanto dall'infrastruttura di rete riesca a gestire si possono verificare *congestioni di rete*
- Per risolvere una congestione e' indispensabile che i nodi comunicanti rallentino la velocita' di trasmissione
- TCP fornisce un meccanismo di *congestion control*, che cerca di inferire la presenza di congestioni lungo il percorso di comunicazione e in tal caso rallenta la velocita' di trasmissione
![[TCPControlloCongestione.png]]

![[TCPControlloCongestione2.png]]

#### TCP: Chiusura di connessione
![[TCPChiusuraConnessione.png]]

#### Versioni di TCP
- TCP Tahoe $\rightarrow$ prima versione con congestion control (seguita da TCP Reno, TCP NewReno, ecc.)
- TCP CUBIC $\rightarrow$ versione ottimizzata per reti ad alta banda e alta latenza
- Data Center TCP (DCTCP) $\rightarrow$ versione ottimizzata per comunicazioni tra nodi in uno stesso data center
- TCP BBR $\rightarrow$ recente e interessante sviluppo emerso dalla ricerca sul fenomeno del "bufferbloat"

## User Datagram Protocol (UDP)
- UDP offre un servizio SENZA CONNESSIONE e NON AFFIDABILE
	- non preserva ordinamento messaggi
	- non garantisce consegna di messaggi
	- non fornisce controllo di flusso
	- non fornisce controllo di congestione
	- multiplexing
	- semantica may-be
		- e' anche possibile che i messaggi vengano consegnati piu' volte

## TCP o UDP?
- La scelta del protocollo di trasporto da usare dipende dal tipo di applicazione che devo realizzare
![[TCPVSUDP.png]]

## TCP/IP: altri protocolli livello trasporto
- Esistono diversi protocolli di livello trasporto alternativi
	- STCP $\rightarrow$ semantica SEQPACKET con supporto a multiflusso e multihoming, ancora in fase di sperimentazione, non funziona con middleboxes
	- DCCP $\rightarrow$ in pratica UDP con congestion control, non molto diffuso, non funziona con middleboxes
	- QUIC $\rightarrow$ ottica dell'ottimizzazione delle performance, innovazione "disruptive", approccio a livello applicativo, basato su UDP
- Tuttavia, i principali protocolli rimangono TCP e UDP
