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

## TCP/IP: Il livello di rete
- In TCP/IP il livello di rete e' implementato dal Internet Protocol (IP)
- Due versioni di IP attualmente in uso su Internet:
	- IPv4 $\rightarrow$ la versione attuale, che e' quella di gran lunga piu' utilizzata
	- IPv6 $\rightarrow$ la versione futura di IP, attualmente in fase di adozione iniziale

### IPv4 header
![[IPv4Header.png]]

### IPv4
- Un indirizzo IPv4 e' composto da 32 bit e suddiviso in una parte che identifica la Local Area Network (LAN) in cui esso si trova (network ID) e in una parte di host (host ID)
- La lunghezza del network ID e' variabile
- Supporto al subnetting
- Rappresentazione di indirizzi IPv4 segue la cosiddetta "dotted notation":
	- es. 192.168.0.1/24
- Il numero massimo teorico di indirizzi IPv4 e' di circa 4 miliardi (2^32)
- In realta' non tutti gli indirizzi sono utilizzabili, visto che alcuni indirizzi (~18 milioni) sono riservato per l'uso in reti private:
	- 10.0.0.0/8 (255.0.0.0)
	- 172.16.0.0/12 (255.240.0.0)
	- 192.168.0.0/16 (255.255.0.0)
- In piu' altri indirizzi sono riservati per multicast

## Routing in una LAN
- All'interno di una LAN i dispositivi IP possono comunicare direttamente tra loro semplicemente usando le funzioni offerte dal protocollo di livello 2
- Uso di Address Resolution Protocol (ARP) per scoprire quale indirizzo 2 (link) corrisponda a un dato indirizzo di livello 3 (IP)

## Oltre una LAN: Internet e AS
- Internet e' un insieme di Autonomous System, ovverosia reti di primo livello, interconnesse
- Al loro interno gli AS hanno molte altre sottoreti

## Routing al di fuori di una LAN
- Per comunicare con nodi al di fuori di una LAN si usa il meccanismo di IP routing
- Speciali dispositivi, detti *router*, si occupano dell'instradamento dei pacchetti
- Ciascun router ha una *tabella di routing*, che contiene le informazioni di instradamento dei pacchetti
- L'instradamento viene fatto a seconda dell'IP di destinazione di un pacchetto, seguendo la regola del *longest matching prefix* 

### Protocolli di routing
- La dimensione spesso enorme delle reti impedisce di configurare le informazioni di instradamento manualmente sui router
- Necessita' di protocolli che gestiscano automaticamente le tabelle di routing
	- BGP per routing inter-AS
	- OSPF e RIP per routing intra-AS

## Problemi di IPv4
- IPv4 ha un numero molto limitato di indirizzi
- Gli indirizzi IPv4 sono praticamente esauriti
	- Quelli a disposizione di Europa e Asia sono esauriti rispettivamente nel 2012 e nel 2011
- Necessita' di meccanismi per scoprire questa mancanza:
	- NAT
	- Migrazione a IPv6

### Network Address Translation (NAT)
- NAT e' una soluzione che trasforma automaticamente indirizzi privati in indirizzi globali
	- All'attraversamento di un dispositivo NAT (tipicamente un router di confine), i pacchetti IP in uscita vengono modificati sostituendo all'indirizzo de mittente X un indirizzo pubblico Y
	- I pacchetti IP in ingresso per l'indirizzo Y saranno manipolato in modo inverso. Ovverosia, all'indirizzo Y pubblico sara' sostituito l'indirizzo privato X.
- Possibilita' di connettere a Internet reti con un numero elevato di indirizzi privati utilizzando un numero relativamente basso di indirizzi pubblici
- NAT e' uno strumento molto utilizzato (praticamente obliquo)
- Sfortunatamente il NAT e' un approccio che presenta anche numerosi svantaggi:
	- Problemi nel caso si voglia rendere accessibili all'esterno servizi che girano nella rete con indirizzi privati
		- Necessita' di configurazione ad hoc per server
		- Uso di protocolli per consentire al traffico esterno di passare attraverso il NAT (STUN, TURN, ICE, ecc.)
	- Problemi con alcuni tipi di servizi
		- Ad esempio, per abilitare FTP attivo e' necessario un NAT stateful con uno specifico modulo di protocol translation
	- **Nella Internet moderna, esistono molti strumenti, denominati "middleboxes", che, come NAT, manipolano il traffico per vari motivi e possono rappresentare un problema per le applicazioni**

## IPv6
- IPv6 e' la nuova versione di IP, specificatamente progettata per superare i limiti di scalabilita' di IPv4
- IPv6 adotta indirizzi di 128 bit
	- Questo consente di allocare (in teoria) $3,4 \cdot 10^{38}$ indirizzi (un numero di indirizzi per $m^2$ per superficie della Terra maggiore del numero di Avogardo)
- Numerosi vantaggi rispetto a IPv4
	- Adozione di un formato di frame piu' semplice (e quindi veloce da processare per i router)
	- Indirizzi con diversi scope: link-local, site-local (deprecato) e global
	- Ripensamento di alcuni protocolli in favore di soluzione piu' robuste (es. da ARP e Neighbor Discovery)
	- Estensioni per connettere dispositivi low-power

### IPv6 header
![[IPv6Header.png]]

### Indirizzi IPv6
- Rappresentazione esadecimale
	- FEDC:BA98:7654:3210:FEDC:BA98:7654:3210
- Sequenza di zeri sono abbreviate con "::"
	- 1080:0:0:0:0:0:200C:417A = 1080::200C:417A
- Tre tipologie di indirizzo:
	- Unicast
	- Anycast
	- Multicast
- Vari scope di indirizzamento
	- Link Local (FE80::/10)
	- Site local (FEC0::/10) (deprecato)
	- Global (2000::/3)
	- Multicast (FF00::/8)
- Indirizzi di tipo speciale:
	- IPv4-mapped (::FFFF:0:0/96)
	- IPv4-compatible (::/96)
	- 6t04

### Adozione di IPv6
- Per supportare IPv6 c'e' la necessita' di modificare tutto il software (applicazioni e sistemi operativi) e il firmware (che gira in router, switch, gateway, ecc.) di rete
	- Talvolta questo comporta notevoli problemi (es. FTP, gateway DSL)
- La maggior parte di sistemi operativi, applicazioni e router supporta gia' IPv6

## Compatibilita' IPv4 e IPv6
- Al moment esistono fondamentalmente 3 situazioni per quanto riguarda la connettivita' IP:
	- reti con sola connettivita' IPv4
	- reti con sola connettivita' IPv6
	- reti sia con connettivita' IPv4 che con connettivita' IPv6
- **Bisogna progettare applicazioni in grado di funzionare in tutte e tre le condizioni summenzionate**
- Servono soluzioni per far parlare le reti del primo tipo con quelle del secondo

## Diffusione di IPv6
![[diffusioneIPv6.png]]

## Modello a doppia clessidra
![[modelloDoppiaClessidra.png]]

## TCP/IP e mobilita'
- Purtroppo TCP/IP e' stato progettato per una rete fissa e non offre un buon supporto per i terminali mobili
	- La causa fondamentale e' la duplice natura di ID e di "locator" degli indirizzi IP
- Adozione di diversi meccanismi per fornire il supporto alla mobilita' dei nodi
	- DHCP
	- Mobile IP

### Dynamic Host Configuration Protocol (DHCP)
- DHCP e' il protocollo di autoconfigurazione attualmente piu' usato
- DHCP assegna dinamicamente un indirizzo IP a un nodo
	- Time-based leasing per riuso indirizzi
	- Riassegnazione dello stesso indirizzo allo stesso host se possibile
- Inoltre, DHCP configura anche gateway e DNS
- In IPv6 il ruolo del DHCP viene tipicamente svolto dallo Stateless Address Autoconfiguration (esiste pure DHCPv6, ma e' quasi inutilizzato)

### Mobile IPv6 (MIPv6)
- Piu' performante di MIPv4
	- Route optimization incluso nel protocollo
	- Uso di Routing Header vs. incapsulazione
	- Dynamic Home Address Discovery usa anycast e ritorna una singola risposta al nodo mobile
	- Largo uso di piggybacking grazie alle Dest. Opt.
- Piu' sicuro di MIPv4
	- Uso obbligatorio di IPSEC
	- Facilitato l'uso di packet filtering
- Piu' robusto e flessibile di MIPv4:
	- Uso di Neighbor Discovery al posto di ARP
	- Facilitato il routing di traffico multicast
	- Non sono piu' necessari Foreign Agent
	- Meccanismo di movement detection bidirezionale
	- Nuova opzione "Advertisement Interval" sul Router Advertisement
- Cio' nonostante, nemmeno MIPv6 e' in grado di risolvere completamente i problemi di mobilita' di IP
	- Network Mobility
	- Administrative burden
	- Always best connected


## Multipath TCP
- Versione di TCP con supporto multipath (ovverosia a connessioni che usano $N>1$ endpoint e $N$ subflow) pensata per dispositivi mobili
- Supportato da tutti i principali sistemi operativi, soprattutto in ambito mobile 
- Soluzione (relativamente) semplice ma molto efficace
![[MultipathTCP.png]]

## TCPIP: sotto il livello di rete
- L'ubiquita' di IP permette all'ingegnere informatico, che si occupa di sviluppare applicazioni di rete, di non curarsi dei protocolli di livello 1 e 2
	- Ragioniamo sempre in termini di scambio di messaggi su TCP o UDP
	- In larga misura, si puo' anche ignorare la tecnologia di comunicazione sottostante
- Tuttavia, si deve dare sempre molta attenzione ad alcuni aspetti molto importanti:
	- Performance
	- Middleboxes
	- Supporto a IPv6

## Interne - Organi di Controllo
- Il principale organo di controllo di Internet e' la Internet Society (ISOC)
- ISOC e' un'organizzazione internazionale (con sedi a Washington e Ginevra) non governativa per la promozione dell'utilizzo dell'accesso a Internet
- La Internet Architecture Board (IAB) e' l'organo tecnico di ISOC
- IAB ha diverse sotto-organizzazioni, la piu' importante delle quali e' certamene IETF

### Internet Engineering Task Force
- IETF e' l'organo che ha il compito di definire le specifiche dei protocolli di rete usati su Internet
- Composta da ingegneri e ricercatori che contribuiscono gratuitamente con il proprio lavoro
- Diversi comitati (working group) che lavorano su un tema specifico
- Organizzazione democratica

## RFC
- I Request For Comments (RFC) sono i documenti fondamentali che definiscono gli standard di Internet

### RFC: Processo di Pubblicazione
![[RFC.png]]

## Internet Assigned Numbers Authority (IANA)
- IANA e' un dipartimento di ICANN con il compito di coordinare alcuni degli elementi chiave che permettono a Internet di continuare a funzionare senza intoppi
- In particolare, IANA gestisce:
	- Assegnazione corrispondenze tra servizi e numeri di porta
	- Allocazione indirizzi IP agli Internet Register
	- Root DNS

## Internet Registry
- Gli indirizzi IP (sia IPv4 che IPv6) vengono generalmente assegnai in modo gerarchico
- Gli utenti ottengono gli indirizzi IP dal proprio Internet Service Provider (ISP)
- Gli ISP ottengono l'assegnazione di indirizzi IP da un Local Internet Registry (LIR), da un National Internet Registry (NIR), o dal corrispondente Regional Internet Registry (RIR)

### Regional Internet Registry
- Esistono 5 RIR:
	- AfriNIC $\rightarrow$ Africa Region
	- APNIC $\rightarrow$ Asia/Pacific Region
	- ARIN $\rightarrow$ North America Region
	- LACNIC $\rightarrow$ Latin America and some Caribbean Islands
	- RIPE NCC $\rightarrow$ Europe, Middle East and Central Asia
- Il nostro RIR di riferimento e' ovviamente RIPE