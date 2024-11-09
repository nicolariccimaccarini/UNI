## Il problema del Naming
Necessita' di **identificare** (**e ritrovare**) le altre entita' (e risorse) nel sistema.
Complessita' del problema nel distribuito ma anche nel concentrato e difficolta' di soluzioni generali.
**I nomi sono organizzati in livelli:**
- **nome** $\rightarrow$ LOGICO
- **indirizzo** $\rightarrow$ FISICO
- **route** $\rightarrow$ come raggiungere l'entita'

Il **nome** specifica quale oggetto si riferisce, denomina l'entita':
- nome logico a livello utente
- identificatore come nome (interno) definito dal sistema

L'**indirizzo** specifica dove l'oggetto risiede
La **route** specifica come raggiungere l'oggetto

## Livelli di Nomi e mapping
Il passaggio nomi $\rightarrow$ indirizzi $\rightarrow$ route avviene attraverso *funzioni di corrispondenza* (**Mapping**):
- nomi $\rightarrow$ indirizzi
- indirizzi $\rightarrow$ route

Indirizzi come tramite tra nomi (statici) e route (dinamiche)
- nomi scelti dall'utente
- indirizzi assegnati dal sistema
- route gestite dal protocollo di rete (IP)

**Risoluzione** del nome: mapping dal nome all'indirizzo (piu' in generale, agli attributi corrispondenti al nome)

## Binding tra nomi e risorse
- **Binding** $\rightarrow$ associazione di un nome con un oggetto e il set di relativi attributi. Il binding e' il legame tra un nome e la specifica risorsa ad esso associata che il Client usera'.
- **I sistemi di nomi sono l'elemento fondamentale nella gestione del binding tra nomi e risorse in una rete di calcolatori**
- L'associazione tra nomi e oggetti/attributi tipicamente cambia nel tempo.
- Molto spesso, per motivi di scalabilita' o fault tolerance, e' desiderabile indicare con lo stesso nome piu' repliche di una risorsa anche in modo dinamico. Il sistema di nomi "controlla" quale specifica replica della risorsa associata al nome verra' utilizzata dal richiedente

Lato client sono possibili due diverse strategie per la gestione dei binding:
- **binding statico** $\rightarrow$ Il binding viene eseguito una sola volta e rimane inalterato per tutto il tempo di vita dell'applicazione
- **binding dinamico** $\rightarrow$ il binding ha durata limitata e viene rieseguito periodicamente. Questa abilita l'uso di un pool dinamico di risorse e consente di ottimizzarne l'efficienza, ad esempio ridirigendo le richieste sul gestore piu' scarico o quello in quel momento disponibile nel sistema, a fronte di un costo maggiore per il binding

## Un sistema di naming per internet
- Internet necessita di un servizio di nomi, con l'obiettivo principale di mantenere e fornire le corrispondenza tra nomi logici di host e gli indirizzi IP.
- Prima soluzione (191-1981) $\rightarrow$ database testuale *hosts.txt* da scaricare e copiare in `/etc/hosts`, mantenuto dallo Stanford Research Institute
- Qualsiasi database centralizzato rappresenta una soluzione insufficiente e non scalabile per il sistema di naming di Internet;
	- **Single point of failure** $\rightarrow$ se si guastasse il server contenente il sistema di nomi, l'intera rete internet ne risentirebbe
	- **Prestazioni** $\rightarrow$ tutte le richieste sarebbero concentrate su un solo nodo, causando significativi problemi di gestione (traffico, computazione)
	- **Distanza** $\rightarrow$ un sistema centralizzato porterebbe a significativi ritardi nel caso di client molto distanti, o che usino collegamenti lenti e congestionati
	- **Manutenzione molto frequente e onerosa** $\rightarrow$ ogni singolo cambiamento, dovrebbe essere riportato sulla base di dati del sistema centralizzato


## Domain Name System (DNS)
- Servizio di nomi decentralizzato basato sul concetto di **domini** amministrativamente indipendenti.
- **NOMI LOGICI GERARCHICI**
- Gerarchia di nomi logici:
	- 13 root server il cui indirizzo IP e' noto
	- Top Level Domains (.com, .it, ecc.)
- **Concetto di delega** $\rightarrow$ un dominio delega i sottodomini a gestori sottostanti (che se ne assumono responsabilita' e **autorità**)
- Domini non collegati a reti fisiche o organizzazioni (**logico vs fisico**)

### Nomi di DNS
- I singoli nomi sono **case insensitive** e al max 63 char, il nome completo max 255 char
- Ogni dominio indicato in modo **relativo** o **assoluto**. Ogni dominio deve fare riferimento al dominio che lo contiene.
![[DNSnames.png]]

### Risoluzione di un nome
- **Risoluzione** di un nome (conversione tra **nome logico** e **indirizzo fisico**) avviene tramite un servizio di nom che risponde dinamicamente alle richieste
- A livello di API si passa il riferimento da mappare a un **resolver** (Client) che:
	- o conosce gia' la risposta (cache)
	- oppure la trova attraverso una richiesta C/S a un **name Server**
- Ogni dominio corrisponde al **Name Server** che ha l'autorita' sulla **traslazione degli indirizzi**, che non ha una visione completa, ma solo locale
- Per ottimizzare le prestazioni e ridurre al minimo il traffico generato, ogni richiesta viene fatta al servizio di nomi tramite un **agente specifico** di gestione dei nomi per una localita' (DNS Resolver)

### DNS Resolver
Tipicamente, i client non effettuano le risoluzioni direttamente ma inoltrano le richieste a un componente **DNS Resolver** installato nella propria rete locale o fornito dal proprio ISP. Il DNS Resolver a sua volta effettua risoluzioni tramite query ricorsive e salva i risultati nella propria cache (migliori prestazioni).
![[DNSResolver.png]]\

### DNS Record Type
DNS salva le informazioni in **record**:
- A ogni nome possono essere associati molti record, sia di tipo diverso, che dello stesso tipo
- E' possibile effettuare diverse tipologie di interrogazioni al DNS specificando il tipo di record di interesse

Diversi tipi di record:
- A per risoluzioni da nome host a indirizzo IPv4
- AAAA per risoluzione da nome host a IPv6
- MX per risoluzione da nome dominio a nome dei server e-mail
- NS e SOA per informazioni su name server e dominio
- PRT per risoluzione inversa, ovverosia da indirizzo IP a nome, e DNS-SD

### DNS - protocollo di trasporto
DNS utilizza un semplice protocollo richiesta-risposta basato su UDP.
UDP scelto perche':
- **Non e' necessaria affidabilita'**. Il protocollo e' talmente semplice che e' facile realizzare una forma di affidabilita' a livello applicativo.
- **Non e' necessario congestion control**. Il protocollo occupa un traffico talmente limitato che e' molto improbabile che esso diventi una causa di congestioni
- **E' importante minimizzare la latenza nella risoluzione dei nomi**. Il setup di una connessione TCP rallenterebbe eccessivamente le procedure di risoluzione dei nomi

Nuove soluzioni in ambito DNS:
- DNSSEC
- DNS-over-HTTPS

### DNS - Formato messaggi
![[formatoMessaggiDNS.png]]

### dig
*dig* e' una utility per la diagnosi di problemi con il sistema di risoluzione nomi DNS. La sintassi e':
```
dig [@server] nome [tipo_di_query]
```
Alcuni esempi:
1. Ottieni le informazioni sul dominio unife.it:
   `dig unife.it`
2. Ottieni i record MX per il dominio unife.it:
   `dig unife.it mx`
3. Ottieni i record A (ovverosia l'indirizzo IP) per l'host www.unife.it

### Caching
Per velocizzare le richieste di risoluzione (successive). i sistemi di nomi fanno spesso uso di meccanismi di caching.
Il DNS prevede un sistema di caching distribuito a piu' livelli, di cui il DNS Resolver/Recurser  e' solo un elemento
![[Caching.png]]

### Tempo di validita' delle associazioni e caching
- Le associazioni tra nome e attributo possono cambiare, senza preavviso. 
- Questo puo' causare un disallineamento tra le informazioni (aggiornate) nel sistema dei nomi e quelle (obsolete) contenute nella cache.
- Necessita' di soluzioni per evitare che i Client usino informazioni invalide
- Nel DNS, i record restituiti alle richieste di risoluzione hanno un tempo di validita' esplicito (TTL $\rightarrow$ Time To Leave), scaduto il quale essi devono essere eliminati dalle eventuali cache in cui sono stati salvati

### Tempo di propagazione del DNS
Quando si apporta una modifica al database del DNS ci vuole tempo perche' essa abbia effetto, per vari motivi:
- Alcuni resolver ignorano il valore TTL dei record
- I resolver effettuano il caching dell'assenza di record per un nome (problema di definizione nuovo di nomi) 
- Browser e OS sono particolarmente aggressivi con il caching
- Il caching di informazioni sui nameserver e' tipicamente molto lungo

**Tempo di propagazione DNS** $\rightarrow$ tempo necessario perche' le informazioni nelle cache sulla rete vengano invalidate e possano essere rimpiazzate. Tipicamente, siamo nell'ordine delle 24 o 48 ore (dato empirico)

## URL: Uniform Resource Locator
![[URL.png]]

Si noti la complessita' del sistema dei nomi:
- piu' livelli di naming nel sistema
- piu' sistemi di naming presenti

## Spazio dei nomi
**NOME** e' una costruzione linguistica che distingue un oggetto in una collezione di oggetti.
Uno spazio dei nomi (**name space**) e' la collezione di tutti i nomi sintatticamente validi per un particolare servizio.
Lo spazio dei nomi puo' essere:
- piatto $\rightarrow$ nessuna struttura
- partizionato $\rightarrow$ **gerarchia** e **contesti (DNS)** (es. www.ing.unife.it - ing viene risolto nel contesto unife, ecc.)
- descrittivo $\rightarrow$ con riferimento a una struttura di **oggetto**
	oggetti hanno attributi (OSI X.500) e classi (OSI X.521)
	`dn`: cn=«Mauro Tortonesi», o=unife, c=it
	`telephoneNumber`: +39 0532 97 4888
	mail: mauro.tortonesi@unife.it
	`objectClass`: person
	`objectClass`: top

Il nome deve anche permettere di ritrovare l'entita' che denota, deve facilitare la ricerca.

## Names Services - Servizi di Nomi
- Un **servizio di nomi** memorizza i binding tra i nomi e gli attributi degli oggetti.
- Il principale scopo di un servizio di nomi e' di **risolverli** cioe' fornire ai Clienti gli attributi associati a un nome.
- La risoluzione fornisce gli attributi legati al nome.
- Un servizio di nomi puo' essere consultato implicitamente o esplicitamente.

- I Clienti del **name Server** sono:
	- **Clienti** che hanno esigenza di **risolvere un nome** per poter riferire una risorsa
	- **entità risorse** (Server rispetto ai clienti di prima, ossia che devono essere riferiti) che devono **registrare** il servizio e diventano Clienti del name Server
- **Comunicazione Clienti/name Server**
	- Si tendono a ottimizzare le operazioni piu' frequenti
	- I Clienti propongono la maggior parte del traffico
	- Le risorse da registrare fanno operazioni piu' rare e producono traffico piu' limitato

## Servizi di Directory e Discovery
- Un **servizio di nomi** recupera gli attributi associati a un nome.
- Un servizio di **directory/discovery** recupera le informazioni disponibili a partire da qualunque attributo (anche i nomi possono essere visti come attributi)
- Es.:
	- Name Service $\rightarrow$ elenco di telefono
	- Directory Service $\rightarrow$ pagine gialle
- Un servizio di **discovery** da accesso alle stesse informazioni di un directory service ma e' specifico per un ambiente locale dove i servizi e gli host possono cambiare molto dinamicamente. Ha specifiche, pensate per ambienti a elevata dinamicita' per registrare e deregistrare i servizi e per lookup.

**Directory**
- soluzioni di **nomi globali
- Cliente deve **conoscere** l'indirizzo della Directory
**Discovery**
- soluzioni di **nomi locali**
- Cliente recupera il nome del Discovery a runtime (**broadcast**). Non e' richiesta alcuna conoscenza a priori.

Un utente mobile vuole avere accesso a:
- **informazioni globali**, come:
	- descrizione dei dispositivi
	- delle preferenze proprie del suo profilo
	- delle sue firme digitali e PKI
	- delle sue sorgenti di informazioni, ecc.
- **informazioni locali**, come:
	- descrizione delle risorse disponibili localmente
	- dei gestori presenti
	- ecc.

Discovery e' fondamentale per arrivare a realizzare servizi location- e context-aware.

### Directory: esempi
- **OSI X.500.** $\rightarrow$ CCITT definisce X.500 come "una collezione di sistemi aperti che cooperano per mantenere un database logico di informazioni sugli oggetti del mondo reale. Gli utenti della Directory possono leggere o modificare l'informazione, o parte di essa, solo se hanno i privilegi necessari”
- **LDAP (Lightweight DAP)** $\rightarrow$ E' un importante directory, molto diffuso nel mondo Internet, e' basato su una variazione del Directory Access Protocol (DAP) specificato da X.500
- **Active Directory** $\rightarrow$ Microsoft propone **Active Directory** come un servizio di direttori integrato nel e per il sistema operativo (anche con interfaccia LDAP)

### I servizi di Discovery
Un servizio di discovery mantiene le associazioni `<nomi-attributi>` come un servizio di directory o di nomi, pero' memorizza gli attributi delle risorse locali perche' serve per facilitare l'accesso a tali risorse **senza conoscenza prestabilita**. Un Cliente usa comunicazioni broadcast (o multicast) per recuperare servizi locali.
Esempio $\rightarrow$ **JINI**, una delle prime soluzioni di discovery in ambiente Java.

## Multicast DNS (mDNS) e DNS-SD
Multicast DNS e' nato come estensione di DNS per **la risoluzione di nomi** dinamica in ambienti di rete locale (LAN).
Fa uso di:
- messaggi UDP
- porta 5353
- comunicazioni multicast sia per le richieste che per le risposte
- dominio riservato .local per motivi di sicurezza
- codifica UTF-8 per le stringhe di nomi
\
Uso di DNS-SD al di sopra di mDNS per il **discovery di servizi**

## Link Local Multicast Name Resolution (LLMNR)
LLMNR e' nato come risposta di Microsoft a mDNS.
Fa uso di:
- messaggi UDP
- porta 5355
- comunicazione multicast per le richieste, unicast per le risposte
- i nodi client devono anche mettersi in ascolto sulla porta TCP 5355

Uso di Simple Service Discovery Protocol (SSDP), un protocollo della suite UPnP basato su scelte ingegneristiche discutibili come il trasporto di messaggi HTTP su UDP, al di sopra di LLMNR per il discovery di servizi