
**==Sicurezza informatica: crittografia==**
3 tipi di crittografia moderna:
- chiave privata (simmetrici)
- chiave pubblica (asimmetrici)
- chiave omomorfica (in fase di sviluppo)

I sistemi crittografici sono formati quindi da chiave e algoritmo; dove la sicurezza dei sistemi crittografici e' data dalla segretezza della chiave e dalla segretezza dell'algoritmo.
Percio' l'algoritmo puo' essere noto, ma e' importante che venga aggiornato per renderlo il piu' robusto possibile, finche' l'attaccante non conosce la chiave non riesce a privarsene.
L'altro concetto fondamentale della crittografia e' la robustezza dell'algoritmo che e' determinato dalla robustezza della chiave.

I numeri di bit della chiave:
- chiave privata $\rightarrow$ circa 2040 bit
- chiave pubblica $\rightarrow$ 392-256 bit

Nessun sistema e' assolutamente sicuro; ma un sistema e' detto potenzialmente sicuro se tempo di fare la crittoanalisi, cioe' lo studio dei metodi per ottenere il significato di informazioni cifrate senza avere accesso all'informazione segreta che e' di solito richiesta per effettuare l'operazione.
Quindi se il tempo di fare la crittoanalisi e' maggiore del tempo di vita delle informazioni e se il costo della crittoanalisi e' maggiore al valore dell'informazione.

**chiave privata**: abbiamo il mittente e il destinatario che possono cifrare con la stessa chiave privata; oppure utilizzare due chiavi diverse ma comunque collegate tra di loro.
La crittografia a chiave privata puo' essere utilizzata per trasferire un messaggio su un canale sicuro; se viene intercettato richiede la condivisione chiave tra mittente e destinatario, quindi non puo' essere letto; 
per verificare l'integrita' del messaggio: MIC (Message Integrity Code), cioe' il checksum cifrato del messaggio. Puo' essere usato anche per l'autenticazione ma tramite la crittografia a chiave privata abbiamo il problema della condivisione della chiave. Questo perche' la chiave deve essere condivisa tramite un canale sicuro.
L'autenticazione, per verificare che i due soggetti comunichino tra loro due si manderanno tra le due parti messaggi di sfida a vicenda, perche' solo loro due sono capaci di decifrare la chiave privata.

**chiave pubblica**: abbiamo una chiave pubblica che e' di pubblico dominio.
Qui non abbiamo il problema della condivisione della chiave; la chiave pubblica viene distribuita attraverso questo dominio di chiave pubbliche (PKI - Public Key Infrastructure), pero' abbiamo un costo computazionale rispetto alla crittografia simmetrica; la chiave appartiene solo a quell'utente; l'utente cifra con la chiave pubblica, il destinatario decifra utilizzando quella chiave privata.
Questo tipo di crittografia puo' essere utilizzata sia per comunicare in canali sicuri, perche' il messaggio viene decifrato con la propria chiave pubblica; si possono immagazzinare i propri dati anche all'interno di un media, perche' le informazioni vengono cifrate con la chiave pubblica; puo' essere utilizzata anche per l'autenticazione: la chiave privata appartiene solamente all'utente, il mittente cifra con un messaggio di sfida la chiave pubblica in modo che solo quell'utente che ha la chiave pubblica puo' decifrare il messaggio.
E analogamente per il destinatario, il cui risponde cifrando la propria chiave privata e l'utente che ha inviato il messaggio per primo puo' ipotizzare la chiave del destinatario per verificare l'autenticita' del messaggio.
Ma puo' essere utilizzato per la paternita', come per esempio la firma digitale, visto che la chiave privata appartiene solo a quell'utente puo' firmare il messaggio utilizzando la sua chiave privata, e il mittente puo' verificare la sua autenticita' utilizzando la chiave pubblica.

---

==**La cifratura della chiave pubblica è significativamente più onerosa. Se lei ha un documento molto grande che vuole firmare digitalmente esiste una soluzione per velocizzare la procedura di firma?**==
Si, si puo' utilizzare la funzione di hash. Effettuo una funzione di hash del documento e firmare il documento sulla funzione di hash, appunto per ottimizzare la firma digitale.

---

==**cos'e' il web cashing?**==
E' una strategia che viene implementata soprattutto all'itnerno del web, cosi' quando un client va a richiedere una particolare risorsa non dovra' andare a dialogare direttamente con il web server, o application server che magari saranno carichi per altre cose; di conseguenza potra' anche andare a reperire questa risorsa in maniera piu' veloce.

Abbiamo 4 tipi di caching all'interno della rete:
- il **browser caching**, dove memorizza all'interno della cache le pagine digitate
- **Transparent proxy**, che fa da web cache per l'intera LAN del client; in questo caso vengono memorizzati i contenuti pubblici e questi contenuti vengono inviati al browser in risposta per evitare di fare un'altra richiesta al server.
- **Reverse proxy**, e' di fronte al server web che fa da web caching per l'intera application web. In questo caso vengono memorizzate le pagine pubbliche dinamiche per evitare il re-rendering di esse, o anche puo' fare la conversione di pagine pubbliche e statiche per ottimizzare i tempi di elaborazione e di risposta al progetto web.
- **Cache server**, dove vengono memorizzate pagine private dinamiche; in questo caso vengono salvati contenuti privati dove possono accedervi solo le persone autorizzate e quindi per evitare il re-rendering in porzioni di pagine private pubbliche vengono chieste frequentemente, la generazione puo' esser onerosa.

---

==**Remote procedure call:**==
Si occupa dei concetti convenienti per lo sviluppatore di applicazioni di rete. In particolare, fornisce dei servizi, noi possiamo definire dei servizi come delle funzioni che sono implementate lato server e che sono invocabili remotamente.
Quindi, io dal client con una sintassi, con una chiamata a funzione, posso attraversare questo stato di middle, che si chiama RPC, questa chiamata lato client viene catturata dal mio supporto RPC, vinee inviata al server, il server esegue la funzione che e' stata invocata e rimanda indietro al client la risposta che verra' restituita come il valore di ritorno della funzione che ha invocato il client.

---

==**Come si realizza RPC**==
Come avviene dietro le quinte? In realta' ho un Client che come abbiamo visto, chiama la funzione "moltiplica" localmente, e questa funzione moltiplica finge di essere la funzione moltiplica del Server.
Come fa a fingerlo? Lo fa grazie al fatto che nel lato client esiste una componente chiamata **stub** che ha il compito di fingere di essere la procedura server lato client.
Quindi, avrò uno **stub** specifico per ogni funzione che viene implementato nel lato server Quindi, se ho una funzione moltiplica, dividi, somma, sottrai $\rightarrow$ sono 4 funzioni e avrò uno stub per ognuna di queste funzioni.
Si ha uno stub lato Client e Server per ogni procedura.
Lo stub ha lo scopo di essere una componente proxy che finge di essere il Server. Gli fornisce al client un interfaccia, chiamo la funzione moltiplica (dove non sto chiamando la procedura server, ma sto chiamando lo stub del Client), lo stub prende i parametri chiamati dal Client e ha il compiuto di impacchettarli in un messaggio di chiesta, di trasmettere al di sopra di RPC run-time il messaggio di richiesta al Server.
Al lato Server l'RPC run-time dara' questo messaggio a uno **stub del server** (esiste per ogni procedura del Server) e ha il compito inverso dello **stub del client**, ovvero quello di prendere i messaggi di richiesta, di spacchettarli, trasformare i parametri e passarli alla funzione in un formato nativo alla funzione procedura che eseguira'.
La procedura ritornera' un risultato; il risultato lo passera' allo **stub del server**, che svolgera' la funzione opposta a quella svolta in precedenza, cioe' impacchettare il risultato della procedura del messaggio di risposta, intellegibile al client e che verra' inviato attraverso questo meccanismo di RPC run-time allo stub del client.
In questo caso, lo stub del client effettua lo spacchettamento di questo meccanismo di risposta, prende il valore di ritorno che e' contenuto, lo converte in un formato nativo e lo restituisce al client.

Questo meccanismo di RPC e' basato su un layer di base che permette l'invocazione di funzioni basandosi su scambi di messaggi e delle procedura stub lato Client e Server che si occupano della conversione tra un formato comune e il formato specifico dei messaggi dei parametri valori di ritorno.

---

==**Che cos'e' IDL?**==
Utilizzato per descrivere le interfacce degli oggetti e tipi di parametri, non e' un linguaggio di programmazione (non server per implementare gli oggetti o per realizzare client che accedano dagli oggetti).
- Oltre ai tipi di dati primitivi, per cui fornisce gia' routine di serializzazione e di deserializzazione, XDR permette di gestire tipi di dati complessi
- In XDR, il formato delle strutture dati e' definito attraverso un apposito linguaggio IDL (Interface Definition Language) simile al C
- Uso di *IDL compiler* per generare automaticamente partendo dalle procedure di codifica e decodifica dei dati complessi che sono noti dal sistema (leggere e scrivere dati complessi).

Sun/ONC RPC, esempio di IDL di una struttura complicata, dove definisco una costante, ho delle strutture dati che si chiamano `namenode` (e' un puntatore `namelist` della struttura IDL compiler che mi generera' automaticamente queste funzioni per dati nativi oppure complessi.). E' un implementazione C only, ma nonostante questo ho bisogno di un IDL che non e' il C.
L'intel del Server e l'intel del Client sono lo stesso? No perche' uno puo' essere a 32 bit e uno a 64 bit.

---

==**Semantica di chiamata**==

- **Semantica may-be**:
  E' la strategia piu' semplice. Questa strategia non si interessa delle politiche per quanto riguarda malfunzionamenti. Quindi sto implementando delle chiamate di procedura remota di tipo **may-be** (completamente best-effort).
  Se la chiamata fallisce me ne accorgo lato Client perche' scade un time-out, non posso lato Client sapere cosa e' successo.
  Quando mando la richiesta e non mi arriva il messaggio di risposta non so se la procedura e' stata eseguita o il Server e' crashato, in poche parole non so minimamente niente.
  Inoltre, se la chiamata ha successo, la procedura remota potrebbe essere stata eseguita anche piu' di una volta (Client invia solo una richiesta, ma ci possono essere messaggi duplicati dal servizio di comunicazione sottostante). Ricordiamo che RPC spesso viene usato sopra UDP ed esistono delle possibilita' che il pacchetto venga duplicato durante il canale di comunicazione. Semantica di semplice realizzazione ma solleva problemi di consistenza di stato sul Server, insicurezza sul Client se le procedure sono state invocate o no.

- **Semantica at-least-once**:
  Se il Client stub non riceve risposta scaduto il time-out riprovo a spedire il messaggio di richiesta un numero di N volte.
  Se la chiamata ha successo, la procedura e' stata eseguita una o piu' volte (richieste e messaggi duplicati), da cui at-least-once (almeno una volta).
  Se la chiamata fallisce il client puo' pensare a:
	- malfunzionamento permanente di rete
	- server crash
  Questo tipo di semantica puo' andare bene nel caso di **procedure idempotenti** (procedure che non hanno side effect), cioe' che possono essere eseguite molte volte con lo stesso effetto sullo stato di interazione C/S.
  Esempio, server stateless di NFS.77
  Se non ho procedure idempotenti utilizzo un altro tipo di semantica

- **Semantica at-most-once**:
  Si assegnano degli identificatori a tutte le richieste e una volta che e' stata eseguita lato Server esso si tiene traccia dell'identificativo e se riceva un successivo messaggio con lo stesso identificativo lo cestina/ignora. Questo perche' si evita di eseguire la stessa procedura.
  Nel caso il Server ottenesse un nuovo messaggio con la richiesta precedente, scoprirebbe che ha mandato il messaggio di risposta ma il Client non l'ha ricevuto, allora cosa fa?
  Il messaggio di risposta che si era salvato in una cash viene rimandato al Client finche' non riceve un acknowledge da parte del Client.
  Questo permette di ottenere una semantica di tipo at-most-once (al massimo una), in quanto e' garantito che in ogni caso la procedura remota e':
	- non eseguita
	- eseguita solo parzialmente
	- eseguita solo una volta

- **Semantica exactly-once**:
  Semantica difficile da realizzare.
  Il Server potrebbe implementare le procedure come transazioni atomiche, in modo che le procedure siano eseguite del tutto o per niente.
  Questo permette di ottenere una semantica di tipo exactly-once (esattamente una), in quanto e' garantito che in ogni caso la procedura e':
	- non eseguita
	- eseguita solo una volta
  
Questa semantica rappresenta il caso ideale, le implementazioni di RPC solitamente presentano semantica di tipo at-most-once

---

**==FTP attivo e passivo. Problemi della violazione del layering?==**
FTP (file transfer protocol) stabilisce un collegamento con una macchina remota per il trasferimento (upload e download) di file. Come per telnet, ci sono problemi di sicurezza, legati alla trasmissione in chiaro delle password. Possibile uso di FTP over SSL o di alternative sicure come scp (trasferimento di file su canale ssh).
- Vari programmi applicativi, da linea di comando con interfaccia grafica
- Protocollo di comunicazione TCP
- Protocollo ftp con comandi (4 caratteri ASCII a 7 bit) e risposte (numeri a 3 cifre)

Esempi di comandi protocollo ftp:
- STOR local-file, trasferisce un file locale sulla macchina remota
- RETR remote-file trasferisce un file remoto sul disco locale

Utente utilizza varie interfacce (ad es. linea comandi, con put (esegue STOR), get (esegue RETR), mget e mput, help, dir, ls, cd, lcd, …), ftp usa due connessioni per ogni collegamento client/server, una di CONTROLLO (su porta 21) e una di DATI (su porta 20). Per questo, in alcuni testi si dice che le informazioni di controllo sono trasmesse fuori out-of-band.
Il server mantiene lo stato della sessione del Client

**ftp attivo vs passivo**
- FTP in modalita' attiva lavora in questo modo:
	1. Il client inizia una connessione (generalmente sulla porta 21) verso il server e comunica la porta in cui si pone in ascolto (generalmente una porta random decisa dal client) per lo scambio dei dati
	2. Il server conferma la connessione (nel caso validano le credenziali)
	3. Il server apre una connessione verso il client sulla porta comunicata
	4. Il client conferma la connessione
	5. Inizia la trasmissione dei dati
- FTP in modalita' passiva lavora in questo modo:
	1. Il client inizia una connessione (generalmente sulla porta 21) verso il server
	2. Il server conferma la connessione (nel caso validano le credenziali) e comunica la porta in cui si pone in ascolto (generalmente una porta random in un preciso range impostato lato server) per lo scambio dei dati
	3. Il client apre una connessione verso il server sulla porta comunicata per la trasmissione dei dati
	4. Il server conferma la connessione
	5. Inizia il trasferimento dei dati

La differenza tra le dei modalita' di funzionamento del FTP sta nel fatto che la modalita' passiva, quando ben gestita consenta l'apertura e chiusura di porte casuali, riducendo i rischi legati a possibili attacchi. L'utilizzo di un FTP passivo risolve i problemi legati al firewall nei client, ma d'altro canto si dovrebbe lasciare aperte molte porte non privilegiate. Quasi tutti i server FTP consentono di impostare un range di porte in cui il server puo' mettersi in ascolto.

**Problemi di FTP**
FTP presenta numerosi problemi, al punto che potrebbe forse essere indicato come esempio di come ***NON*** progettare servizi Internet. 
In FTP attivo il Client comunica il proprio indirizzo IPv4 al Server come argomento del comando PORT, un errore di design davvero eclatante. Infatti, scrivere un indirizzo di livello 3 in un protocollo di livello 7 rappresenta una gravissima (!!!) violazione del principio di layering alla base del modello ISO/OSI con serie conseguenze per il protocollo applicativo. In seguito all’arrivo di IPv6, si è dovuto estendere il protocollo con due comandi (EPRT ed EPSV) che sostituissero i comandi PORT e PASV. Inoltre, la violazione del layering causa problemi nel caso (praticamente certo) in cui il Client si trovi in un ambiente di rete in cui è vengono usati indirizzi IPv4 privati e NAT, poiché il Server riceve dal Client un IP privato che ovviamente non è raggiungibile.

---

**==Voice over IP (VoIP)==** 
Si intende l’insieme dei protocolli di comunicazione di strato applicativo che rendono possibile tale tipo di comunicazione. VoIP consente una comunicazione audio-video sistema real-time,unicast o multicast, su rete a pacchetto.

---

**==Risoluzione mail exchange==**
Il funzionamento del sistema di posta elettronica dipende dal supporto offerto dalla risoluzione di nomi di tipo Mail eXchange (MX) del DNS.

In particolare, attraverso la risoluzione di nomi di tipo MX, il DNS permette a un server SMTP di scoprire i nomi logici dei Server SMTP di un determinato dominio (ad esempio il dominio di destinazione di un'e-mail).

Senza il supporto alla risoluzione MX, sarebbe impossibile scoprire quale sia il Server SMTP di un particolare dominio.

Si noti che i Server SMTP di un dominio sono tipicamente replicati. Piu' precisamente, abbiamo almeno 2 Server: 1 master e 1+ slave


All’interno del dominio pippo.com ho un utente che ha il proprio user agent pre-configurato dove si installa il proprio user agent, bisogna configurarlo per dirgli che bisogna utilizzare questo SMTP server del proprio provider, e anche IMAP server o POP3 server del proprio provider. 
**SMTP Server A, id mittente , come fa a sapere chi è l’SMTP Server? Qual’è il server che si occupa di svolgere il ruolo di server SMTP per il dominio paperino.com?**

Non lo puo' sapere, perche' l'unica cosa che sa e' il dominio in cui e' definita la casella elettronica destinataria @paperino.com.
Non c'e' nell'email alcun riferimento al nome del server SMTP a cui consegnare la posta, solo il nome della casella di posta, a non so il nome del server in cui questa casella si tiene.

Per smistare questa posta, l'SMTP server del mittente deve scoprire il nome dell'SMTP server del destinatario. Non c'e' un default.
Quindi, per scoprire qual'e' l'SMTP server lo fa attraverso un'apposita interrogazione DNS, con risoluzione di tipo `MailExchange` $\rightarrow$ indirizzo ipv4; il `mailexchange` non parte da un nome di host ma da un nome di dominio. Io mando una richiesta di nome di dominio; `mailexchange` paperino.com significa mi mandi indietro il nome di tutti i server SMTP, dove ce ne devono essere almeno due: master e un slave, del dominio paperino.com.

Mi arriva indietro il nome, ma prima di utilizzarlo devo fare una seconda risoluzione di tipo A o AAA, oppure entrambe, per avere gli Ip corrispondenti alla lista di questi server che mi sono stati restituiti.

Quindi, devo avere 2 tipi di risoluzioni: MX e A/AAA. Solo allora, dopo che ho i nomi e gli indirizzi Ip che posso connettermi e trasferire l’email dal mittente al destinatario. L’email viene messa nel database che è la casella di posta e il destinatario può accedere in maniera completamente asincrono rispetto alla procedura di trasmissione/invio della mail

---

**==Posta elettronica==**
Il servizio di posta elettronica si basa su:
- **User Agent (UA)**, Client che si interfacciano con l'utente
- **Mail Transfer Agent (MTA)**, i server di posta

E-mail parte da UA mittente e arriva a UA destinatario attraverso la rete dei MTA.

Utilizzo del protocollo di trasferimento **SMTP** (Simple Mail Transfer Protocol) per la consegna della posta alla casella del destinatario (modello push):
- Mail Transfer Agent usano SMTP
- User Agent in trasmissione, quando manda la propria posta manda al proprio server SMTP del proprio provider, in trasmissione utilizza SMTP per iniziare il processo rooting della mail connettendosi con il server del proprio Internet service provider e consegnando la mail a questo server SMTP che poi si sconnettera' all'utente, prendera' in carico l'email che provera' a consegnarla a destinazione per diversi giorni fino a quando non verifichera' che tutto e' andato bene.

Nel caso non riesce a inviarla a destinazione, a un certo punto mandera' un messaggio di errore all'utente.
Utilizzo di protocolli specifici, come POP3 o IMAP (o, ancora meglio, le loro versioni sicure, POP3s e IMPAPS) per la ricezione della posta, ovverosia il trasferimento della posta dalla casella di posta dell'utente allo UA che ne permette la consultazione (**modello pull**).

Il formato dei messaggi e' RFC 822, sono messaggi che sono formati da un header e un body.
Gli header contengono tutte le informazioni classiche di un messaggio di posta, che compongono l'intestazione; *from* (indirizzo mittente), *to* (mailbox destinatario, anche più destinatari) , *date*, *subject* (soggetto del messaggio), *cc* (copia destinatari).

---

==**MIME**==

Multipurpose Internet Mail Extensions e' un protocollo che estende RFC 822 per consentire l'invio dei dati multimediali (audio, immagini, video, documenti word, etc.)

MIME ha 3 parti fondamentali:
1. Righe di intestazione (integrano header di RFC 822) per definire il tipo di dato
2. Specifica del tipo di dato presente nella mail
3. Indica la codifica usata per i vari dati, in modo che possano essere trasmessi in un e-mail (che usa SOLO caratteri ASCII). Eventuali conversioni di formati binari ASCII (ad esempio, tramite codifica base64)

---

**==CDN==**
Content Delivery Network, serve per distribuire su grande scala il contenuto statico o che cambia raramente. Es. patch per un qualsiasi programma.

Sono funzioni che in automatico da un server centrale distribuiscono su copie localizzate di questo server centrali editati, in cui gli utenti possono accedere, e sono fisicamente vicini a loro; per evitare di scaricare grossi contenuti statici da un unico server centrale, che potrebbero essere anche da una parte del mondo lontano e avere di conseguenza un enorme latenza sul download.

---

**==E' automatica la connessione dell'utente?==**
Si e' gestito tutto in automatico, l'utente non deve decidere a quale server connettersi.

---

**==Mi parli della gestione dello stato tra un client e un server==** 
La gestione dello stato tra un client e un server puo' essere di 2 tipi:
- **stateless** $\rightarrow$ lo stato, l'interazione tra client e server non viene salvato sul server; di conseguenza ogni nuova richiesta non dipende dalla precedente
- **stateful** $\rightarrow$ salva lo stato d'interazione tra client e server; di conseguenza ogni nuova richiesta da parte del client dipende dalla precedent

$\Rightarrow$ non c'e' uno buono e uno cattivo, ma a seconda dell'applicazione che voglio posso usarne uno o l'altro.

**Nel web questo potrebbe avere altri problemi?**
Il web e' basato su un server HTTP che e' stateless $\rightarrow$ se voglio mantenere lo stato d'interazione tra client e server devo mantenerlo a livello applicativo nel mio application server.

**E come faccio agire questo stato?**
Lo faccio con i cookie $\rightarrow$ esistono soluzioni client side o server side.
Le soluzioni client-side e' una delle soluzioni piu' usate, cioe' i cookie. Dove viene creato questo token in base alla connessione, in cui viene fornito insieme ad ogni richiesta HTTP tra client e server per mantenere traccia di sessione tra client e server.

---

**==Architettura client-server web==**
Al giorno d'oggi il web si sta evolvendo e si e' passati dalle prime architetture del web client/server a delle architetture piu' complesse che permettono la gestione di pagine non solo statiche ma anche di pagine dinamiche attive.
Nasce cosi' un architettura detta **estesa** proprio perche' ci sono delle componenti aggiuntive.
- Il web browser che rappresenta il nostro client invia delle richieste a un server web. 
- Il server web ha il compito di rispondere a queste richieste attraverso delle pagine statiche, tutti i contenuti statici relativi anche a pagine dinamiche; una pagina dinamica puo' contenere dei contenuti statici (es. immagini). I contenuti statici sono all'interno di un file system nel quale un utente puo' caricare tutti questi contenuti statici attraverso ad esempio il servizio FTP.

Se il server web non riesce a rispondere a questa richiesta del browser perche' necessita, non solo di un pagina statica, ma bensi' di un contenuto dinamico, allora questa richiesta viene inoltrata a un application web che e' contenuta all'interno di un application server e fa parte del livello 2 che e' l' application tier.

Esistono 3 tier:
1. presentation tier
2. application server
3. data tier

Il data tier contiene un database la quale l' application web si servira' per prelevare i contenuti necessari alla generazione della pagina dinamica.

Esistono delle interfacce che permettono la condivisione tra un livello e l'altro.
L’interfaccia tra il web browser e il server web è un’interfaccia di tipo HTTP; L’interfaccia tra il database e l’application server è di tipo SQL. 
Queste due interfacce, sono interfacce fisse/prestabilite che non cambiano al variare dell’architettura. Mentre l'interfaccia tra il web server e l'application web e' un interfaccia che puo' variare a seconda dell'architettura stessa dell'application web.

Inizialmente si usavano le CGI, proprio perche' al web inizialmente serviva solo contenuti statici, quindi il server doveva eseguire solo dei semplici script.
Poi, con l’evoluzione del web si iniziano a creare dei veri e propri framework, ovvero delle piattaforme dove il server deve andare a inserire del codice per generare la pagina.

Quindi, si è preferito passare dalle CGI a un’interfaccia di tipo HTTP. Questo porterebbe a pensare di eliminare o spostare il server web dopo l’application web. Questo però è un errore in quanto: innanzitutto il server web serve per servire contenuti statici nell’immediato, quindi per creare una risposta più velocemente alla richiesta del browser ; poi l’implementazione del HTTP fatta dal server web è un’implementazione sicura, quindi il server web è stato implementato apposta per garantire questa sicurezza e l’application web invece no.

Questa architettura estesa è stata generata per creare appunto anche delle pagine dinamiche/attive.

Una pagina dinamica a differenza di una statica, non è identica ad ogni richiesta ma viene creata ad ogni momento dal server web in richiesta a una risposta del browser e può cambiare da richiesta a richiesta.

Le pagine attive si differenziano dalle pagine dinamiche, perché non vengono generate del tutto dal server bensì è il browser che si occupa di completare la richiesta; Quindi il compito del server è molto limitato rispetto alla creazione di pagine dinamiche o statiche, e c’è un lavoro più complesso da parte del browser, del client. Le pagine attive sono il soggetto principale del web 2.0, che è l’evoluzione del web tradizionale, dove non si ha più un web composto da siti, ma di servizi; si ha una distribuzione di contenuti che non vengono più solo forniti dal web verso l’utente ma c’è un’interazione completa fra l’utente e il web stesso.

---

**==Quali sono i vantaggi di avere un'architettura distribuita nel DNS? Perche' non abbiamo un database centralizzato ma lo abbiamo distribuito?==**
Non abbiamo un database centralizzato innanzitutto perche' ha un singolo point of failure, cioe' in caso di guasto andrebbe in crash tutto il sistema; se invece ho un sistema decentralizzato in caso di guasto di uno dei punti, il mio sistema puo' continuare a funzionare.

Inoltre, utilizziamo un sistema di tipo decentralizzato anche per migliorare le prestazioni, quindi servizio e richieste in modo piu' veloce.

Il DNS si basa su una struttura di tipo gerarchica nel quale abbiamo domini di alto livello e da questi dipenderanno dei successivi domini in modo appunto gerarchico, ma amministrativamente indipendenti.

---

**==Dal punto di vista della gestione amministrate invece?==**
Sono indipendenti tra loro pero' c'e' un concetto di delega. I domini di piu' alto livello delegano i domini di sotto livello.

---

**==Quali sono i vantaggi di un UTF-8 contro UTF-16 o un UTF-32?==**
Utf-8 è un encoding completamente compatibile con la codifica ASCII, proprio perché i primi caratteri dell’encoding corrispondono ai primi caratteri del codice ASCII. Questo permette una retrocompatibilità con il passato che era il punto fondamentale il quale si voleva arrivare per permettere un funzionamento più eterogeneo. Utf-8 utilizza un encoding a lunghezza variabile, come utf-16.

Mentre utf-32 utilizza un encoding a 4 byte fissi.

Utf-8 è lo standard principale per xml e json, e viene utilizzato per rendere i sistemi distribuiti eterogenei, infatti nel caso in cui abbiamo un sistema locale che utilizza un altro tipo di codifica viene effettuata la transcodifica, ovvero viene convertita la codifica locale in codifica utf-8, e viceversa dall’altra parte della comunicazione, rendendo la comunicazione eterogenea.

Utf-16 invece, è lo standard di Java e Window.

Utf-32, non viene più tanto utilizzato proprio perché ha una struttura complessa e non è compatibile e retro-compatibile con le altre codifiche.

---

**==Che cos'e' UTF-8==**
UTF-8 e' un protocollo testuale che rappresenta l'evoluzione del UTF-16 e UTF-32.
UTF-8 usa i gruppi di byte per rappresentare i caratteri unicode, ed e' particolarmente utile per il trasferimento tramite la posta elettronica a 8 bit.

UTF-8 e' di lunghezza variabile per natura; associa a ciascun carattere una sequenza di byte di lunghezza variabile che va da 1 a 4.
Dato che UTF-8 usa solo byte per rappresentazione i caratteri ASCII, riesco a utilizzare stringhe null-terminated.

**Vantaggi**:
- Il vantaggio piu' ovvio di qualsiasi codifica UTF e' che permette di rappresentare tutti i caratteri, a differenza di codifiche piu' vecchie.
- Alcuni caratteri Unicode (per esempio l'alfabeto latino) occupano in UTF-8 un solo byte, altri richiedono fino a 4 byte.
- Una sequenza di byte che codifica un carattere non puo' apparire come parte di una sequenza piu' lunga che codifica un altro carattere, come succedeva per codifiche a lunghezza variabile meno recenti.
- Il primo byte di una sequenza e' sufficiente a determinare la lunghezza. questo rende molto semplice estrarre una sotto-stringa da una stringa piu' lunga, senza bisogno di decodificare la sequenza di byte UTF-8.
- La maggior parte dei software esistenti sono stati scritti senza tenere conto di Unicode, perche' l'uso di questo andrebbe a creare problemi di compatibilita'. 
- UTF-8 e' la codifica predefinita per il formato XML.

**Svantaggi**:
- UTF-8 usa sequenze di lunghezza variabile, cioe' i singoli caratteri possono venire rappresentati con sequenze di byte di lunghezze diverse.

Le alternative: UTF-32 e UTF-16
- UTF-32 utilizza sempre sequenze di numeri a 32 bit, ovvero 4 byte. La semplicità della sua struttura migliora la leggibilità del formato. Nelle lingue che utilizzano principalmente l'alfabeto latino e quindi solo i primi 128 caratteri, questa codifica occupa molta più memoria del necessario (4 byte invece di 1). 
- UTF-16 si è affermato come formato di visualizzazione in sistemi operativi come Apple macOS e Microsoft Windows e viene utilizzato anche in molti framework di sviluppo software. È una delle codifiche UTF più vecchie ancora in uso. La sua struttura è particolarmente adatta per la codifica di caratteri in lingua non latina, perché occupa poco spazio in memoria. La maggior parte dei caratteri può essere rappresentata con 2 byte (16 bit). Solo in caso di caratteri rari la lunghezza può raddoppiare fino a 4 byte.

---

**==Quali sono le architetture dei principali firewall?==**
Abbiamo 3 tipologie di firewall principali:
- Packet filtering firewall
- Stateful inspection firewall
- Application proxy firewall (o application-level gateway)

Questi 3 tipi di firewall, poi possono essere applicati nell'architettura generando diversi tipi di architetture firewall.
- **screening router firewall**, nella quale abbiamo un router principale che attraverso un package filtering firewall filtra tutti i pacchetti che dall'esterno vogliano arrivare alla rete interna, e viceversa. Questa di solito e' una soluzione applicata per piccole reti, come quella di casa.
- **single baston** e' un architettura dove si ha un firewall posto tra la rete interna e la rete esterna, ed e' appunto una macchina che permette la divisione tra la rete esterna e quella interna garantendo la sicurezza tra le due parti.
- **double bastion**, permette di creare delle zone smilitarizzate all'interno dei quali abbiamo dei dispositivi sicuri in quanto sono protetti da dei firewall; tra questi dispositivi possiamo trovare anche l'application proxy firewall che a differenza degli altri permette di filtrare un determinato servizio.
- **distributed firewall** nel quale abbiamo due zone smilitarizzate, di cui una smilitarizzata interna e una esterna. Per quella esterna avremo applicazioni e servizi accessibili dall'esterno, mentre per quella interna avremo applicazioni e servizi accessibili dalla rete interna (intranet).

---

**==Che cos'e' la zona demilitarizzata?==**
E' una sottorete fisica o logica che contiene ed espone dei servizi ad una rete esterna non ritenuta sicura, come ad esempio Internet. Lo scopo di una zona demilitarizzata e' di proteggere la rete LAN di un'organizzazione.

I server Web potrebbero dover comunicare con un database per fornire alcuni servizi specializzati. Quindi bisognerebbe configurare una zona militarizzata altamente monitorata che comprende per lo piu' server Web e server simili che si interfacciano con il mondo esterno, cioe' Internet, in maniera tale da proteggere i server del database ponendoli in una zona separata dalla zona demilitarizzata dato che e' pubblicamente accessibile. Quindi puo' essere utilizzato un firewall dell'applicazione che funge da mezzo di comunicazione tra il server del database e il server web, cosi' da fornire un ulteriore livello di sicurezza, anche se e' piu' complesso. Questo tipo di configurazione ha pero' uno svantaggio dovuto al fatto che gli utenti della workstation non possono usare directory di rete e apportare modifiche direttamente a pagine Web o altri file e quindi costretti ad usare protocolli applicativi come FTP per caricare o scaricare le pagine.

---

**==Che cos'e' FTP?==**
FTP (File Transfer Protocol) stabilisce un collegamento con una macchina remota per il trasferimento (upload e download) di file. Si tratta di un protocollo basato sull'architettura client/server, ed e' un protocollo di un servizio a livello applicativo il quale si basa sulla connessione TCP. 
Inoltre, si basa su due comandi specifici:
- **storage** $\rightarrow$ serve per allocare in una macchina remota un file che abbiamo su un disco locale
- **retrieve** $\rightarrow$ serve per prelevare dalla macchina remota per poi inserirlo sul disco locale

Si basa sull'utilizzo di due canali di connessione:
- il canale **comandi o controllo** in cui vengono date tutte le informazioni apposta per instaurare una connessione per le azioni da svolgere (porta 21 della macchina server)
- La connessione di tipo **dati** in cui vengono trasferiti i dati, avvengono generalmente sulla porta 20, ma spesso non e' cosi' perche' l'implementazione FTP puo' essere fatta in due modi diversi

Innanzitutto bisogna specificare che il servizio FTP adotta un processo master che si mette in attesa di ricezioni di connessioni chiamate **FTP Demon**, cioe' un processo demone. Genera un processo **slave** per ogni connessione che si collega.

In base al sistema in cui opera, puo' esserci o un singolo processo che collega entrambe le connessioni (quella di controllo e quella di dati), oppure esiste un processo singolo per ogni connessione.

L'implementazione di FTP puo' essere di due tipi:
- **FTP attivo**
	- la macchina client si connette alla porta controllo della macchina server e gli fornisce il proprio indirizzo Ip insieme alla porta su cui si deve connettere. In modalita' attiva, il client FTP si connette alla porta 21 del server FTP da una porta non privilegiata a caso;
	- La porta di comando del client contatta la porta di comando del server e fornisce la sua porta dati
	- Il server fornisce un riconoscimento alla porta di comando del client
	- Il server stabilisce una connessione tra la sua porta dati e la porta dati del client
	- Alla fine il client Invia una conferma al server.
	- L'FTP attivo deve essere utilizzato quando il server FTP, che sta tentando di connettersi, non supporta connessioni FTP passive o se il server FTP si trova dietro un firewall / router / dispositivo NAT.
- **FTP passivo** $\rightarrow$ La modalita' FTP passiva e' stata sviluppata per risolvere i problemi di connessione della modalita' attiva. Questa e' la comunicazione tra il client FTP e il server in modalita' passiva.
	- Il client contatta la porta di comando del server e invia un comando PASV per indicare che questa e' una connessione passiva
	- Quindi il server da la sua porta dati di ascolto al client
	- Quindi il client effettua una connessione dati tra il server e se stessa utilizzando la porta specificata (la porta e' data dal server)
	- Alla fine il sever invia una conferma al client
	- L'FTP passivo dovrebbe essere utilizzato sempre a meno che non si sia verificato un errore o se la connessione FTP utilizza porte FTP non standard.

**Differenze tra FTP attivo e passivo**:
1. La modalita' attiva offre maggiore sicurezza al server FTP. Ma in modalita' passiva no. La modalita' passiva viene utilizzata quando le connessioni FTP sono bloccate dai firewall.
2. L'FTP attivo potrebbe causare problemi a causa dei firewall. Ma l'FTP passivo non ha problemi di connessione dei firewall
3. In modalita' attiva, il client stabilisce il canale di comando e il server stabilisce i dati, ma nell'FTP passivo, entrambe le connessioni vengono stabilite dal client.
4. La maggior parte della modalita' predefinita del browser web e' passiva. La modalita' attiva non viene utilizzata come modalita' predefinita di un browser.

---

**==Che cos'e' AJAX?==**
Ajax e' l'abbreviazione di Asynchronous JavaScript and XML.
E' un servizio per creare pagine attive interattive, basandosi su uno scambio di dati in background fra web browser e server, consentendo cosi' l'aggiornamento dinamico di una pagina web senza esplicito ricaricamento da parte dell'utente.

---

**==Che cos'e' una funzione di hash?==**
Sono delle funzioni ce servono per garantire una maggiore sicurezza nei documenti.
Sono delle particolari funzioni che prendono in input un messaggio di una qualsiasi lunghezza, arbitraria, e restituiscono un output di lunghezza fissa, molto piu' piccola rispetto a quella originiaria.
Una particolarita' delle funzioni di hash e' quella di inseguire questo calcolo in maniera velocissima e dato un messaggio di input risultera': se il messaggio e' sempre lo stesso il medesimo hash, ma se cambia anche solo di una piccolissima parte come ad esempio una virgola, l'hash del messaggio sara' notevolmente diverso.
Dall'hash e' quasi impossibile risalire al messaggio vero e proprio.
L'hash viene utilizzato in molti ambiti:
- Si puo' garantire l'integrita' del documento, perche' potrei hashare direttamente il documento e inviarlo; ed ecco che quando il ricevente lo ricevera' potra' tranquillamente hasharlo di nuovo e vedere se i due hash si concludono
- Possono essere riutilizzati anche per garantire una maggiore sicurezza; per esempio se vado a salvare dei dati sensibili su un supporto che non viene considerato sicuro; basti pensare alle password, quando ci logghiamo la password non viene condivisa direttamente dal server perché potrebbe essere troppo insicuro, ma si effettuerà l’hash e il server controllerà che i due hash coincidono, con quello salvato nel database e quello che è stato appena inviato.

---

**==Naming, DNS e Name Server==**
Dato che la rete remota e' molto vasta, bisogna utilizzare una procedura di **naming**, cioe' associare un nome logico a un server, un indirizzo che corrisponde alla localizzazione fisica all'interno di un server e una route che serve per localizzare la risorsa. 
Indirizzo fisico e' l'indirizzo IP.
Da un nome reperisco il sistema, ma c'e' anche un percorso per arrivarci.
Il nome specifica quale oggetto (entita') si riferisce, denota l'entita':
- nome logico a livello utente
- identificatore come nome (interno) definito dal sistema

L'indirizzo specifica dove l'oggetto risiede, la route specifica come raggiungere l'oggetto.
Questa associazione avviene tramite una funzione di corrispondenza, che risolve il problema di associare il nome logico alla risorsa.
La traduzione da nome a indirizzo e' come un hashtable, mapping.
Esistono due mappe fondamentali; la mappa fra indirizzo e percorsi che mi viene gia' data da IP, e' una mappa che devo consultare nella risoluzione del nome.
Dal nome ottengo l'indirizzo IP, e da esso riesco a utilizzare la risorsa.
**Binding**, associazione tra un nome e una risorsa.
Quindi, quando invochiamo una funzione (oriented programming, classi derivate, ecc.) e si ha un oggetto/riferimento a un oggetto com per esempio l'interfaccia, si chiama un metodo e esso viene risolto al momento della chiamata, e si va a vedere se e' associato alla classe base.
Due tipologie di binding:
- **binding statico**, puo' non essere ideale perche' mi associa a tutto il tempo di vita dell'applicazione, e' il binding che viene effettuato una sola volta.
- **binding dinamico**, viene effettuato ogni volta per aggiornare le risorse, e questo mi da la possibilita' di conoscere quali sono le risorse che sono disponibili e quali no. In costanti dinamiche si utilizza un binding dinamico, che porta maggiori performance.

A livello ideale, il binding dinamico bisognerebbe farlo ogni volta, ma questo non accade perche' si possono avere problemi computazionali, cioe' si avrebbe troppa elaborazione e quindi viene effettuato a intervalli di tempo.
Per riuscire a mantenere in memoria questi binding c'e' bisogno di utilizzare un database, ma utilizzare un database unico e centrale non e' per nulla efficiente e si e' a rischio di single point failure, quindi non posso piu' accedere alle mie risorse, c'e' una difficolta' di consultazione del mio database e il problema piu' grosso e' quello di malfunzionamenti, quindi il problema piu' grosso e' la manutenzione.

La soluzione a questo è l’utilizzo del **DNS** (Domain Name System), prevede un sistema decentralizzato dei nomi, infatti viene utilizzato per assegnare nomi ai nodi della rete(host). 
Il servizio è realizzato attraverso un database distribuito, costruito dai server DNS. La radice nominata ci sono solo le informazioni dei top level domains, sono cosiddetti livelli di 1 livello che sono gestiti da grandi organizzazione.
Ognuno dei domini e sotto domini ha almeno 2 server che hanno tutte le informazioni che appartengono a quel dominio; in ogni nodo del grafo ci sono le sue informazioni, che sono le informazioni dei domini sottostanti, nomi e Ip dei server dei sottodomini. al terzo livello ci sono le organizzazioni vere e proprie Ogni dominio può essere **relativo** o **assoluto**.
Ogni dominio deve fare riferimento al dominio che lo contiene. Esiste il concetto di delega: un dominio delega i sottodomini a gestori sottostanti (che se ne assumono responsabilità e autorità). 
es. Se il DNS di Facebook non funzionava era loro autorità sistemarlo.

All'interno del DNS abbiamo diversi **record type**, come per esempio:
- **A**, per la risoluzione da nome host a indirizzo IPv4
- **AAAA**, per la risoluzione da nome host a indirizzo IPv6
- **MX**, per risoluzione da nome dominio a nome del server di posta elettronica.

Il DNS che e' un protocollo di trasporto, usa un semplice protocollo richiesta-risposta basato su UDP. UDP viene scelto per le seguenti ragioni:
- non e' necessaria affidabilita' $\rightarrow$ questo perche' il protocollo e' talmente semplice che e' facile realizzare una forma di affidabilita' a livello applicativo
- non e' necessario il **congestion control** $\rightarrow$ questo perche' il protocollo occupa un traffico talmente limitato che e' molto improbabile che esso diventi una causa di congestioni
- e' importante minimizzare la latenza nella risoluzione dei nomi $\rightarrow$ quindi il setup di una connessione TCP rallenterebbe eccessivamente le procedure di risoluzione dei nomi.

---

**==Il concetto di organizzazione gerarchica e di delega, fornisce dei vantaggi da un punto di vista amministrativo?==**
Si. I DNS implementano uno spazio gerarchico dei nomi, per permettere che parti di uno spazio dei nomi, conosciute come "zone" possano essere delegate da un **name server** ad un altro name server che si trova piu' in basso nella gerarchia.
Una "zona" DNS e' una parte dello spazio dei nomi, costituita da un dominio e i suoi sottodomini che non solo a loro volta delegati, che e' sotto una stessa gestione amministrativa e quindi e' gestita da uno o piu' server. Per ragioni di ridondanza, ciascuna zona e' "replicata" su piu' server, e di conseguenza la delega e' costituita da piu' record name server, che indicano ciascuno dei server indicati contiene le informazioni per quella zona.

---

**==Architettura delle applicazioni distribuite, modello client/server==**
Le applicazioni distribuite prevedono un'interazione tra due elementi: mittente e destinatario.
Rispetto a questo tipo di comunicazione, ci sono due caratteristiche che rappresentano diverse tipologie che sono:
- disegnazione dei processi
- sincronizzazione dei processi 

Per quanto riguarda la designazione dei processi, si puo' parlare di:
- **schemi diretti simmetrici** $\rightarrow$ prevedono che si nomino esplicitamente la vicenda e di conseguenza nelle send e receive vengono nominate esplicitamente anche rispettivamente il mittente e destinatario
- **schemi diretti asimmetrici** $\rightarrow$ il mittente nomina esplicitamente il destinatario, ma il destinatario non specifica da quali mittenti ricevera' la comunicazione. Quindi tiene aperta una linea di comunicazione generica su cui si agganceranno alla porta i vari mittenti che richiedono il servizio
- **schemi indiretti** $\rightarrow$ prevedono che la riconciliazione non avvenga direttamente tra il mittente e il destinatario, ma ci sia un soggetto intermedio che si chiama **mailbox** che fa da intermediario nella comunicazione. Quindi il mittente non richiede la comunicazione direttamente al destinatario ma deposita la sua richiesta all'interno della mailbox e il destinatario non comunica direttamente con il mittente ma verifica periodicamente se nella mailbox si sono depositate richieste.
  E questo avviene anche nel caso delle risposte, il destinatario deposita l'esito dell'operazione direttamente nella mailbox dove verra' verificata dal cliente.

---

**==Secondo lei, questo tipo di comunicazione tramite mailbox in che occasioni potrebbe essere utile?==**

Puo' essere per motivi di sicurezza, in cui non e' opportuno tenere aperta una comunicazione diretta oppure anche nel caso di comunicazioni asincrone in cui e' opportuno che il mittente o il destinatario o entrambi continuino a fare le loro operazioni, quindi eseguire altri processi e non siano costantemente in attesa dell'interazione con l'altra parte.

A livello di sincronizzazione, due processi possono comunicare in due modalita':
- La modalita' **sincrona**, prevede che i processi siano bloccati, ovvero quando il mittente manda una richiesta al destinatario, il mittente rimane bloccato finche' la richiesta non ha voluto una conclusione che gli e' stata rimandata, cioe' di successo fallimento.
- La modalita' **asincrona**, il mittente e il destinatario continuano ad eseguire le loro operazioni e non rimangono bloccati in attesa dell'operazione.
  La modalita' asincrona e' piu' semplice da realizzare a livello di programmazione, pero' presenta dei problemi per quanto riguarda la consistenza del server, perche' appunto non si puo' avere la garanzia che la comunicazione sia andata a buon fine, soltanto guardando lo stato del mittente e del destinatario.

Nel caso in cui ci siano server occupati con un numero elevato di operazioni che devono essere eseguite, la programmazione si complica ancora di piu' in quando bisogna introdurre un **buffer** (array che prevede la memorizzazione sequenziale delle richieste che vengono memorizzate a livello temporaneo).

Le applicazioni distribuite possono essere:
- **con connessione** $\rightarrow$ la connessione viene stabilita fin da subito tra mittente e destinatario
- **senza connessione** $\rightarrow$ la programmazione diventa piu' onerosa a livello di dati, in quanto ogni pacchetto di informazione deve contenere oltre al overhead anche l'indirizzo del destinatario..

La comunicazione puo' essere:
- **Affidabile** $\rightarrow$ nel caso in cui il supporto garantisca la consegna dei messaggi. In caso di perdita di messaggi o guasti, il supporto tipicamente ritrasmette (anche piu' volte) il messaggio. Ovviamente non puo' risolvere guasti permanenti (per esempio di rete). Alto costo di realizzazione (basse prestazioni) vs. semplicita' di programmazione
- **Non Affidabile** $\rightarrow$ In questo caso il supporto invia i messaggi senza verificarne la consegna. Alte prestazioni vs. difficolta' di programmazione.
  
Un esempio di queste due comunicazioni e' la differenza tra TCP e UDP.

Per quanto riguarda il modello client/server, e' un evoluzione di questo modello generico, dove il client fa da mittente, quindi e' la macchina che chiede al server di eseguire una richiesta. Invece il server e' una macchina centralizzata che si occupa di eseguire dei compiti specifici.
Lo schema di comunicazione e' **molti a uno**, questo perche' vari client possono comunicare con lo stesso server.
La programmazione del server e' molto piu' complicata rispetto a quella del client perche' deve poter gestire molte connessioni, e soprattutto il server deve essere avviato all'avvio della macchina.

La comunicazione tra client e server deve avvenire attraverso il modello pull o il modello push.
- **Modello pull** $\rightarrow$ il client richiede l'informazione al Server e in genere si blocca e aspetta la risposta (simile a chiamata di procedura). Modello piu' utilizzato. Quindi di conseguenza e' una modalita' sincrona.
- **Modello push** $\rightarrow$ il client segnala il proprio interesse e poi fa altro. E' compito del Server di inviare l'informazione se e quando disponibile. Il Server effettua l'invio (push) delle informazioni (applicazioni della posta) $\rightarrow$ modello asincrono.
  Il modello push puo' anche essere emulato da un'operazione di polling non bloccante lato Client. Succede che il Client ogni tot secondi richiede al Server servizio fino all'ottenimento della risposta. 

Il modello pull e' Server-driven e asincrono (bloccante); il modello polling e' Client-driven e asincrono (non bloccante); il modello push e' Server-driven e asincrono.

Per quanto riguarda le richieste multiple di servizio l'interazione tra client e server puo' essere **stateful o stateless**.

---

**==Cosa sono JSON e XML==**
Sono due linguaggi:
**JSON** 
- viene utilizzato per archiviare le informazioni in modo organizzato e di facile accesso. La sua forma completa e' JavaScript Object Notation, ed offre una raccolta di dati leggibili dall'uomo a cui e' possibile accedere logicamente.
- E' un formato di rappresentazione dei dati particolarmente leggero e molto utilizzato sul Web
- Molto piu' compatto e significativamente piu' performante e facile da processare rispetto a XML
- Pensato per applicazioni Web 2.0 e per la manipolazione dei dati in JavaScript
- Compatibile con hash in linguaggio JavaScript

**XML**
- E' un linguaggio di marcatura progettato per memorizzare i dati. 
- E' comunemente usato per il trasferimento dei dati
- E' un linguaggio di descrizione specializzabile per settori specifici
- Standardizzato da W3C, e' molto utilizzato anche fuori dal web
- Tecnologia sviluppata in ottica machine-oriented, per facilitare la generazione automatica di codice che valida e/o manipola tipi di dati strutturati rigorosamente definiti.
- XML e' usato sia per la rappresentazione di dati e messaggi scambiati che per la definizione del loro formato

---

**==Protocollo testuale: possono esserci problemi di visualizzazione con utf-8, in quali casi potremmo avere questo problema?==**
Poiche' in UTF-8 i caratteri hanno dimensione variabile, si deve fare particolare attenzione a verificare che un buffer di memoria non contenga dei caratteri incompleti prima di utilizzarlo, quindi prima li devo validare.

Con una `write()` da un file o da una comunicazione tra processi (IPC - Inter process Communication) potrei leggere in un buffer solo una parte di una stringa UTF-8, che non contiene tutti i byte che codificano l'ultimo carattere ricevuto.

Oltre a verificare che un buffer non contenga caratteri incompleti bisogna verificare che i dati rappresentati siano effettivamente validi. Devo passare per una libreria di validazione.

Il C non si e' allineato a UTF-8; supporta delle codifiche multibyte.

**Transcodifica**
- In Java si usano le funzioni di transcodifica fornite da `InputStreamReader` e `OutputStreamReader` (il secondo parametro del costruttore specifica la codifica "esterna"), classi fondamentali per la codifica
- In Unix/C lo standard e' rappresentato dalla funzione `iconv` fornita dalle librerie di sistema, che pero' e' piuttosto difficile da utilizzare, si consiglia di utilizzare librerie piu' semplici come:
	- `utf8proc`
	- `libunistring` $\rightarrow$ piu' semplice
	- `glib`

---

**==Che cos'e' NAT?==**
NAT e' una soluzione che trasforma automaticamente indirizzi privati in indirizzi globali. Ed e' l'unica cosa che fa andare in piedi l'internet IPv4.
Ha la possibilita' di connettere a Internet reti con un numero elevato di indirizzi privati utilizzando un numero relativamente basso di indirizzi pubblici.
Il suo funzionamento e' che ho una rete all'interno della quale uso degli indirizzi privati, e nel router a confine di questa rete, il gateway adsl di casa nostra ad esempio, ho un processo NAT che e' in esecuzione sul mio router. Esso sa che c'e' una rete interna ed esterna, su quella interna ci sono degli indirizzi privati che nono possono comunicare con la rete esterna. Quindi il router connesso alla rete esterna ha un indirizzo IP pubblico e modifica tutti i pacchetti che arrivano da indirizzi IP privati verso l'esterno e modifica l'indirizzo sorgente e porta, fingendo di aver inviato lui i pacchetti. Dunque mette il proprio indirizzo IP e un numero di porta finta sopra a questi pacchetti, e quando gli ritorneranno indietro prendera' l'indirizzo di destinazione, lo modifichera' e lo mandera' all'interno.

**Svantaggi**:
- Problemi nel caso si voglia rendere accessibili all'esterno servizi che girano nella rete con indirizzi privati
	- Necessita' di configurazioni ad hoc per server
	- Uso di protocolli per consentire al traffico esterno di passare attraverso NAT (STUN, TURN, ICE, ecc.)
- Problemi con alcuni tipi di servizi. Ad esempio, per stabilire FTP attivo e' necessario un NAT stateful con uno specifico modulo di protocollo translation.

Nella internet moderna, esistono molti strumenti, denominati "middleboxes", che, come NAT, manipolano il traffico per vari motivi e possono rappresentare un problema per le applicazioni. Cioe' questi middleboxes assumono che possono passare solo traffico UDP e TCP, non potendo utilizzare altri protocolli diversi come SCTP.

---

**==Dinamicita' pagine web==**
Le prime pagine apparse sul Web erano scritte in HTML ed erano statiche e immutabili.
Che cosa succedeva? Praticamente c'era un web master che utilizzava un editor HTML per scrivere una pagina in HTML. Il web master generava/gestiva sulla propria workstation locale il proprio sito, esportava l'HTML e lo caricava sul web server, il web server prendeva l'HTML generato e lo caricava tipicamente attraverso FTTP.
Quindi, si utilizzava FTTP per caricare questi documenti HTML generati da form page e ogni volta che cambiava qualcosa bisognava rigenerare gli HTML e ricaricarli sul web attraverso FTTP.
Sul web server si trovano semplicemente delle **pagine statiche**, che erano rappresentate da dei file sul disco fisso, sul file system sul server. Il server era assolutamente semplicissimo: prendeva una richiesta e le richieste che si potevano avere erano "mandami un file HTML o un file CSS".
Una volta effettuato il parsing della richiesta, e una volta determinato il percorso all'interno del file system del file HTML che deve servire; servire il file HTML e' una riga di codice: esiste una system call che si chiama `sendfile` che prende tutto il contenuto del file e lo mette in una socket.

Evoluzione del Web verso una maggiore dinamicita'.
Per i siti web in cui si hanno delle pagine che cambiano velocemente non si possono utilizzare pagine statiche ma dinamiche.

Tipi di pagine:
- **Pagine statiche** $\rightarrow$ fanno parte del web1.0, sono documenti web il cui contenuto e' definito al momento della loro creazione non cambia piu'. Qualunque richiesta di pagina statica fornisce sempre la stessa risposta.
- **Pagine dinamiche** $\rightarrow$ sono documenti Web che vengono creati dal Web Server solo quando un browser li richiede. In tale caso il Server Web attiva un'applicazione che crea il documento in modo dinamico e lo invia al browser. Il contenuto di un documento dinamico puo' variare di richiesta in richiesta.
- **Pagine attive** $\rightarrow$ fa parte del web2.0, una pagina attiva ha un contenuto che NON e' completamente specificato dal Server. E' un pezzo di programma che viene mandato dal Server al browser, dove viene eseguito localmente. Una pagina attiva puo' interagire con l'utente e con altre risorse ed entita', per cui il suo contenuto non e' mai statico.

Vantaggi/svantaggi pagine statiche:
- Vantaggi:
	- semplicita' (della pagina, server) che si riflette anche in performance (la facilita' di effettuare il cache di queste informazioni); e affidabilita'
	- Estrema performance, le pagine dinamiche possono essere servite lato server (una system call) oppure attraverso una cache
	- Affidabilita', perche' un minimo di componenti e' essenziale, quindi server semplicissimo, browser semplicissimo e quindi massima affidabilita'
- Svantaggi:
	- pagine poco flessibili, per ogni cambiamento il web master deve cambiare in locale e ricaricarle sul server.

Vantaggi/svantaggi pagine dinamiche:
- Vantaggi:
	- adatte per informazioni con elevata frequenza di cambiamento
	- nonostante queste pagine dinamiche siano date per creare un sacco di applicazioni, per il browser sono come delle pagine statiche; quindi non ho bisogno di un browser sofisticato
- Svantaggi:
	- ho una computazione sul server che richiede server piu' potenti;
	- ho delle prestazioni che sono piu' basse rispetto alle pagine statiche, perche' ho un tempo di elaborazione e in molti casi non posso effettuare il caching delle pagine dinamiche;
	- ho bisogno di un programmatore con delle buone capacita' perche' realizzare delle applicazioni web basate sulle pagine dinamiche e' molto difficile, non in quanto realizzare delle applicazioni web basate sulle pagine attive, ma comunque non e' una passeggiata.
	- un'iterazione tra l'utente e la pagina e' limitata (principale svantaggio), e' del tipo "clicca e aspetta"; quindi l'idea sarebbe quella di generare le pagine in fretta, perche' questo "clicca e aspetta" distrugge la user experience

Vantaggi/svantaggi pagine attive:
- Vantaggi:
	- l'interazione con l'utente e' continua
	- ho un minor carico sul server perche' la computazione non e' tutta sul server ma e' principalmente sul client; si effettua un flooding della computazione dal server al client.
	- pagine complesse ma esistono modelli di programmazione innovativi e interessanti
- Svantaggi:
	- c'e' bisogno di browser piu' sofisticati e di dispositivi client piu' potenti
	- nonostante la qualita' dei browser queste pagine sono di una complessita' veramente alta
	- richiedono ottime capacita' di programmazione
	- problemi di portabilita' (coinvolgono tutti i client)
	- problemi di sicurezza (eseguono sui client), se ho degli utenti malintenzionati posso rompere la mia applicazione
	- soluzione di elevatissima complessita'

---

**==Telnet e rlogin==**
Servizi di terminale remoto.
Telnet e rlogin permettono il collegamento con macchina remota.
Funziona che noi dalla macchina locale ci colleghiamo alla macchina remota e vediamo l'interfaccia locale trasformato in un terminale del sistema remoto.
Quindi il terminale locale diventa un terminale del sistema remoto: a noi puo' sembrare di lavorare localmente ma in realta' stiamo lavorando da remoto.

**telnet** standard in TCP/IP (Internet) (gestisce eterogeneita' di S.O., HW, etc.)

**rlogin** per i sistemi UNIX (remote login), in sistemi UNIX c'e' il concetto di terminale interno al sistema operativo; nel kernel ci sono sistemi di gestione unix, significa che ci sono strutture/concetti per supportare il terminale. Questo permette di dare per scontato che tante funzionalita' sono dentro al sistema operativo e non devo implementarle (molto piu' semplice).

I programmi applicativi telnet e rlogin hanno interfaccia verso utente da linea di comando (sono comandi in Unix).
Il protocollo di comunicazione e' TCP/IP

**telnet**:
- Una possibile implementazione del telnet su una macchina Unix in cui ha il concetto di pseudo terminale, puo' essere quello in cui il demone telnet crea una shell di login. Su di essa possiamo lanciare dei vari comandi dell'applicazione
- La shell di login e' agganciato al telnet attraverso il pseudo terminale che puo' essere una funzione del sistema operativo come in rlogin oppure e' una funzione del pseudo terminale che deve essere fornito dal Server telnet.
- **Client** stabilisce una connessione TCP con Server (porta 23), quindi accetta caratteri da utente e li manda al server e contemporaneamente accetta caratteri dal server e li visualizza sul terminale dell'utente.
- **Server** accetta la richiesta di connessione dal Client e inoltra i dati da connessione TCP al sistema locale.

**Caratteristiche principali del protocollo applicativo di telnet**:
- Gestione eterogeneita' tramite interfaccia standard (NVT)
- Client e Server negoziano le opzioni del collegamento (ASCII a 7 bit o a 8 bit)
- comunicazione simmetrica

**Caratteristiche rlogin**:
- conosce l'ambiente di partenza e quello di arrivo, ha notizie di `stdin`, `stdout` e `stderr` (collegati al client mediante TCP)
- utilizzo di una sola connessione TCP/IP
- esporta l'ambiente del client verso il server

**flow control**: il cliente rlogin tratta localmente i caratteri di controllo del terminale

**out-of-band** signaling per i comandi dal server al client. implementato con TCP urgent mode

**in-band** signaling per i comandi dal client al server (spedizione dimensione finestra)

---

==**xmpp**==
XMPP e' un protocollo che permette la comunicazione real-time di messaggi e informazioni di stato (assente, occupato, etc.).
I client si registrano presso server locali.
Routing di messaggi e architettura simile a SMTP,

XMPP e' un protocollo di comunicazione basato su XML (Extensible Markup Language). Il set standard XML codifica o traduce i documenti informatici in formato leggibile dalla macchina. XMPP e' utilizzato per MOM (Messagge-oriented middleware), che sono programmi software progettati per l'invio e la ricezione di messaggi tra sistemi diversi.

---

==**TCP e UDP**==
La User Datagram Protocol (UDP) e' uno dei protocolli di base utilizzati per la trasmissione di dati su Internet, l'altro e' il Transmission Control Protocol (TCP).

**Velocita'**
UDP in media puo' essere un protocollo molto piu' veloce per la trasmissione di informazioni rispetto a TCP perche' utilizza un totale di 8 byte per pacchetto di dati da trasmettere rispetto a TCP che ne usa 20 (?).

**Senza connessione**
Questa intestazione piu' snella viene perche' UDP e', a differenza di TCP, senza connessione. Con il protocollo TCP, e' garantito che i dati arrivino nell'ordine corretto ed e' controllato per errori all'arrivo, mentre UDP non fornisce questa protezione, i pacchetti possono essere persi o danneggiati.

**Usi**
Mentre il protocollo TCP viene utilizzato per la navigazione web, UDP e' piu' spesso utilizzato per Voice over IP e giochi, dove la velocita' e' assolutamente fondamentale e i piccoli errori che possono apparire in UDP non sono cosi' importanti.

---

**==Perche' si usano solo TCP e UDP in internet==**
TCP e UDP sono protocolli utilizzati per l'invio di bit di dati, noti come pacchetti, su internet.
Essi sono sopra il protocollo internet IP quindi, se sis ta inviando un pacchetto tramite UDP o TCP, quel pacchetto viene inviato sicuramente a un indirizzo IP.

TCP e UDP non sono i soli protocolli che lavorano su IP, tuttavia sono quelli piu' ampiamente utilizzati.

**TCP** e' acronimo di **Transmission Control Protocol** ed e' il protocollo piu' comunemente usato su Internet.
Quando si carica una pagina web, il computer invia pacchetti al TCP all'indirizzo del server web, chiedendo di farci vedere quella pagina web per voi.
Il web server risponde inviando un flusso di pacchetti TCP, che il browser web mette insieme per formare la pagina web e mostrarla sullo schermo.
Quando si clicca il link, si accede a un sito o si invia un commento, il browser invia pacchetti TCP al server e il sever risponde con altri pacchetti TCP.
Il protocollo TCP garantisce che il destinatario riceva i pacchetti. 
Il destinatario (ad esempio il web server) invia la conferma di ricezione al mittente (il nostro computer). 
Se il mittente non riceve conferma, rispedisce i pacchetti, e smette solo dopo un certo periodo di tempo se il destinatario non risponde perché offline. 
I pacchetti vengono inoltre controllati per eventuali errori. 
Il TCP è molto affidabile e i pacchetti sono tracciati in modo che nessun dato venga perso o danneggiato durante il transito.
Questo è il motivo per cui i download di file non vengono danneggiati anche se si utilizza una rete lenta o che si interrompe spesso. 

**UDP** è l'acronimo di **User Datagram Protocol**. Un datagramma è uguale a un pacchetto di informazioni quindi il protocollo UDP funziona in modo simile a quello TCP, con una differenza, non controlla gli errori. 
Quando si utilizza UDP, i pacchetti vengono inviati al destinatario velocemente senza attendere e senza assicurarsi che il destinatario li abbia ricevuti, continuando a inviare pacchetti. 
Se il destinatario perdesse alcuni pacchetti UDP, non ha alcun modo di chiederli di nuovo. 
In pratica una comunicazione UDP non dà alcuna garanzia di ricezione dei dati.
Il vantaggio è che i computer possono comunicare tra loro più rapidamente.
UDP viene utilizzato quando la velocità di rete è elevata e può essere superflue il controllo di errori. 
Ad esempio, UDP è spesso utilizzato per li video in diretta in streaming e per i giochi online. 
Un video in streaming in diretta è un flusso di dati continuo che viene inviato al computer. 
Se si perde qualche fotogramma, esso viene saltato e di certo non sarà possibile chiedere di vederlo dopo. 
I flussi di streaming UDP si differenziano rispetto a quelli TCP proprio perché i pezzi di video non ricevuti vengono saltati. 
Se si perde la connessione per alcuni secondi, il video si blocca per un attimo e poi passa al punto di ripresa saltando i pacchetti persi. 
Se si verifica una minore perdita di pacchetti, il video o l'audio possono essere distorti per qualche istante e tornare buoni subito dopo. 
Con i giochi online la storia è simile, se si perde qualche pacchetto UDP, i giocatori passano da un punto a un altro senza che si veda movimento. 
Quello che conta è rimanere attuali nel gioco, senza guardare al passato e a ciò che eventualmente è stato perso. Saltando la correzione di errori che farebbe TCP, si velocizza la connessione del gioco e si riduce la latenza.

**Conclusione** 
Se un'applicazione utilizza il protocollo TCP o UDP dipende dal suo sviluppatore e non si può cambiare.
La maggior parte dei programmi vogliono la correzione degli errori e preferiscono la robustezza del protocollo TCP, mentre alcune applicazioni hanno bisogno di velocità e si affidano a UDP. 
Con un programma come Wireshark si possono vedere i vari pacchetti che viaggiano avanti e indietro sul computer. 
Se si sta configurando un router o un firewall per aprire certe porta, se non si è sicuri se un'applicazione utilizza TCP o UDP, si può scegliere di aprire "entrambi" per applicare la stessa regola sia al traffico TCP e UDP.

---

**==Differenza tra IPv4 e IPv6==**
Un indirizzo IPv4 e' composto da 32 bit e suddiviso in una parte che identifica la Local Area Network (LAN) in cui esso si trova (Network ID) e in una parte di host (host ID) che identifica il particolare nodo all'interno di quella rete.

La lunghezza del network ID e' variabile e quindi permette il subnetting, cioe' posso creare delle sottoreti all'interno di una rete un po' piu' grande assegnandoli alla sottorete una porzione del network ID piu' lunga rispetto a quello della rete principale.

Rappresentazione di indirizzi IPv4 segue la cosiddetta “dotted notation”: 192.168.0.1/24
E’ rappresentato da indirizzi da 4 byte, ogni byte va da 0 a 255 partendo dal byte più significativo a sinistra. /24 identifica il numero di bit che compongono la network ID. In questo i 24 bit sono i primi 3 byte cioè 192.168.0 e rappresentano il codice della LAN dove si trova il mio nodo ed ha come host 1 all’interno della LAN.

Il numero massimo di dispositivi con indirizzo IPv4 e' di circa 4 miliardi ($2^{32}$). In realta' non tutti gli indirizzi sono utilizzabili, visto che alcuni indirizzi (~18 milioni) sono riservati per l'uso di in reti private:
- 10.0.0.0/8 (255.0.0.0)
- 172.16.0.0/12 (255.240.0.0)
- 192.168.0.0/16 (255.255.0.0)

In piu' altri indirizzi sono riservati per multicast, routing. In generale c'e' limite massimo di efficienza di assegnazione degli indirizzi.

---

**==Problemi di IPv4==**
IPv4 ha un numero molto limitato di indirizzi e sono esauriti ovunque. L'esaurimento degli indirizzi significa che il local registry non e' piu' in grado di assegnare degli indirizzi IPv4.
Quindi per attaccare nuovi dispositivi all'interno di Internet? Non possiamo utilizzare indirizzi pubblici ma quelli privati utilizzando il NAT (collegato a reti private all'internet pubblica) oppure fare la migrazione a IPv6 ma non e' una soluzione banale.

---

**==IPv6==**
IPv6 e' la nuova versione di IP, specificatamente progettata per superare i limiti di scalabilita' di IPv4, in particolare per quanto riguarda la dimensione per lo spazio degli indirizzi.
IPv6 adotta indirizzi di 128 bit e questo consente di allocare (in teoria) $2^{138}$ cioe' $3,4 \cdot 1038$ indirizzi (un numero di indirizzi per metro quadro di superficie della Terra, maggiore del numero di Avogadro)

Ha numerosi vantaggi rispetto a IPv4:
- Adozione di un formato di frame piu' semplice (e quindi piu' veloce da passare per i router)
- Indirizzi con diversi scope: link-local, site-local (deprecato) e global
- Ripensamento di alcuni protocolli (es. ARP) in favore di soluzioni piu' robuste
- Estensioni per connettere dispositivi low-power

La differenza tra questi due formati di indirizzi sta nel fatto che gli indirizzi IPv6 sono più lunghi e formattati in modo diverso, quindi esistono più configurazioni di indirizzi IPv6 univoci possibili. IPv4 è un sistema a 32 bit che utilizza una stringa di numeri separati da punti, mentre IPv6 è un sistema a 128 bit che usa sequenze alfanumeriche separate da due punti.