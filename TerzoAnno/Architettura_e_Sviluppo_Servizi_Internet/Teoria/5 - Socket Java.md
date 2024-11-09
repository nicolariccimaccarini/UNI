## La programmazione Client/Server e le socket
Un Client e un Server su macchine diverse possono comunicare mediante socket.
Una **socket** rappresenta un **terminale** di un **canale di comunicazione bidirezionale**

Ci sono due diversi tipi di modalita' di comunicazione con le socket:
- **con connessione** e affidabile, in cui viene stabilita una connessione senza perdita di messaggi tra Client e Server $\rightarrow$ uso di socket STREAM
- **senza connessione** e non affidabile, in cui non c'e' connessione e i messaggi vengono recapitati uno indipendentemente dall'altro senza garanzia di consegna $\rightarrow$ uso di socket DATAGRAM

## Identificazione dei processi comunicanti
**Identificazione dei processi** nella rete, uso di **nomi globali** univoci e sempre non ambigui $\rightarrow$ Indirizzo IP della macchina + Porta come identificativo del Servizio
![[idProcCom.png]]

### Numeri di Porta
Funzione fondamentale delle porte e' **identificare un servizio**. La richiesta di un servizio a un processo server non richiede quindi la conoscenza a priori del suo pid, ma solo della porta e dell'indirizzo IP della macchina su cui si esegue il Server.

I numeri di porta 0 e 1023 sono riservati (**well-known** ports) $\rightarrow$ possono essere usati solo da processi di sistema.
Queste porte vengono usate per servizi standard ben noti.

Porte 1023-49151 sono dette **registered** e sono ache esse standardizzate ma disponibili per processi che eseguono senza privilegi di amministratore.
Porte 49152-65535 sono dette **dinamiche** a uso libero.

## Le socket in Java: gerarchia classi
I servizi di comunicazione Java sono forniti dalle classi del package di networking `java.net`.

Classi per Socket:
- **Con connessione** (TCP)
	- classe `Socket` per Client
	- classe `ServerSocket` per Server
- **Senza connessione** (UDP)
	- classe `DatagramSocket` per Client e Server

### Comunicazione con connessione: Java socket STREAM
Le socket STREAM sono i terminali di un canale di comunicazione virtuale **con connessione** creato tra il Client e il Server.
La comunicazione avviene in modo **bidirezionale**, affidabile, con i dati che vengono **consegnati in sequenza** (modalita' **FIFO**).

Queste caratteristiche discendono dal TCP (protocollo di comunicazione sottostante):
- TCP e' un protocollo di trasporto (livello 4 OSI) e fornisce delle porte per identificare i servizi

La **connessione** tra i processi Client e Server e' definita da
`<protocollo; indirizzo IP Client; porta Client; indirizzo IP Server; porta Server>`

La comunicazione tra Client e Server segue uno schema **asimmetrico n:1**. Questo ha portato al progetto di due tipi di socket diversi, una per il Client e una per il Server.
Classi distinte per ruoli Cliente e Servitore:
- `java.net.Socket` per la socket lato Client
- `java.net.ServerSocket` per la socket lato Server

#### Lato Cliente: classe `java.net.Socket`
La classe `Socket` consente al **Client** di creare una socket STREAM (TCP) per la comunicazione con il Server.
Tale socket e' anche detta socket "attiva".
I **costruttori** della classe **creano** la socket, la **legano** a una porta locale e la **connettono** alla socket del Server.

Presenti diversi costruttori (ovviamente `public`) della classe `Socket`
- `Socket (InetAddress remoteAddr, int remotePort) throws...`
  Crea una socket stream cliente e la collega alla porta specificata dalla macchina remota all'indirizzo IP `remoteAddr`
- `Socket (String remoteHost, int remotePort) throws...`
  Crea una socket stream cliente e la collega alla porta specificata dalla macchina remota di nome logico `remoteHost`
- `Socket (InetAddress remoteAddr, int remotePort, InetAddress localAddr, int localPort) throws...`
  Crea una socket stream cliente e la collega alla porta specificata dalla macchina remota. Inoltre lega la socket a una porta della macchina locale e su una specifico IP locale.

#### Lettura/scrittura su socket Java
Per leggere/scrivere si usano gli stream $\rightarrow$ uno stream e' una sequenza ordinata di byte, che viene acceduto in modo sequenziale.
- Stream di ingresso, `InputStream` per leggere byte
- Stream di uscita, `OutputStream` per scrivere byte

Nel caso socket, i metodi:
- `public InputStream getInputStream()`
- `public OutputStream getOutputStream()`

Esempio di creazione e uso stream nel caso socket:
- Parte di ingresso e canale di comunicazione:
``` Java
DataInputStream in = new DataInputStream(sock.getInputStream());
float f - in.readFloat();
```
- Parte di uscita del canale di comunicazione:
``` Java
DataOutputStream out = new DataOutputStream(sock.getOutputStream());
out.writFloat(f);
```

Wrapping Streams $\rightarrow$ uno stream puo' essere impacchettato in un altro stream per fornire altre funzioni, di piu' alto livello.

Le varie classi di stream sono orientate ai byte. Per gestire dei caratteri invece si usano le classi **Reader/Writer**.

Esempio di creazione reader e wrapping in `BufferedReader` per line-oriented input:
``` Java
InputStreamReader isr = new InputStreamReader(sock.getInputStream() "UTF-8");
BufferedReader in = new BufferedReader(isr);
String line = in.readLine();
```

Esempio di creazione writer e wrapping per buffering:
``` Java 
OutputStreamWriter osw = new OutputStreamWriter(sock.getOutputStream(), "UTF-8");
BufferedWriter bw = new BufferedWriter(osw);
bw.write("qualcosa");
bw.newLine();
bw.flush();
```

#### Limiti alle aperture contemporanee di connessioni
Una connessione impegna risorse dei nodi e dei processi. Costa crearle e mantenerle. Per questo motivo, il numero di connessioni che un processo puo' aprire e' limitato, per cui e' necessario chiudere le connessioni non piu' utilizzate.

Il metodo `close()` chiude l'oggetto socket e disconnette il Client dal Server.
``` Java
public synchronized void close()
```

#### Lato Cliente: classe `java.net.Socket`
Per ottenere delle **informazioni** si possono utilizzare i metodi:
``` Java
public InetAddress getInetAddress()
```
restituisce l'indirizzo della macchina remota a cui la socket e' connessa

``` Java
public InetAddress getLocalAddress()
```
restituisce l'indirizzo della macchina locale

``` Java
public int getPort()
```
restituisce il numero di porta sulla macchina remota a cui la socket e' connessa

``` Java
public int getLocalPort()
```
restituisce il numero di porta sulla macchina locale a cui la socket e' legata

#### Client di echo (servizio standard sulla porta 7)
``` Java
try {
	Socket theSocket = new Socket(hostname, 7);
	BufferedReader networkIn = new BufferedReader( 
		new InputStreamReader( theSocket.getInputStream(), “UTF-8”));
	BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));
	BufferedWriter networkOut = new BufferedWriter(
		new OutputStreamWriter( theSocket.getOutputStream(), “UTF-8”));
		
	System.out.println("Connected to echo server");
	
	while (true) {
		String theLine = userIn. readLine();
		if (theLine.equals(".")) break;
		networkOut. write(theLine);
		networkOut.newLine();
		networkOut. flush();
		System.out.println(networkIn. readLine());
	}
} // end try
catch (IOException e) {
	System.err.println(e);
	System.exit(1);
}
```

#### Lato Server: classe `java.net.ServerSocket`
La classe `ServerSocket` definisce una socket di **ascolto**, capace di accettare delle richieste di connessione provenienti da qualunque Client. Tale socket e' detta anche **passiva**, per ricordare che il Server ha un ruolo passivo nel rendez-vous con il Client.
Il costruttore **crea** la socket del Server e la **lega** alla porta della macchina server. Definisce anche la lunghezza della coda in cui vengono messe le richieste di connessione non ancora accettate dal Server (il modello C/S e' asimmetrico molti:1).

Costruttori:
``` Java
public ServerSocket(int localPort) 
	throws IOException, BindException
```
crea una socket in ascolto sulla porta specificata

``` Java
public ServerSocket (int localPort, int count)
throws IOException, BinfdException
```
crea una socket in ascolto sulla porta specificata con una coda di lunghezza `count`

Dopo aver creato la `ServerSocket`, il Server si mette in attesa di nuove richieste di connessione chiamando il metodo `accept()`.
La chiamata di `accept()` **blocca** il Server fino all'arrivo di una richiesta.
Quando arriva una richiesta, `accept()` stabilisce una nuova connessione tra il Client e un nuovo oggetto `Socket` restituito da `accept()`.
La trasmissione dei dati avviene con i metodi visti per il lato Client.
``` Java
public Socket accept() throws IOException
```

Per ottenere delle informazioni sulle socket passive si possono utilizzare i metodi:
``` Java
public InetAddress getInetAddress()
```
restituisce l'indirizzo della macchina server **locale** 

``` Java
public int getLocalPort()
```
restituisce il numero di porta sulla macchina **locale** a cui la socket e' legata

#### Server daytime (servizio standard sulla porta 13)
``` Java
try {
	ServerSocket server = new ServerSocket(13);
	while (true) {
		Socket sock = server.accept();
		// Alla connessione, il Server invia la data al Client
		BufferedWriter out = new BufferedWriter(
			new OutputStreamWriter(sock.getOutputStream(), "UTF-8"));
		out.write(new Date()); // Data e ora corrente
		out.newLine();
		out.flush();
		sock.close();
	}
}
catch (IOException e) {
	System.err.println(e);
	system.exit(1);
}
```

#### rcp lato Client
``` Java
File theFile = new File(args[3]);
// ... controllo esistenza file ...
try {
	char risposta; FileInputStream fileIn;
	Socket s = new Socket(args[0], Integer.parseInt(args[1]));
	DataInputStream in = new DataInputStream(s.getInputStream());
	BufferedWriter out = new BufferedWriter(
	new OutputStreamWriter(s.getOutputStream(), “UTF-8”));

	out.write(args[2]); out.newLine(); out.flush();
	if (in.readChar() == 'S') {
		byte buffer[] = new byte[BUFDIM];
		int bytesRead;
		FileOutputStream fileOut = new FileOutputStream(theFile);
		while ((bytesRead = in.read(buffer, 0, BUFDIM)) != -1) {
			fileOut.write(buffer, 0, bytesRead);
		}
		fileOut.close();
	}
	s.close();
} catch (IOException e) { ...
```

#### rcp lato Server (caso iterativo)
``` Java
...
try {
	ServerSocket ss = new ServerSocket(Integer.parseInt(args[0]));
	while (true) {
		Socket s = ss.accept();
		BufferedReader in = new BufferedReader(
			new InputStreamReader(s.getInputStream(), “UTF-8”));
		DataOutputStream out = new DataOutputStream(s.getOutputStream());
		File file = new File(in.readLine());
		if (file.exists()) {
			out.writeChar('S'); out.flush();
			byte buffer[] = new byte[BUFDIM];
			int bytesRead;
			FileInputStream fileIn = new FileInputStream(file);
			while ((bytesRead = fileIn.read(buffer, 0, BUFDIM)) != -1) {
				out.write(buffer, 0, bytesRead);
			}
			fileIn.close();
			out.flush();
		} else {
			out.writeChar('N'); out.flush();
		}
		s.close();
	}
}
catch (IOException e) { ...
```

#### Opzioni delle Socket
Il comportamento di default delle socket Java puo' essere modificato agendo sulle *opzioni* delle socket, utilizzano i metodi
- `setTcpNoDelay(boolean on) throws ...`
  il pacchetto e' inviato immediatamente, senza bufferizzazioni
- `setSoLinger(boolean on, int linger)`
  dopo la close, il sistema tenta di consegnare i pacchetti ancora i attesa di spedizione. Questa opzione permette di scartare i pacchetti in attesa
- `setSoTimeout(int timeout) throws ...`
  la lettura del socket e' normalmente bloccante. Questa opzione definisce un timeout, trascorso il quale il thread si sblocca (ma viene lanciata una eccezione da gestire)

#### Comunicazione senza connessione: Java Socket DATAGRAM
Le socket DATAGRAM permettono a due thread di **scambiarsi messaggi senza stabilire una connessione** tra loro.
La comunicazione con le socket DATAGRAM:
- **non e' affidabile**, cioe' si puo' verificare la perdita di messaggi
- diversi messaggi tra una stessa coppia Client e Server possono essere consegnati **non in ordine**, in quanto puo' capitare che seguano strade diverse

Queste caratteristiche derivano dal protocollo di comunicazione UDP su cui si appoggiano.

Vantaggi/svantaggi di STREAM vs DATAGRAM (e TCP vs UDP)?
Vi e' un solo tipo di socket per DATAGRAM sia per il Client sia per il Server;
- classe `DatagramSocket`

##### La classe `java.net.DatagramSocket`
Classe socket DATAGRAM senza connessione:
```  Java
public final class DatagramSocket extends Object
```

Costruttore:
``` Java
DatagramSocket (InetAddress localAddress, int localPort) throws ...
```
Crea la socket DATAGRAM (UDP) **legata localmente** (a indirizzo IP e porta)

Per comunicare, uso **send** e **receive**:
``` Java
objDatagramSock.send(DatagramPacket);
objDatagramSock.receive(DatagramPacket);
```
La socket e' legata SOLO localmente.

##### La classe `java.net.DatagramPacket`
`DatagramPacket` e' la classe per preparare i datagrammi da inviare sulla socket.
Una istanza **datagramma** specifica un array di byte su cui scrivere (o da cui leggere) e contiene le indicazioni per la comunicazione.

Costruttore nel caso di **INVIO** del messaggio:
``` Java
DatagramPacket(byte[] buf, int lenght, InetAddress address, int port);
```
Il buffer `buf` e' un'area in cui mettere i dati, l'indirizzo e' quello del destinatario. `InetAddress` e' la classe per gli indirizzi IP

Costruttore nel caso di **RICEZIONE** messaggio:
``` Java
DatagramPacket(byte[] buf, int lenght);
```
Il buffer `buf` e' un'area preparata per ricevere i dati.

La classe `DatagramPacket` offre metodi per estrarre o resettare le informazioni:
``` Java
getAddress(), getPort() // restituisce address e port in macchina remota
setAddress(InetAddress addr), setPort(int port)
getData(), sendData(byte[] buf), etc.
```

##### La classe `java.net.InetAddress`
La classe `InetAddress` permette di rappresentare gli indirizzi IP delle macchine, sia per socket unicast che multicast, sia per indirizzi IPv4 che IPv6.
La classe fornisce i metodi per **risolvere i nomi**, cioe' passare dal nome simbolico all'indirizzo IP e viceversa.

Esempio:
``` Java
public static InetAddress getByName(String host) throws UnknownHostException
```

Restituisce l'indirizzo IP corrispondente al nome logico di host fornito.

##### Client DATAGRAM: schema
``` Java
// Creazione socket
dgramSocket = new DatagramSocket();

// Interazione da console con l'utente
BufferedReader stdIn = new BufferedReader( new InputStreamReader(System.in));
System.out.println("Domanda...");
String richiesta = stdIn.readLine();

// Creazione del pacchetto di richiesta con le informazioni inserite dall'utente
sendBuff = richiesta.getBytes("UTF-8");
packetOUT = new DatagramPacket(sendBuff, sendBuff.lenght, serverAddr, serverPort);

// Invio del pacchetto al Server
dgramSocket. send(packetOUT);

// Attesa del pacchetto di risposta
packetIN = new DatagramPacket (buf, buf.lenght);
dgramSocket.receive(packetIN);
```

##### Server DATAGRAM: schema
``` Java
// Creazione socket
dgramSocket = new DatagramSocket(PORT);

// Attesa del pacchetto di richiesta
packet = new DatagramPacket(buf, buf.length);
dgramSocket.receive(packet);

// Estrazione delle informazioni dal pacchetto ricevuto
byte[] requestBuf =
Arrays.copyOf(packet.getData(), packet.getLength());
String request = new String(requestBuf, “UTF-8”);

 ...
// Creazione del pacchetto di risposta con la linea richiesta
...

// Invio del pacchetto al client
dgramSocket.send(packet);
```

#### Esempio Client/Server DATAGRAM
Client:
``` Java
...
BufferedReader inFromUser = new BufferedReader(new InputStreamReader(System.in));
DatagramSocket clientSocket = new DatagramSocket();

InetAddress serverAddr = InetAddress.getByName(“server.unife.it");

byte[] receiveData = new byte[1024];

System.out.println("Inizio Client, inserire stringa:");
String sentence = inFromUser.readLine();
byte sendData = sentence.getBytes(“UTF-8”);

DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, serverAddr, 9876);

clientSocket.send(sendPacket);

DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);

clientSocket.receive(receivePacket);

String modifiedSentence = new String(receivePacket.getData(), “UTF-8”);

System.out.println("FROM SERVER:" + modifiedSentence);

clientSocket.close();

...
```

Server:
``` Java
...
DatagramSocket serverSocket = new DatagramSocket(9876);
byte[] receiveData = new byte[1024];

while(true) {
	System.out.println("Server attende richiesta");
	
	DatagramPacket receivePacket =
	new DatagramPacket(receiveData, receiveData.length);
	serverSocket. receive(receivePacket);
	byte[] requestBuf = Arrays.copyOf(packet.getData(), packet.getLength());
	String sentence = new String(requestBuf, “UTF-8”);
	System.out.println("Richiesta ricevuta: " + sentence);
	
	InetAddress IPAddress = receivePacket.getAddress();
	int port = receivePacket.getPort();
	
	String uppercasedSentence = sentence.toUpperCase();
	byte[] sendData = uppercasedSentence.getBytes( “UTF-8”);
	
	DatagramPacket sendPacket = 
		new DatagramPacket(sendData, sendData.length, IPAddress, port);
	serverSocket. send(sendPacket);
} ...
```

#### Note conclusive sulle socket DATAGRAM
Si notino alcuni aspetti importanti che possono verificarsi in applicazioni che fanno uso di socket DATAGRAM:
- **La comunicazione via Socket DATAGRAM non e' affidabile**, per cui in caso di perdita del messaggio del Client o della risposta del Server, il Client puo' rimanere bloccato in attesa indefinita della risposta
- **Possibile blocco del Client** anche nel caso di invio di una richiesta a un Server non attivo
- **Le DATAGRAM sono appoggiate sul protocollo UDP che non ha flow control**, se il Server riceve troppi datagrammi per le sue capacita' di elaborazione, questi vengono scartati, senza nessuna notifica ai Client

#### Socket e malfunzionamenti
Le socket STREAM e DATAGRAM si comportano in maniera molto diversa a fronte di **malfunzionamenti**.
Questo deriva dalle differenze tra i protocolli sottostanti (rispettivamente TCP E UDP)
![[MalfunzionamentiSocket.png]]

#### Semantica may-be
Le socket DATAGRAM presentano una semantica di comunicazione di tipo **may-be**. Questo perche' il protocollo di supporto UDP **non mette in pratica nessuna azione per fronteggiare il caso di guasto**. Un messaggio DATAGRAM viene semplicemente inviato, senza chiedere di essere avvisati dalla ricezione. Quindi il mittente non puo' desumere cosa sia successo del messaggio spedito.

Le socket DATAGRAM non sono quindi affidabili. Forniscono buone presentazioni, ma possono portare problemi a livello di applicazione.

#### Semantica at-most-once
Nel caso delle socket STREAM, la comunicazione e' affidabile e la semantica ottenuta e' detta **at-most-once**. Il supporto di comunicazione fa di tutto per garantire la consegna dei messaggi e ogni messaggio viene consegnato al massimo una volta.
Questo e' reso possibile dal sottostante protocollo **TCP** che chiede conferma di ogni messaggio inviato e ritrasmette quelli persi. Inoltre il TCP numera i messaggi e quindi si accorge di messaggi arrivati piu' volte.

#### Quale tipo di Socket utilizzare?
La scelta del tipo di socket viene fatta sulla base delle molte differenze tra la socket STREAM e quella DATAGRAM:
- Servizi che richiedono una connessione $\leftrightarrow$ servizi connectionless. 
- Socket STREAM sono **affidabili**, DATAGRAM no
- **Prestazioni** STREAM (costo di mantenere una connessione logica) inferiori alle DATAGRAM
- STREAM hanno **semantica** at-most-once, DATAGRAM may-be
- Ordinamento messaggi preservato in STREAM non in DATAGRAM
- Per fare **broadcast/multicast** piu' indicate le DATAGRAM


#### Le socket multicast
- Invio di uno stesso messaggio a molti riceventi diversi (un gruppo) con socket **multicast**.
- Le socket multicast estendono le datagram aggiungendo funzionalita' di gestione gruppi.
- Un gruppo multicast e' specificato da indirizzi di classe D (da 224.0.0.0 a 239.255.255.255)
- Fase di **creazione della socket multicast**:
  `MulticastSocket ms = new MulticastSocket(7777);`
- **Definizione del gruppo**:
  `InetAddress gruppo = InetAddress.getByName("229.1.2.3)`
- **Operazioni di ingresso/uscita dal gruppo**:
  `ms.joinGroup(gruppo);`
  `ms.leaveGroup(gruppo);`

**Spedizione e ricezione**:
``` JAva
String msg = “ciao”;
byte[] buf = msg.getBytes(“UTF-8”);
DatagramPacket packet=new DatagramPacket(buf,buf.length,gruppo,7777);
ms.send(packet);

byte[] buf = new byte[DIM];
DatagramPacket recvpacket = new DatagramPacket(buf,buf.length);
ms.receive(recvpacket);
```
- Un messaggio inviato a una socket multicast viene ricevuto da tutto il gruppo dei sottoscrittori.
- Un messaggio puo' essere inviato anche da una normale socket datagram
- Una stessa socket multicast puo' appartenere contemporaneamente a molti gruppi differenti

#### Le socket Unix
Nei sistemi Unix esiste un tipo di socket che rappresenta una forma di IPC tra due processori nella stessa macchina simile alle FIFO.

Rispetto alle connessioni TCP/IP su interfaccia loopback, le socket di dominio Unix presentano alcuni vantaggi:
- Poiche' possono essere utilizzati solo per la comunicazione sullo stesso host, l'apertura al posto di un socket TCP/IP non comporta il rischio di accettare connessioni remote
- Il controllo dell'accesso viene applicato con meccanismi basati su file, dettagliati, ben compresi e applicati dal sistema operativo
- Le socket Unix hanno **tempi di configurazione piu' rapidi e una maggiore velocita' di trasmissione dei dati** rispetto alle connessioni di tipo loopback TCP/IP.
- Le socket Unix **si possono usare per la comunicazione tra container sullo stesso sistema**
- Supportano passaggio di file descriptor.

Nonostante il nome non sono supportate solo su sistemi operativi basati su Unix, ma anche su sistemi Microsoft.

Le socket Unix hanno tre tipologie:
- **Stream** $\rightarrow$ connection-oriented, stream-based, affidabili (semantica TCP)
- **Datagram**: connectionless, preservano i confini dei messaggi, e al contrario delle socket Datagram di dominio Internet **sono affidabile consegnano i messaggi rispettando l'ordine di trasmissione**
- **Sequential packet (SEQPACKET)** $\rightarrow$ connection-oriented, preservano i confini dei messaggi, sono affidabili e consegnano i messaggi rispetto all'ordine di trasmissione (semantica STCP)

Esempio:
``` Java
...
var address = UnixDomainSocketAddress.of("/var/myapp/socket");
try (var serverChannel = ServerSocketChannel.open(UNIX)) {
	serverChannel.bind(address);
	try (var clientChannel = serverChannel.accept()) {
		ByteBuffer buf = ByteBuffer.allocate(1024);
		clientChannel.read(buf);
		buf.flip();
		System.out.printf("Read %d bytes\n", buf.remaining());
	}
} finally {
	Files.deleteIfExists(address.getPath());
}
...

...
var address = UnixDomainSocketAddress.of("/var/myapp/socket");
try (var clientChannel = SocketChannel.open(address)) {
	var buf = ByteBuffer.wrap("Hello world".getBytes(“UTF-8”));
	clientChannel.write(buf);
}
...
```