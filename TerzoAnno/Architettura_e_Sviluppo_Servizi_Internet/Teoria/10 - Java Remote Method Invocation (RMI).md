Java RMI e' un'implementazione Java-specific del modello RPC, ovviamente esteso per considerare un'architettura a oggetti.

In modo analogo a RPC, l'obiettivo di Java RMI e' quello di rendere il piu' possibile simili la chiamata locale e la chiamata remota.

La piattaforma Java facilita la gestione dell'eterogeneita' dei nodi e consente di mascherare completamente le differenze sintattiche fra chiamata locale e remota:
- uso di interfacce Java al posto di IDL;
- uso di tipi di dati primitivi Java;
- serializzazione per trasmissione (dello stato) degli oggetti;
- repository di implementazione per condivisione del codice;

## Modello a oggetti distribuiti
I sistemi a oggetti distribuiti, come Java RMI e COBRA, permettono di realizzare applicazioni basate sul paradigma a oggetti.
Essi presentano numerosi vantaggi, in termini di estendibilita', facilita' di integrazione e semplicita' di sviluppo, e si sono mostrati vincenti a livello enterprise.

Nel modello a oggetti distribuito di Java RMI un *oggetto remoto* consiste in un oggetto:
- con metodi invocabili da un'altra JVM, in esecuzione su un host differente
- descritto **tramite interfacce remote** che dichiarano i metodi accessibili da remoto.

## Architettura RMI
![[ArchitetturaRMI.png]]

- **Stub**: proxy locale su cui vengono fatte le invocazioni destinate all'oggetto remoto
- **Skeleton**: elemento remoto che riceve le informazioni fatte sullo stub e le realizza effettuando le corrispondenti chiamate sul server 
- **Remote Reference Layer**: fornisce il supporto alle chiamate inoltrate dallo stub. Il RRL ha il compito di instaurare la connessione fra il client e il server eseguendo operazioni di codifica e decodifica dei dati
- **Transport Layer**: localizza il server RMI relativo all'oggetto remoto richiesto, gestisce le connessioni (TCP/IP, timeout) e le trasmissioni (sequenziali, serializzate)
- **Registry**: servizio di naming che consente al server di pubblicare un servizio e al client di recuperarne il proxy

## Sviluppo di un'applicazione RMI
Per realizzare un'applicazione distribuita usando Java RMI dobbiamo:
1. Definire l'interfaccia di servizio
2. Progettare le implementazioni dei componenti utilizzabili in remoto e compilare le classi necessarie (con `javac`)
3. Generare stub e skeleton delle classi utilizzabili in remoto (con il compilatore RMI `rmic`)
4. Attivare il registry dei servizi
5. Registrare il servizio (il **server** deve fare un **bind sul registry**)
6. Effettuare il **lookup sul registry** (il **client** deve ottenere il reference all'oggetto remoto)

A questo punto l'interazione tra il client e il server pio' procedere.

### Definizione dell'interfaccia di servizio 
L'interfaccia del servizio e' il contratto che deve essere rispettato dai servitori anche dai clienti (a cui deve essere nota).
Le interfacce di servizio sono semplici interface Java che devono ereditare dall'interfaccia `java.rmi.Remote`.
Tutti i metodi pubblici di queste interfacce saranno invocabili da remoto. Tali metodi devono dichiarare di sollevare `java.rmi.RemoteException`.

``` Java
imprt java.rmi.*
public interface EchoService extends Remote {
	public String getEcho(String echo) throws RemoteException;
}
```

### Implementazione Server
``` Java
import java.rmi.*;
import java.rmi.server.*;

public class EchoServiceImpl extends UnicastRemoteObject implements EchoService {
	public String getEcho(String str) throws RemoteException {
		return str;
	}
	public EchoServiceImpl() throws RemoteException {
		super();
	}
	public static void main(String[] args) {
		try {
			EchoService service = new EchoServiceImpl();
			Naming.rebind("EchoService", service);
		} catch(Exception e) {
			System.err.println(e.getStackTrace());
			System.exit(1);
		}
	}
}
```

Il server deve estendere la classe `UnicastRemoteObject` e implementare costruttore e metodi dell'interfaccia remota

### Implementazione Client
``` Java
import java.io.*;
import java.rmi.*;

public class EchoServiceClient {
	public static void main(String[] args) {
		try {
			InputStreamReader isr = new InputStreamReader(System.in);
			BufferedReader stdIn = new BufferedReader(isr);
			
			/* Connessione al registry RMI remoto: si deve
			ricercare l’interfaccia e ritirarla in modo
			corretto */
			EchoService service = (EchoService)Naming.lookup("EchoService");

			// Interazione con l'utente
			System.out.print("Messaggio? ");
			String message = stdIn.readLine();
			
			// Richiesta del servizio remoto
			String echo = service.getEcho(message);

			// Stampa risultati
			System.out.println("Echo: ” + echo + "\n");
		} catch (Exception e) { e.printStackTrace(); System.exit(1); }
	}
}
```

### Compilazione e creazione stub e skeleton 
**Compilazione di interface, client e server**:
``` shell
[mauro@remus rmi]$ javac EchoService.java EchoServiceClient.java EchoServiceImpl.java
```

**Creazione di Stub e Skeleton con il compilatore RMI**:
``` Shell
[mauro@remus rmi]$ rmic –v1.1 EchoServiceImpl
```

`rmic` genera `EchoServiceImpl_Stub.class` e `EchoServiceImpl_Skel.class`

1. Avviamento del registry:
   `[mauro@remus rmi]$ rmiregistry`
2. Avviamento del server:
   `[mauro@remus rmi]$ java EchoServiceImpl`
3. Avviamento del client:
   `[mauro@remus rmi]$ java EchoServiceClient`

### Serializzazione
In Java marshalling e unmarshalling vengono realizzati tramite il meccanismo di *serializzazione* offerto dal linguaggio.

La serializzazione permette la lettura/scrittura di dati complessi da/su stream e viene fatta in maniera **trasparente** dal supporto (ovverosia dal tipo di stream)

**Serializzazione**: trasformazione di oggetti complessi in semplici sequenze di byte su uno stream di output
$\Rightarrow$ metodo `writeobject()`

**Deserializzazione**: decodifica di una sequenza di byte e costruzione di una copia dell'oggetto originale da uno stream di input
$\Rightarrow$ metodo `readObject()`

In Java la serializzazione e' utilizzata in molti casi:
- persistenza
- trasmissione di oggetti tra macchine diverse (parametri e valori di ritorno in RMI)

Sono **automaticamente serializzabili** (tramite l'apposito supporto fornito dalla Java Virtual Machine) istanze di oggetti che:
- **implementano l'interfaccia *Serializable***
- contengono **esclusivamente oggetti** (o riferimenti a oggetti) **serializzabili**
Infatti, la serializzazione in Java e' ricorsiva.

ATTENZIONE: non viene trasferito l'oggetto vero e proprio ma solo le informazioni che ne caratterizzano l'istanza $\Rightarrow$ no metodi, no costanti, no variabili statiche, no variabili transient.

La maggior parte delle classi della libreria Java sono gia' automaticamente serializzabili, ma alcune non lo sono.

Se una classe contiene oggetti non serializzabili, deve dichiararli come **transient** e implementare i metodi `readObject` e `writeObject`.

Al momento della deserializzazione sara' **ricreata una copia** dell'istanza "trasmessa", usando il codice dell'oggetto e le informazioni ricevute.

### Passaggio di parametri
Il passaggio dei parametri **dipende dal tipo di parametro in considerazione**

| Tipo                     | Metodo Locale                      | Metodo Remoto               |
| ------------------------ | ---------------------------------- | --------------------------- |
| Tipi primitivi           | Per valore                         | Per valore                  |
| Oggetti                  | Per riferimento                    | Per valore (seriaizzazione) |
| Oggetti Remoti Esplorati | Per riferimento (all'oggetto Stub) | Per riferimento remoto      |
Passaggio per valore $\rightarrow$ Serializable Object
Passaggio per riferimento $\rightarrow$ Remote Object

**Oggetti serializzabili**:
- Oggetti la cui locazione non e' rilevante per lo stato.
- Sono passati **per valore**: ne viene serializzata l'istanza che sara' deserializzata a destinazione per crearne una copia locale.

**Oggetti remoti**:
- Oggetti la cui finzione e' strettamente legata alla localita' in cui eseguono (server)
- Sono passati **per riferimento** $\rightarrow$ ne viene serializzato lo stub, creato automaticamente dal proxy (stub o skeleton) su cui viene fatta la chiamata in cui compaiono come parametri

### Localizzazione del servizio
Un client in esecuzione su una macchina ha bisogno di localizzare un server a cui vuole connettersi, che e' in esecuzione su un'altra macchina.

Tre possibili soluzioni:
- Il client conosce in anticipo dov'e' il server
- L'utente dice all'applicazione dov'e' il server 
- Un servizio standard (**naming service**) in una locazione ben nota, che il client fornisce, funziona come punto di indirezione.

Java RMI utilizza un naming service $\rightarrow$ **RMI Registry**

Il registry RMI mantiene un insieme di copie {**name, reference**}, dove *name* e' una stringa arbitraria non interpretata e *reference* e' un riferimento a un oggetto remoto.

Per accedere ai servizi del registry RMI si usa la classe `java.rmi.Naming`, che fornisce i seguenti metodi statici:
``` Java
public static void bind(String name, Remote obj)
public static void rebind(String name, Remote obj)
public static void unbind(String name)
public static String[] list(String name)
public static Remote lookup(String name)
```

Il parametro nome e' una URL che combina la locazione del registry e il nome logico del servizio, nel formato `[rmi://hostname:port/]object_name` (la porta di default e' la 1099).

#### Sicurezza del registry
Problema: accedendo al registry e' possibile ridirigere per scopi maliziosi le chiamate ai server RMI registrati.

Soluzione $\rightarrow$ i metodi `bind(), rebind() e unbind()` sono invocabili **solo dall'host su cui e' in esecuzione il registry**.
$\Rightarrow$ non si accettano modifiche della struttura client/server dall'esterno.

Sull'host in cui vengono effettuate le chiamate al registry deve essercene almeno uno in esecuzione.

Ogni JVM definisce ambienti di esecuzione differenti e protetti per diverse parti, in particolare per quanto riguarda le interazioni con applicazioni distribuite e l'uso di codice remoto.

Per rendere maggiormente sicure le applicazioni RMI e' necessario impedire che le classi scaricate da host remoti effettuino operazioni per le quali non sono state preventivamente abilitate. A questo fine, si usano `ClassLoader` che effettuano il controllo sul caricamento di codice remoto, associati a `securityManager` per la specifica di protezione.

Java mette a disposizione un `SecurityManager` per applicazioni RMI $\rightarrow$ `RMISecurityMangager`.

#### Serialization Filtering
La deserializzazione di dati non attendibili, in particolare da un Client sconosciuto, non attendibile o non autentico, e' un attivita' intrinsecamente pericolosa perche' il contenuto del flusso di dati di ingresso determina gli oggetti creati, i valori dei relativi campi e i riferimenti tra di essi. Mediante un'attenta costruzione del flusso, un avversario puo' eseguire il codice in classi arbitrarie con intenti dannosi.

La serializzazione in Java e' stata la causa di **molti gravi problemi di sicurezza**.

Necessita' di **implementare comunicazioni su canale cifrato e di adottare serialization filtering**

#### Security Manager - Deprecato!
In passato il meccanismo principale per la gestione della sicurezza delle applicazioni RMI era il Security Manager. Tuttavia esso adotta un approccio che si e' rivelato inadeguato, ed e' stato recentemente deprecato.

`RMISecurityManager` era pensato per essere istanziato al lancio dell'applicazione RMI, sia nel client che nel server.
``` Java
if (System.getSecurityManager() == null) {
	System.setSecurityManager(new RMISecurityManager());
}
```

Se il `SecurityManager` non e' specificato, non e' permesso nessun cambiamento di codice remoto da parte sia del client (Stub) che del server.

I Security Manager erano pensati per effettuare l'enforcing delle politiche di sicurezza nelle applicazioni Java.

Le politiche di sicurezza sono definite dagli sviluppatori, di solito usando dei file di policy.

Sia il Server che il Client devono essere lanciati specificando il file con le autorizzazioni che il security manager deve caricare.

Esempio:
``` Shell
java -Djava.security.policy=echo.policy EchoServiceImpl
java -Djava.security.policy=echo.policy EchoServiceClient host
```

#### Condivisione codice - Insicuro!
RMI originariamente permetteva a Client e Server di condividere codice attraverso il meccanismo del codebase.
Il codebase viene usato dal **Client** per scaricare le classi necessarie del server (interfaccia, stub, oggetti restituiti come valori di ritorno).

Il codebase viene usato dal **Server** per scaricare le classi necessarie relative al client (oggetti passati come parametri nelle chiamate).

### Garbage collection di oggetti distribuiti
In un sistema a oggetti distribuito, e' desiderabile la deallocazione automatica degli oggetti remoti che non hanno piu' nessun riferimento presso dei client.

Il sistema RMI utilizza un algoritmo di garbage collection distribuito basato sul conteggio dei riferimenti:
- Ogni JVM aggiorna una serie di contatori di ciascuno associato ad un determinato oggetto
- Ogni contatore rappresenta il numero dei riferimenti ad un certo oggetto che in quel momento sono attivi su una JVM.
- Ogni volta che viene creato un riferimento ad un oggetto remoto il relativo contatore viene incrementato. Per la prima occorrenza viene inviato un messaggio che avverte l'host del nuovo client.
- Quando un riferimento viene eliminato il relativo contatore viene decrementato.

Se si tratta dell'ultima occorrenza un messaggio avverte il server.

