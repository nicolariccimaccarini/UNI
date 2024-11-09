## Definizione
- **Design pattern** $\rightarrow$ soluzione progettuale assodata per un proble a ricorrente in un determinato contesto
- Permettono riuso dell'esperienza di progettazione
- Conducono a sistemi piu' contenibili
- Aumentano la produttivita'
- linguaggio comune per comunicare scelte progettuali (se i pattern sono opportunamente codificati e indentati)

## Detto in un altro modo: "concept reuse"
- Se ci limitiamo a riusare un programma o un componente sviluppato in un'altra occasione, dobbiamo seguire le stesse decisioni fatte da chi ha sviluppato originariamente quel codice
- Questo puo' implicare delle limitazioni al riuso
- Quindi a noi interessa "riusare" in senso piu' astratto
	- l'approccio utilizzato e' descritto in modo indipendente
	- noi svilupperemo l'implementazione secondo le nostre decisioni progettuale

## Ricapitolando
- Cosa sono
	- costituiscono un vocabolario comune per i progettisti
	- sono una notazione abbreviata per comunicare in modo efficace principi complessi
	- aiutano a documentare l'architettura
	- catturano parti critiche di un sistema in forma compatta
	- mostrano piu' di una soluzione
	- descrivono astrazioni software
- Cosa non sono
	- non sono una soluzione precisa di problemi progettuali
	- non risolvono tutti i problemi progettuali

## Elementi essenziali di un Design Pattern
- **Nome**
	- un design pattern deve essere identificato in modo univoco per favorire la comunicazione
- **Problema affrontato**
	- descrizione del contesto in cui e' possibile applicare il pattern
- **Soluzione**
	- per la progettazione OO e' espressa in termini di classi, responsabilita' e collaborazioni
- **Conseguenze**
	- risultati dell'applicazione del pattern, da valutare nella scelta tra piu' alternative
- **Struttura** 
	- diagramma UML

## Come classificarli
- GoF descrivono i Design Pattern classificandoli in base a due caratteristiche
	- **purpose** $\rightarrow$ campo di applicazione del pattern
	- **scope** $\rightarrow$ indica se il patter specifica relazioni tra classi oppure tra oggetti

### Classificazione
- **Strutturali**
	- si concentrano sul come classi e oggetti sono combinati per formare strutture piu' grandi
	- pattern strutturali di classi, usano l'ereditarieta' per comporre interfacce o implementazioni
	- pattern strutturali di oggetti descrivono modi di comporre oggetti per realizzare nuove funzionalita'
- **Behavioral**
	- pattern che identificano metodi di comunicazione tra oggetti e li realizzano
	- aumentano la flessibilita' implementando la comunicazione
	- sono legati all'interazione e alla responsabilita'
- **Creational**
	- astraggono il processo di istanziazione (creazione)
	- aiutano a rendere il sistema indipendente da come gli oggetti sono creati, composti e rappresentati
	- nascondono la conoscenza di quali sono le classi concrete effettivamente istanziate
	- nascondono dettagli sulla creazione e sulla composizione degli oggetti

## Singleton
**Problema**
- talvolta e' necessario garantire che una classe abbia un'unica istanza, accessibile attraverso un unico punto di accesso
- passare attraverso `new` non va bene 

``` Java
public MyClass {
	private MyClass() {}
}
```
la sintassi e' legale, ma non ha molto sento: il constructor e' privato e quindi non riesco ad istanziare la classe

``` Java
public MyClass {
	public static MyClass getInstance() {}
}
```
visto che e' un metodo statico, posso fare `MyClass.getInstance()`

``` Java
public MyClass {
	private MyClass() {}
	public static MyClass getInstance() {
		return new MyClass();
	}
}
```
`MyClass.getInstance()` mi istanzia l'oggetto. Ora devo fare in modo di poterne creare solo uno.

**Soluzione**
- si rende il costruttore della classe privato, in modo che non sia possibile creare direttamente istanze (al di fuori della classe)
- si fornisce un metodo "static" per ottenere l'unica istanza, che viene conservata in un campo static privato della class
- varianti
	- l'istanza puo' essere creata all'inizializzazione del programma oppure e' la prima volta che vine richiesta
	- l'istanza, se necessario, puo' appartenere a una sottoclasse della classe singleton

**Conseguenze**
- accesso controllato all'unica istanza
- non occorre usare variabili globali per accedere all'unica istanza
- e' semplice estendere (mediante subclassing) la classe singleton senza modificare il codice che la usa
- se serve, e' semplice passare da una singola istanza a un numero diverso di istanze

Esempio di implementazione
``` Java
import java.util.*;
public class Singleton {
	private volatile static Singleton uniqueInstance;
	// altri dati importanti
	private Singleton() {}
	
	public static Singleton getInstance() {
		if(uniqueInstance == null) {
			synchronized(Singleton.class)m{
				if(uniqueInstance == null) {
					uniqueInstance = new Singleton();
				}
			}
		}
	return uniqueInstance;
	}
}
```

## Factory Method
**... program to an interface, not to a implementation ...**
- quando usiamo new stiamo certamente istanziando una classe concreta, quindi una implementazione e non una interfaccia
- quando abbiamo un insieme di classi concrete spesso capita questo
- abbiamo varie possibili classi da cui decidiamo a tempo di esecuzione quale vogliamo istanziare
- in una situazione simile sappiamo che in caso di modifiche dobbiamo riesaminare tutto il codice

**Il problema**
- una classe ha bisogno di creare un oggetto ("prodotto") che implementa una interfaccia, ma vuole evitare che dipenda da una specifica implementazione concreta tra quelle disponibili
- oppure, una classe vuole delegare alle sottoclassi la creazione di determinati oggetti

Es.: negozio che vende pizze
``` Java
Pizza orderPizza() {
	Pizza pizza = new Pizza();

	pizza.prepare();
	pizza.bake();
	pizza.cut();
	pizza.box();
	return pizza;
}
```

Ma ci sono tanti tipi di pizza
``` Java
Pizza orderPizza(String type){
	Pizza pizza;
	if(type.equals(“marin”)) {
		pizza=new MarinaraPizza();
	} else if(type.equals(“marghe”)) {
		pizza=new MargheritaPizza();
	} else if...
}

pizza.prepare();
pizza.bake();
pizza.cut();
pizza.box();
return pizza;
}
```

Per star dietro alla concorrenza bisogna aggiungere nuovi tipi di pizza...

**Soluzione**
- si incapsula la creazione degli oggetti in un metodo
- varianti
	- il metodo puo' essere nella stessa classe che deve usare gli oggetti oppure essere un metodo "static" di una classe diversa
	- il metodo "abstract" e quindi richiedere che una sottoclasse ne definisca l'implementazione, oppure essere concreto e fornire una implementazione di default

``` Java
public class PizzaStore {
	SimplePizzaFactory factory;

	public pizzaStore(SimplePizzaFactory factory){
		this.factory = factory;
	}

	public Pizza orderPizza(String type) {
		Pizza pizza;

		pizza = factory.createPizza(type);

		pizza.prepare();
		pizza.bake();
		pizza.cut();
		pizza;box();
		return pizza;
	}
}
```

- Abbiamo avuto successo e tutti vogliono la nostra pizza: ci espandiamo in altre citta'!
- Pero' dobbiamo adattarci
	- a Roma vogliono la pizza molto sottile
	- a Palermo la vogliono sottile e croccante
	- a Bari vorrebbero anche la scamorza come formaggio
	- ...
- Come fare?

Potremmo togliere `SimplePizzaFactory` e creare diverse factory. Poi componiamo i `PizzaStore` con le factory opportune.
``` Java
RomaPizzaFactory romaFactory = new RomaPizzaFactory();
PizzaStore romaStore = new PizzaStore(romaFactory);
romaStore.order(“margherita”);

BariPizzaFactory bariFactory = new BariPizzaFactory();
PizzaStore bariStore = new PizzaStore(bariFactory);
bariStore.order(“margherita”);
```

- Qui ogni Store fa le cose a modo suo.
- Noi vorremmo che il codice che "fa pizza" fosse legato al `PizzaStore` e che non fosse sempre lo stesso, pur facendo pizze in modo diverso.
- Mettere `createPizza()` dentro a `PizzaStore` ma rimanendo flessibili.

``` Java
public abstract class PizzaStore {

	public Pizza orderPizza(String type) {
		Pizza pizza;

		pizza = createPizza(type);

		pizza.prepare();
		pizza.bake();
		pizza.cut();
		pizza.box();
		
		return pizza;
	}

	abstract createPizza(String type);
}

public class RomaPizzaStore extends PizzaStore {
	Pizza createPizza(String item) {
	if(item.equals(“margherita”)) {
		return new RomaStyleMargheritaPizza();
	else if(item.equals(“marinara”)) {
		return new RomaStyleMarinaraPizza();
	else ...
	}
}
```
 
- Un "factory method" gestisce la creazione di oggetti e la incapsula in una sottoclasse
- Questo disaccoppia il codice della superclasse dalla creazione dell'oggetto nella classe derivata

![[UMLFactoryMethod.png]]

## Abstract Factory
**Problema**:
- una classe ha bisogno di creare una serie di oggetti (prodotti) che implementano delle interfacce correlate, ma si vuole evitare che dipenda da una specifica implementazione concreta tra quelle disponibili
- il sistema deve essere configurato con famiglie multiple di prodotti, dove una famiglia e' progettata per essere nel suo insieme
- si vuole distribuire una libreria di prodotti rivelando solo le interfacce e non le classi concrete che le implementano

**Soluzione**:
- si definisce una interfaccia `AbstractFactory` con metodi per creare diversi prodotti, e una o piu' classi concrete che implementano questa interfaccia in riferimento a una singola famiglia di prodotti
- a run-time si costruisce un'istanza di una "concrete factory" che viene usata per creare prodotti

Consideriamo la pizza come composta da varie parti (pasta, salsa, formaggio, verdure, ...)
``` Java
public class RomaIngredientFactory implements PizzaIngredientFactory {
	public Dough createDough() {
		return new ThinDough();
	}
	public Sauce createSauce() {
		return new FreshSauce();
	}
	public Cheese createCheese() {
		return new MozzarellaCheese();
	}
	public Veggies[] createVeggies() {
		Veggies veggies[] = { new Garlic(), new Onion(), new Mushroom() };
		return veggies;
	}
	...
}

public abstract class Pizza {
	String name;
	Dough dough;
	Sauce sauce;
	..
	
	abstract void prepare();

	void bake() {
		//baking
	}
	void cut() {
		// cutting
	}
	void box() {
		//boxing
	}
	..
}

public class RomaPizzaStore extends PizzaStore {
	protected Pizza createPizza(String item) {
	Pizza pizza = null;
	PizzaIngredientFactory ingredientFactory = new RomaIngredientFactory();
	if(item.equals(“marg”)) {
		pizza = new RomaStyleMargeritaPizza(RomaIngredientFactory)
	...
}
```

**Conseguenze**:
- nasconde le classi concrete; solo la factory sa quale classe concreta viene istanziata
- consente di sostituire facilmente una famiglia di prodotti con un'altra
- garantisce che i prodotti usati insieme siano della stessa famiglia
- **PROBLEMA**: rende onerosa l'aggiunta di nuovi prodotti a una famiglia, in quanto bisogna modificare simultaneamente tutte le famiglie e tutte le "concrete factories"

## Factory vs Abstract Factory
![[abstractVSfactory.png]]

- Factory: si basa su eredita', la creazione e' delegata alla sottoclasse che implementa il factory method per creare gli oggetti
	- lo scopo e' permettere a una classe di rimandare l'instanziare alle due sottoclassi
- Abstract Factory: si basa su composizione, la creazione e' implementata in metodi esposti nella factory interface
	- lo scopo e' creare famiglie di oggetti correlati senza dover dipendere dalle loro classi concrete

## Adapter
**Problema**:
- Puo' essere sia a livello di classe che di oggetto
	- classe $\rightarrow$ occorre utilizzare una classe gia' disponibile (`Adaptee`) insieme a una libreria di classi sviluppata in modo indipendente; la libreria richiede una particolare interfaccia (`Target`) che non e' presente nell'`Adaptee`
	- oggetto $\rightarrow$ occorre utilizzare un oggetto gia' disponibile (`Adaptee`) insieme a una libreria di classi sviluppata in maniera indipendente; la libreria richiede una particolare interfaccia (`Target`) che non e' presente nell'`Adaptee`

**Soluzione**:
- classe
	- si crea una nuova classe che implementa l'interfaccia `Target` ed eredita l'implementazione dalla classe `Adaptee`
	- l'implementazione nell'`Adapter` dei metodi di `Target` richiama i metodi di `Adaptee`
- oggetto
	- si crea una nuova classe che implementa l'interfaccia `Target` e contiene un riferimento a un oggetto della classe `Adaptee`
	- l'implementazione dell'`Adapter` dei metodi di `Target` richiama i metodi dell'oggetto `Adaptee`

**Conseguenze**:
- classe
	- Se `Target` non e' una interfaccia pura e' necessaria ereditarieta' multipla
	- se c'e' una gerarchia di `Adaptee` occorre una gerarchia parallela di `Adapter`
- oggetto
	- l'`Adapter` puo' essere utilizzato per oggetti della classe `Adaptee` e di tutte le classi derivate
	- `Adapter` e `Adaptee` rimangono due oggetti distinti: overhead d memoria

## Composite
**Problema**:
- vogliamo trattare un insieme di oggetti come se fosse l'istanza di un unico tipo di oggetti
**Scopo**:
- combinare gli oggetti in una struttura gerarchica ad albero
**Motivazione**:
- differenziare tra foglia e nodo di solito richiede una logica complessa che puo' introdurre errori
**Soluzione**:
- un'interfaccia che tratti oggetti complessi e elementari in modo uniforme
**Quando usarlo**:
- quando il client non ha bisogno di differenziare tra foglie e nodo

![[Composite.png]]

**Component**:
- e' l'astrazione di tutti i componenti, compresi quelli compositi
- dichiara l'interfaccia degli oggetti
- implementa il comportamento di default dell'interfacia comune a tutte le classi
- dichiara un'interfaccia per accedere e gestire i suoi componenti figlio
**Leaf**:
- rappresenta una foglia dell'albero
- implementa tutti i metodi di Component
**Composite**:
- rappresenta un Component composito (un ramo)
- implement metodi per manipolare i livelli sottostanti
- implementa tutti i metodi di Component, di solito delegandoli ai livelli sottostanti

## Decorator
**Problema**:
- si vuole aggiungere delle responsabilita' ad un oggetto (Component) senza cambiare l'interfaccia
- l'aggiunta deve essere fatta a runtime, oppure ci sono piu' aggiunte utilizzabili in combinazioni diverse; oppure si deve rimuovere a runtime le responsabilita' aggiunte; non e' possibile usare l'ereditarieta'
**Soluzione**:
- so definisce una classe Decorator che implementa l'interfaccia di Component
- Decorator mantiene un riferimento al Component che viene "decorato"
- l'implementazione delle operazioni di Component nella classe Decorator richiama l'implementazione del componente decorato, ma ha la possibilita' di fare delle pre o post elaborazioni
**Conseguenze**:
- il comportamento aggiuntivo e' trasparente per gli utenti del componente
- e' possibile applicare piu' Decorator a cascata
- l'insieme dei Decorator puo' essere deciso a runtime (ed e' possibile rimuovere un decoratore durante l'esecuzione)

``` Java
public class LowerCaseInputStream extends FilterInputStream {
	public LowerCaseInputStream(InputStream in) {
		super(in);
	}
	
	public int read() throws IOException {
		int c = super.read();
		return (c == -1 ? c : Character.toLowerCase((char)c));
	}

	public int read(byte[] b, int offset, int len) throws IOException {
		int result = super.read(b, offset, len);
		for (int i = offset; i<offset+result; i++) {
			b[i]=(byte)Character.toLowerCase((char)b[i]);
		}
		return result;
	}
}

public class InputTest {
	public static void main(String[] args) throws IOException {
		int c;
		try {
			InputStream in = new LowerCaseInputStream( new BufferedInputStream(
							 new FileInputStream("test.txt"))); 
		while((c = in.read()) >= 0) {
			System.out.print((char)c);
		}
		in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
```

Questo pattern e' utilizzato spesso per aggiungere a un oggetto responsabilita' che non riguardano "cosa" viene fatto, ma "come":
- logging
- gestione delle transazioni
- caching
- sincronizzazione

Esistono diversi standard di "look-and-feel" per le interfacce grafiche. Differiscono per come visualizzano menu, bottoni, scrollbar, ...

## Observer
**Problema**:
- Spesso i cambiamenti nello stato di un oggetto (Subject) devono riflettersi su uno o piu' oggetti da esso dipendenti
- si vuole disaccoppiare il Subject dagli oggetti dipendenti
**Soluzione**:
- Si definisce una interfaccia Observer, con un metodo che viene richiamato ad ogni modifica dello stato del Subject
- Gli oggetti che sono interessati a un determinato Subject devono essere registrati spesso per il Subject con un apposito metodo
- Il Subject provvede a richiamare il metodo di notifica per tutti gli Observer registrati ogni volta che cambia il prodotto

**Esempio**:
- la libreria standard Java mette gia' a disposizione una classe (`java.util.Observable`) e un'interfaccia (`java.util.Observer`) per implementare questo pattern
	- il Subject estende Observable
	- quando un metodo modifica lo stato del Subject, deve chiamare il metodo `setChanged` per segnalare il cambiamento
	- al termine di una serie di cambiamenti occorre chiamare il metodo `notifyObservers` per avvisare gli Observer
	- ciascun Observer viene avvisato del cambiamento attraverso il metodo `update`

``` Java
import java.util.Observable;
public class AlertCondition extends Observable {
	public static final int GREEN=0, YELLOW=1, RED=2;

	private int condition;
	public AlertCondition() {
		condition=GREEN;
	}
	public int getCondition() {
		return condition;
	}
	public void setCondition(int newCondition) {
		if (newCondition!=RED && newCondition!=YELLOW && newCondition!=GREEN)
			throw new RuntimeException("Unvalid alert condition!");
		if (newCondition != condition) {
			condition=newCondition;
			setChanged();
		}
		notifyObservers();
	}
}
```

``` Java
import java.util.*;
import java.io.*;
import java.text.*;

public class LogAlertObserver implements Observer {
	private PrintWriter out;
	public LogAlertObserver(String fileName) throws IOException {
		FileOutputStream fos=new FileOutputStream(fileName, true);
		OutputStreamWriter osw=new OutputStreamWriter(fos, "UTF-8");
		out=new PrintWriter(osw);
	}
	public void update(Observable subject, Object arg) {
		AlertCondition alert=(AlertCondition)subject;
		DateFormat dfmt=DateFormat.getDateTimeInstance(DateFormat.MEDIUM,
													   DateFormat.LONG);
		String date=dfmt.format(new Date());
		String state;
		switch (alert.getCondition()) {
			case AlertCondition.GREEN: state="GREEN"; break;
			case AlertCondition.YELLOW: state="YELLOW"; break;
			case AlertCondition.RED: state="RED"; break;
			default: state="UNKNOWN";
		}
		out.println("["+date+"] the alert is: "+state);
		out.flush();
	}
}
```

- Il Subject si preoccupa $\rightarrow$ mandera' notifiche ad ogni oggetto che implementa l'interfaccia Observer
- la progettazione scarsamente accoppiata ci permette di costruire un sistema OO che puo' essere modificato perche' minimizza l'interdipendenza tra gli oggetti
- "Pull" e' considerato piu' corretto che "Push"
- Il framework Swing usa pesantemente questo pattern
- Il pattern Observer definisce una relazione one-to-many tra gli oggetti

## Vantaggi dei Design Pattern
- I Design Pattern permettono il riuso su larga scala di architetture software
- Aiutano anche a documentare e a capire il sistema sviluppato
- I Pattern 

## Svantaggi dei Design Pattern
- I Pattern non portano al riuso diretto del codice
- I Pattern possono essere ingannevolmente semplici
- Se si e' "nuovi" del settore, si puo' definire con l'avere troppi pattern
- I Pattern sono validati dall'esperienza e dal confronto tra gli utilizzatori, piuttosto che da un testing esaustivo
- Integrare i Pattern nel processo di sviluppo di un software richiede una intensa attivita' umana

## Application Framework
- Un Framework comprende il completo design di una applicazione o di un sottosistema
- Un Framework specifica l'architettura di una applicazione e puo' essere customizzato per ottenere un'applicazione
- Un Application Framework e' uno specifico insieme di classi che cooperano strettamente tra loro e che insieme sono riusabili per una categoria di problemi
- Quando uso un Framework, riuso la parte principale del framework, e scrivo solo il codice che viene chiamato

### Tipi di application frameworks
- Infrastruttura di sistema
	- supportano lo sviluppo di infrastrutture, quali comunicazioni, interfacce utente, compilatori, ...
- Integrazione di middleware
	- insiemi di standard e classi di oggetti associate che supportano lo scambio di informazioni
- Applicazioni aziendali
	- supportano lo sviluppo di applicazioni per specifici domini di applicazioni integrando conoscenza del dominio in ambito telecomunicazioni, ambito finanziario, ...

### Usare i framework
- I framework sono generici e quindi devono esser "estesi" per essere usati
	- aggiungere classi concrete che ereditano operazioni dalle classi astratte del framework
	- aggiungere metodi (callbacks) che vengano chiamati in risposta ad eventi riconosciuti dal framework
- Il problema dei framework e' la loro complessita': significa che serve del tempo per imparare a usarli in modo efficace