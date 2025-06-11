
 1. **Quali regole fondamentali governano il modello di permessi tradizionale UNIX?**
    Il modello di permessi tradizionale di UNIX si basa su un sistema di controllo degli accessi che divide gli utenti in tre categorie principali: il proprietario del file (user), il gruppo di appartenenza (group) e gli altri utenti (others). Ogni file e directory ha associato un insieme di permessi specifici per queste tre categorie, rappresentati da nove bit suddivisi in terzetti: lettura (r), scrittura (w) ed esecuzione (w).
    Il **proprietario** ha un controllo quasi completo sui propri file, mentre il root (l'utente con UID 0) ha potere assoluto e puo' modificare qualsiasi file o processo sul sistema.
    Questo modello semplice ma efficace garantisce che solo gli utenti autorizzati possano accedere o modificare file e directory, offrendo un primo livello di sicurezza e controllo.

---

2. **Quali operazioni puo' eseguire solo il proprietario di un file (o root) e quali bit di permesso possono essere impostati su un file?**
   Il proprietario di un file o root puo' modificare i permessi di accesso al file (con `chmod`) e cambiare la proprieta' (con `chown`). Queste operazioni consentono di stabilire chi puo' leggere, scrivere o eseguire un file.
   I bit di permesso sono nove e rappresentano: lettura (r), scrittura (w) ed esecuzione (x). 
   Questi bit vengono applicati in tre categorie: utente proprietario, gruppo e altri utenti. Grazie a questa flessibilita', e' possibile definire accessi personalizzati e proteggere i dati i modo granulare.

---

3. **Quali operazioni puo' eseguire solo il proprietario di un processo (o root) e quali identita' sono associate a un processo?**
   Il proprietario di un processo (o root) ha la possibilita' di inviare segnali al processo (ad esempio `kill`), modificare la sua priorita' (con `nice` o `renice`) o terminarlo. 
   Ogni processo e' caratterizzato da tre tipi di identita':
	- **UID e GID reali**, che identificano l'utente e il gruppo che hanno creato il processo
	- **UID e GID effettivi**, che determinano i privilegi effettivi del processo quando accede alle risorse di sistema
	- **UID e GID salvati**, che permettono di tornare temporaneamente a privilegi elevati, se necessario.
	Questa struttura consente un controllo preciso sulle azioni consentite ai processi, garantendo sicurezza e stabilita' al sistema.

--- 

4. **Che cos'e' l'esevuzione set-UID, perche' passwd ne ha bisogno e cosa succede quando un utente esegue passwd?**
   Il bit **set-UID** e' un permesso speciale che, quando abilitato su un file eseguibile, permette al processo di essere eseguito con i privilegi del proprietario del file, piuttosto che con quelli dell'utente che lo avvia.
   Il comando **passwd** ha bisogno di questo biot perche' deve modificare `/etc/shadow`, un file protetto e accessibile solo a roo.
   Quando un utente normale esegue `passwd`, il kernel imposta l'**UID effettivo** del processo su root, consentendo la modifica sicura della propria password. In questo modo, l'utente puo' cambiare la password senza avere accesso diretto ai file di sistema protetti.

---

5. **Perché sudo è generalmente preferito al login diretto come root o all'uso di su per ottenere privilegi di root, e quali sono i suoi principali vantaggi e svantaggi?**
   **Sudo** e' generalmente preferito perche' consente a un utente di eseguire solo i comandi specifici necessari con privilegi elevati, invece di ottenere una shell completa come root (come con `su` o login diretto).
   I principali **vantaggi** sono:
	- Non e' necessario conoscere la password di root
	- Si possono limitare i privilegi ai soli comandi autorizzati
	- Sudo ritiene dei comandi eseguiti, aumentando la trasparenza e la sicurezza
	- Riduce il rischio di errori o abusi accidentali
	**Svantaggi**:
	- Se un account sudoer viene compromesso, e' come compromettere root
	- Una configurazione di sudo errata puo' creare rischi simili a un accesso diretto come root.

---

6. **Quali tipi di file supporta UNIX e come i nove bit di permesso (``rwx`` per utente, gruppo e altri) regolano le operazioni consentite su ciascun tipo?**
   UNIX supporta diversi tipi di file:
	- File regolari (-)
	- Directory (d)
	- Collegamenti simbolici (I)
	- File di dispositivo a caratteri o a blocchi (c, b)
	- Socket (s)
	- Pipe (p)
	
	I nove bit di permesso regolano
	- File regolari $\rightarrow$ lettura, scrittura o esecuzione del contenuto
	- Directory $\rightarrow$ lettura dell'elenco dei file, scrittura per aggiungere o rimuovere file e esecuzione per entrare nella directory.
	  Questa flessibilita' consente di controllare in dettaglio le operazioni consentite a ciascun tipo di utente.

---

7. **Perché un 'lazy unmount' (unmount -l) è considerato non sicuro, quale comando permette di identificare i processi che mantengono riferimenti al filesystem occupato e come si può effettuare un 'unmount' pulito?**
   Il **lazy unmount** (`unount -l`) e' considerato non sicuro perche' rimuove immediatamente il filesystem dalla gerarchia dei nomi, ma lascia i file aperti ai processi che li stavano gia' utilizzando. Questo puo' causare inconsistenze e rischi di corruzione dei dati. 
   Per vedere quali processi mantengono riferimenti al filesystem, si usa il comando: `fuser -m /punto/di/montaggio`.
   Per uno smontaggio sicuro e pulito, e' meglio usare: `unmount /punto/di/montaggio`

---

8. **Quali sono gli scopi dei bit set-UID, set-GID e sticky, a quali file regolari o directory si applicano e come modificano i controlli sui permessi?**
	- **set-UID (s)**: permette a un file eseguibile di essere eseguito con i privilegi del proprietario, usato ad esempio da passwd
	- **set-GID (s)**: su file eseguibili, fa eseguire il processo con il GID del file; sulle directory, fa ereditare il gruppo ai file creati.
	- **Sticky bit (t)**: sulle directory, impedisce la rimozione di file a chi non e' proprietario o root. Tipico su `/tmp` per evitare che gli utenti elimino file di altri.

---

9. **Chi può modificare i bit di permesso di un file, quale comando può utilizzare e come viene invocato quel comando?**
   I permessi possono essere modificati dal proprietario o da root usando 
   `chmod`: `chmod 755 file.txt`.
   Oppure in modo mnemonico: `chmod u+rwx,g+rx,o+rx file.txt`

---

10. **Chi può modificare la proprietà (o il gruppo) di un file, quali regole devono essere soddisfatte e quale comando esegue l'operazione?** 
    Solo root puo' modificare il proprietario con `chown`. Un utente normale puo' cambiare il gruppo solo se fa parte del gruppo stesso.
    Comando per cambiare proprieta' del gruppo: `chown nuovo_utente:nuovo_gruppo file` 

---

11. **Che cos'è l'ARP spoofing, quali debolezze del protocollo ARP sfrutta e come si svolge in pratica un attacco MITM?**
    L'ARP spoofing consiste nell'invio di pacchetti ARP falsi per associare il proprio MAC a un IP legittimo.
    Debolezza: ARP non ha meccanismi di autenticazione.
    Un attaccante puo' ingannare sia la vittima che il gateway, intercettando e manipolando il traffico in un attacco Man-In-The-Middle (MITM).

---

12. **Come può un attaccante sferrare un attacco MITM con i messaggi di reindirizzamento ICMP e quali sono le debolezze del protocollo ICMP che lo rendono possibile?**

---

13. **Che cos'è l'IP forwarding e perché di solito non è sicuro lasciarlo abilitato sugli host che non sono destinati ad agire come router?**
	- Un host dotato di inoltro IP può agire come router. Ciò significa che può accettare pacchetti di terze parti su un'interfaccia di rete, abbinarli a un gateway o a un host di destinazione su un'altra interfaccia di rete e ritrasmettere i pacchetti.
	- Gli host che inoltrano i pacchetti possono talvolta essere costretti a compromettere la sicurezza facendo apparire i pacchetti esterni come provenienti dall'interno della rete, in modo da eludere gli scanner di rete e i filtri dei pacchetti.

---

14. **Che cos'è lo spoofing IP e quali difese si possono usare contro di esso?**
- L'indirizzo sorgente di un pacchetto IP è normalmente compilato dall'implementazione TCP/IP del kernel e dall'indirizzo IP dell'host da cui è stato inviato il pacchetto. Tuttavia, se il software che crea il pacchetto utilizza socket grezzi, può inserire qualsiasi indirizzo sorgente.
- Le difese che possono essere utilizzate contro di esso sono:
	- Bloccare i pacchetti in uscita il cui indirizzo di origine non rientra nel proprio spazio di indirizzi.
	- Proteggersi dagli aggressori che falsificano l'indirizzo di origine dei pacchetti esterni per ingannare il firewall e fargli credere che provengano dalla rete interna. A questo proposito, un'euristica utile è uRPF.

---

14. **Che cos'è l'IPv4 source routing e come può essere sfruttato da un attaccante?**
    - L'IPv4 source routing fornisce un meccanismo che consente di specificare una serie esplicita di gateway per il transito dei pacchetti verso la destinazione.
	- Il source routing bypassa l'algoritmo di instradamento next-hop che viene eseguito in ogni gateway per determinare come un pacchetto debba essere inoltrato. Qualcuno potrebbe instradare abilmente un pacchetto in modo da farlo apparire come proveniente dalla propria rete anziché da Internet, facendo così passare il firewall.

---

15. **Cosa rappresenta la triade della CIA in materia di sicurezza informatica e cosa significa ciascun principio?**
    La triade CIA sta per CONFIDENTIALITY (riservatezza), INTEGRITY (integrità) e AVAILABILITY (disponibilità). Per riservatezza si intende che l'accesso alle informazioni deve essere limitato solo a coloro che sono autorizzati ad averle; per integrità intendiamo che le informazioni devono essere valide e non alterate in alcun modo non autorizzato, e per disponibilità intendiamo che le informazioni devono essere accessibili agli utenti autorizzati nel momento in cui ne hanno bisogno.

---

16. **Cos'è l'ingegneria sociale, perché è particolarmente difficile difendersi e qual è una forma comune di questo attacco?**
    L'ingegneria sociale è una tecnica di manipolazione psicologica per persuadere persone a compiere azioni e/o rivelare informazioni per causare danno a sè stessi o alla propria azienda. E' particolarmente difficile difendersi in quanto molte volte questi attacchi vengono creati ad hoc e vengono raccolte informazioni private che portano la persona a "cascare nel tranello", come ad esempio la ricezione di un pacco ordinato da internet, ecc. Un attacco comune è il phishing, che inizia con una comunicazione ingannevole, dove gli li aggressori possono inviare email contenenti allegati o link dannosi (spoofing di email e dirottamento di thread) al fine di eseguire codice dannoso sui sistemi delle vittime.

---

17. **Cos'è una vulnerabilità del software, qual è un esempio specifico di tale vulnerabilità e in che modo le pratiche di revisione del codice open source possono contribuire a ridurre queste vulnerabilità?**
    La vulnerabilità del software è un punto debole o un difetto nella progettazione, implementazione o gestione di un sistema che può essere sfruttata per attacchi. Un esempio può essere un buffer overflow: sono dei bug molto pericolosi in quanto gli aggressori potrebbero scoprire se il codice non verifica la dimensione dei dati rispetto a quella del buffer, e quindi causare un overflow e bloccare il programma stesso; dunque gli aggressori potrebbero inserire più dati di quanto la capacità della memoria consente, e causare molti danni. Un codice open source è generalmente più sicuro poiché se è disponibile e aperto a tutti, c'è molta più probabilità che qualcuno trovi una vulnerabilità e la segnali.

---

18. **Qual è la differenza tra un attacco DoS e un attacco DDoS e in che modo questi attacchi influiscono in genere sui sistemi presi di mira?**
    La differenza tra un attacco DoS e un attacco DDoS è il numero di fonti da cui proviene l'attacco. Per DoS infatti si intende Denial of Service, e proviene generalmente da un singolo dispositivo ed è più limitata; l'attacco DDoS invece è un Distributed Denial of Service, quindi ha più fonti e essendo un traffico distribuito può colpire intere reti. Questi attacchi influiscono sul sistema rendendo siti, reti o server (e così via) inutilizzabili, e quindi interrompendo la disponibilità della vittima.

---

19. **Che cosa sono gli abusi interni e perché spesso sono più difficili da individuare rispetto agli attacchi esterni?**
    Gli abusi interni sono un tipo di problematica legato all'abuso dei permessi all'interno di un'azienda o comunque della rete in cui operiamo il nostro sistema; ad esempio, i dipendenti di un'azienda possono divulgare informazioni sensibili sull'azienda stessa o rubare i dati per avere un guadagno personale. Sono molto più difficili da individuare poiché generalmente un amministratore di rete prevede misure di sicurezza e di riparo da parte di fonti esterne e non interne, in quanto si vuole rendere disponibile la rete interna e garantire accesso agli utenti autorizzati senza barriere.

---

20. **Che cosa si intende per backup nel contesto della sicurezza informatica e quali sono i consigli chiave per una gestione efficace dei backup?**
    Un backup è una copia dei dati del proprio dispositivo, che viene conservato al fine di poterli ripristinare semmai dovessero esserci dei problemi di perdita di dati. E' molto importante effettuare un backup sicuro ed efficace, e per far in modo che questo avvenga, dobbiamo salvare il backup in un dispositivo esterno, monitorare dove viene salvato e criptarlo, per evitare che dei dati sensibili vengano divulgati e presi da terze parti con fine malevolo.

---

21. **Cosa sono i virus e i worm informatici e quali sono le principali differenze tra questi due tipi di malware?**
    Un virus è un malware che quando eseguito modifica i programmi del nostro computer/dispositivo, inserendo il proprio codice e replicandosi. Un worm invece è un malware che si replica e si diffonde in altri computer/dispositivi, e non richiedono programmi host.

---

22. **Cos'è un rootkit, come funziona in genere e perché può essere particolarmente difficile rilevarlo e rimuoverlo?**
    Un rootkit è un software progettato per consentire l'accesso ad un computer o ad una parte del suo software che altrimenti non sarebbe consentito. Questo rootkit spesso maschera la sua esistenza e ne esistono vari tipi, e sono particolarmente difficili da rilevare ed eliminare proprio per questo motivo.

---

23. **Quali sono le best practice e i consigli per creare password sicure, gestirle in modo efficace e implementare l'autenticazione a più fattori?**
    Per creare delle password sicure, dobbiamo alternare sequenze randomiche di lettere, simboli e numeri, e non utilizzare parole molto semplici da scoprire e/o qualcosa di riconducibile alla nostra persona (es. mi chiamo Mario e ho 22 anni e la mia password è Mario22). Un metodo efficace è lasciar generare la nostra password da un programma esterno, oppure utilizzare una passphrase, dunque una frase che noi e solo noi possiamo ricordare. Inoltre implementare l'autenticazione a più fattori è molto utile in quanto previene accessi anomali e non registrati ai nostri account o dispositivi.

---

24. **Cos'è la crittografia a chiave simmetrica, come funziona e quali sono i suoi principali vantaggi e svantaggi?**
    Nella crittografia a chiave simmetrica abbiamo due utenti che condividono una chiave segreta che viene utilizzata per crittografare e decrittografare i messaggi.

---

25. **Quando entrambi conoscono la chiave, possono utilizzarla per scambiarsi messaggi, ma una terza persona potrebbe entrare in questo scambio conoscendo la chiave segreta.**
    E' spesso utilizzata poiché le chiavi simmetriche sono efficienti per quanto riguarda la CPU e veloci da verificare.

---

26. **Cos'è la crittografia a chiave pubblica, come funziona e quali sono i suoi principali vantaggi e svantaggi?**
    Nella crittografia a chiave pubblica, vengono generate un paio di chiavi da parte dell'utente A, quella segreta e quella pubblica: quella segreta rimane privata, e invece quella pubblica possono conoscerla tutti; lo stesso accade per l'utente B. Quando A vuole mandare un messaggio a B, lo crittografa con la chiave pubblica di B; B, che ha la sua chiave privata, è l'unica che può decrittografarla. I vantaggi sono che la chiave pubblica può essere condivisa liberamente, consentendo la crittografia di messaggi, ma è più lenta e complessa rispetto alla crittografia a chiave segreta.

---

27. **Cos'è una firma digitale, qual è il suo scopo e come può essere creata utilizzando la crittografia a chiave pubblica?**
    Una firma digitale è un processo che consente di verificare colui che manda un messaggio, al fine di verificare l'autenticità. Può essere creata firmando il messaggio con la propria chiave privata, e quindi il destinatario userà la chiave pubblica del mittende per validarne l'autenticità.

---

28. **Cos'è un certificato digitale, a cosa serve e come si ottiene in genere?**
    Un certificato digitale è un documento elettronico utilizzato per dimostrare la validità di una chiave pubblica: include quest'ultima e le informazioni relative a essa, come informazioni sull'identità del suo proprietario e la firma digitale di una CA (autorità di certificazione) che ha verificato il contenuto del certificato.

---

29. **Che cos'è una funzione hash e quali proprietà specifiche definiscono una funzione hash crittografica?**
    Una funzione hash è una funzione che accetta un input di qualsiasi lunghezza e genera un valore di misura prestabilita che viene derivata dal dato di input. Le proprietà che definiscono una funziona hash crittografica sono l'entanglement, che ci dice come ogni bit del nostro prodotto della hash function (digest) dipende dai bit dell'input; la pseudo-randomness che ci informa su quanto randomico è il nostro prodotto, e la non reversibility, ovvero la proprietà che ci dice quanto difficile poter risalire al nostro input avendo davanti un digest.

---

30. **Cos'è un firewall, come funziona uno schema di filtraggio firewall a due stadi e quale ruolo svolge una DMZ?**
    Un firewall è un sistema che monitora e controlla il traffico di rete seguendo delle regole ben precise e stabilite, al fine di proteggere la rete da intrusi. Uno schema di filtraggio firewall a due strati consiste nell'utilizzare due diversi direwall o di due livelli di filtraggio diverso. Nel primo step, un firewall esterno filtra il traffico tra internet e la DMZ (Demilitarized Zone), permettendo quindi solo le connessioni stabilite; nel secondo step, un altro firewall separa la DMZ dalla rete interna (es. rete aziendale) e applica regole più restrittive per scongiurare ancora di più attacchi alla rete. La DMZ è una zona isolata tra i firewall dove vengono inseriti i server accessibili da internet (es. posta elettronica) e serve per garantire una zona sicura per ospitare dei servizi potenzialmente raggiungibili al pubblico, dunque delimitando la vera e propria rete aziendale dai servizi esterni di cui ad esempio un'azienda fa uso.