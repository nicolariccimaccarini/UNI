### [Controllo degli Accessi e Poteri di Root](8-access-control-and-rootly-powers.md)

- ==Quali regole fondamentali governano il modello tradizionale di permessi UNIX?==
    - Il modello standard di controllo degli accessi UNIX è rimasto sostanzialmente invariato per decenni. Questo modello segue alcune regole di base:
          1. Le decisioni di controllo degli accessi dipendono da quale utente (o, in alcuni casi, l'appartenenza dell'utente a un gruppo) sta tentando di eseguire un'operazione.
          2. Gli oggetti hanno proprietari. File e processi sono esempi di oggetti. I proprietari hanno un controllo ampio, ma non necessariamente illimitato, sui loro oggetti.
          3. Possiedi gli oggetti che crei.
          4. L'account utente speciale `root` può agire come proprietario di qualsiasi oggetto
          5. Solo `root` può eseguire certe operazioni amministrative sensibili.

- ==Quali operazioni può eseguire solo il proprietario del file (o `root`), e quali bit di permesso possono essere impostati su un file?==
    - Solo il proprietario del file (o `root`) può impostare i permessi del file. I bit di permesso che possono essere impostati sono:
        - **Lettura (`r`)**: Permesso di leggere il contenuto del file
        - **Scrittura (`w`)**: Permesso di modificare o eliminare il file
        - **Esecuzione (`x`)**: Permesso di eseguire il file (per gli eseguibili) o entrare nella directory
        - **Set-UID**: Eseguire l'eseguibile con i permessi del proprietario
        - **Set-GID**: Eseguire l'eseguibile con i permessi del gruppo proprietario
        - **Sticky bit**: (Per le directory) Solo il proprietario del file, proprietario della directory, o `root` può eliminare i file

- ==Quali operazioni può eseguire solo il proprietario del processo (o `root`), e quali identità sono associate a un processo?==
    - Il proprietario di un processo può:
        - Inviare segnali al processo
        - Ridurre la priorità di scheduling del processo (aumentare la niceness)
    - Ci sono multiple identità associate a un processo:
        - **Real UID e GID**: Chi siamo realmente; presi da `/etc/passwd` al login e tipicamente non cambiano durante una sessione di login (`root` potrebbe cambiarli)
        - **Effective UID, effective GID e supplementary GIDs**: Usati per i controlli dei permessi di accesso ai file; determinano i permessi effettivi di accesso ai file
        - **Saved IDs**: Usati per entrare e uscire dalla modalità privilegiata; saved set-UID contiene una copia dell'effective UID e saved set-GID contiene una copia dell'effective GID

- ==Cos'è l'esecuzione set-UID, perché `passwd` ne ha bisogno, e cosa succede quando un utente normale esegue `passwd`?==
    - **Esecuzione set-UID**: Quando il kernel esegue un file eseguibile con il bit set-UID impostato, cambia l'effective UID del processo al proprietario del file invece dell'UID dell'utente che ha eseguito il comando.
    - **Perché `passwd` ne ha bisogno**: Il comando `passwd` deve modificare `/etc/shadow` (che è scrivibile solo da `root`) per cambiare le password degli utenti. Gli utenti normali non hanno il permesso di scrivere in questo file.
    - **Cosa succede quando un utente normale esegue `passwd`**:
        1. L'utente `ubuntu` esegue `passwd` (che è posseduto da `root` e ha il bit set-UID impostato)
        2. Il processo viene eseguito con effective UID = 0 (`root`) invece di 1000 (`ubuntu`)
        3. Il processo può ora leggere e scrivere `/etc/shadow` con privilegi di `root`
        4. L'utente può cambiare la sua password senza avere accesso diretto al file shadow

- ==Perché `sudo` è generalmente preferito al login diretto di `root` o `su` per ottenere privilegi di `root`, e quali sono i suoi principali vantaggi e svantaggi?==
    - **Come funziona `sudo`**:
        1. `sudo` guarda in `/etc/sudoers`, che elenca le persone autorizzate a usare `sudo` e i comandi che possono eseguire su ogni host.
        2. Se il comando è permesso, `sudo` chiede la **password dell'utente** (non la password di root)
        3. Se la password è corretta, `sudo` esegue il comando
    - **Vantaggi e svantaggi**:

| Pro                                                                                          | Contro                                                                                             |
| -------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| Logging dei comandi                                                                              | Il logging dei comandi può essere facilmente aggirato (`sudo su`, anche se `sudo su` verrebbe almeno registrato) |
| Gli utenti possono fare cose che richiedono privilegi di root senza avere privilegi di root illimitati    |                                                                                                 |
| Gli utenti non devono conoscere la password di `root` perché `sudo` chiede la password dell'utente | Qualsiasi violazione della sicurezza dell'account personale di un sudoer può essere equivalente a violare `root` |
| Più veloce sia di `su` che del login di `root`                                                         |                                                                                                 |
| I privilegi possono essere revocati senza cambiare la password di `root`                                |                                                                                                 |
| Viene mantenuta una lista di tutti gli utenti con privilegi di `root`                                     |                                                                                                 |
| Minore possibilità di una shell di `root` lasciata incustodita                                               |                                                                                                 |
| Un singolo file può controllare l'accesso per un'intera rete                                       |                                                                                                 |

### [Il Filesystem](10-the-filesystem.md)

- ==Quali tipi di file supporta UNIX, e come i nove bit di permesso (`rwx` per utente, gruppo e altri) governano le operazioni consentite su ogni tipo?== 
    - **Tipi di file UNIX**:
        - File regolare → Simbolo `-`
        - Directory → Simbolo `d`
        - Link simbolico → Simbolo `l`
        - File di dispositivo a caratteri → Simbolo `c`
        - File di dispositivo a blocchi → Simbolo `b`
        - Named pipe → Simbolo `p`
        - Socket di dominio locale → Simbolo `s`
    - **Nove bit di permesso**: Tre triplette di bit di permesso definiscono l'accesso per il proprietario (utente), i proprietari del gruppo e tutti gli altri. Ogni tripletta ha bit di lettura (`r`), scrittura (`w`) ed esecuzione (`x`):

| Tipo di file | `r`               | `w`                                                                   | `x`     |
| --------- | ----------------- | --------------------------------------------------------------------- | ------- |
| `-`       | Leggere              | Scrivere                                                                 | Eseguire |
| `d`       | Elencare i contenuti | Creare, eliminare e rinominare file (funziona solo in combinazione con `x`) | Entrare   |
| `l`       | n/a               | n/a                                                                   | n/a     |
| `c`       | Leggere              | Scrivere                                                                 | n/a     |
| `b`       | Leggere              | Scrivere                                                                 | n/a     |
| `p`       | Leggere              | Scrivere                                                                 | n/a     |
| `s`       | Leggere              | Connettersi e scrivere                                                     | n/a     |
  
- ==Perché un unmount lazy (`umount -l`) è considerato non sicuro, quale comando permette di identificare i processi che mantengono ancora riferimenti al filesystem occupato, e come si può eseguire invece un unmount pulito?==
    - **Perché l'unmount lazy non è sicuro**: 
        - Non c'è garanzia che i riferimenti esistenti si chiudano mai da soli
        - I filesystem smontati in modo lazy presentano semantiche inconsistenti ai programmi che li stanno usando
    - **Comando per identificare i processi**: `fuser -v -m /percorso/al/filesystem` mostra quali processi stanno usando il filesystem montato
    - **Unmount pulito**: Prima identificare e fermare i processi che usano il filesystem, poi usare il normale `umount /percorso/al/filesystem`
  
- ==Quali sono gli scopi dei bit set-UID, set-GID e sticky, a quali file regolari o directory si applicano ciascuno, e come alterano i controlli dei permessi?==
    - **Bit Set-UID**:
        - Si applica a: Solo file regolari
        - Scopo: Eseguire un file eseguibile con i permessi del proprietario invece dell'utente che lo ha eseguito
    - **Bit Set-GID**:
        - Per file regolari: Eseguire un file eseguibile con i permessi del gruppo proprietario
        - Per directory: I file appena creati assumono la proprietà del gruppo della directory piuttosto che il gruppo predefinito dell'utente che ha creato il file
    - **Sticky bit**:
        - Si applica a: Solo directory
        - Scopo: Se impostato su una directory, un file può essere eliminato o rinominato solo dal proprietario della directory, dal proprietario del file, o da `root`
  
- ==Chi può cambiare i bit di permesso di un file, quale comando può usare, e come viene invocato quel comando?==
    - **Chi può cambiare**: Solo il proprietario del file e `root` possono cambiare i permessi del file
    - **Comando**: `chmod` (change mode)
    - **Invocazione**: Il primo argomento specifica i permessi (sintassi ottale o mnemonica), seguito dai nomi dei file
        - Esempio ottale: `chmod 755 nomefile` 
        - Esempio mnemonico: `chmod u+x,g-w nomefile`
  
- ==Chi può cambiare la proprietà (del gruppo) di un file, quali regole devono essere soddisfatte, e quale comando esegue l'operazione?==
    - **Chi può cambiare**: Solo `root` può cambiare la proprietà del file. Gli utenti normali possono cambiare solo la proprietà del gruppo sotto condizioni specifiche.
    - **Regole per il cambio di proprietà del gruppo**: 
        - Deve essere il proprietario del file E appartenere al gruppo target (o essere `root`)
        - Non può cedere la proprietà dei file (eccetto `root`)
    - **Comando**: `chown utente:gruppo nomefile` per cambiare sia proprietario che gruppo, o `chown :gruppo nomefile` per cambiare solo il gruppo

### [Networking](11-networking.md)

- ==Cos'è l'ARP spoofing, quali debolezze del protocollo ARP sfrutta, e come si sviluppa in pratica un attacco MITM?==
    - **ARP spoofing**: Un attacco dove l'attaccante invia risposte ARP false per associare il proprio indirizzo MAC con l'indirizzo IP di un altro host (tipicamente il gateway).
    - **Debolezze del protocollo ARP**:
        - ARP è un protocollo stateless - le risposte ARP vengono automaticamente memorizzate nella cache indipendentemente dal fatto che seguano una richiesta ARP
        - Non c'è autenticazione in ARP
        - Chiunque può inviare risposte ARP non richieste che riscrivono la cache della vittima con informazioni false
    - **Processo di attacco MITM**:
        1. L'attaccante invia una risposta ARP malevola alla vittima: "Io sono 10.1.1.1 (gateway) a MAC_attaccante"
        2. La vittima aggiorna la cache ARP, ora pensando che il MAC dell'attaccante sia il gateway
        3. L'attaccante invia una risposta ARP al gateway: "Io sono 10.1.1.10 (vittima) a MAC_attaccante"  
        4. Tutto il traffico tra vittima e gateway ora fluisce attraverso l'attaccante
        5. L'attaccante può intercettare, modificare o analizzare tutte le comunicazioni
  
- ==Come può un attaccante montare un attacco MITM con messaggi ICMP redirect, e quali debolezze del protocollo ICMP rendono questo possibile?==
    - **Attacco MITM ICMP redirect**:
        - L'attaccante invia un messaggio ICMP redirect malevolo alla vittima fingendo di provenire da un host esterno
        - Il messaggio di redirect dice alla vittima: "dovresti inviare i pacchetti per l'host X a me invece che al gateway corrente"
        - Il sistema della vittima aggiusta automaticamente la sua tabella di routing di conseguenza
        - Il traffico futuro viene instradato attraverso l'attaccante, permettendo l'intercettazione
    - **Debolezze del protocollo ICMP**:
        - **Mancanza di Autenticazione**: I redirect ICMP non contengono informazioni di autenticazione, non c'è modo di verificare che il messaggio provenga da un router legittimo
        - **Elaborazione Automatica**: Quando un host riceve un redirect ICMP, aggiusta automaticamente la sua tabella di routing senza ulteriori verifiche

- ==Cos'è l'IP forwarding, e perché è solitamente non sicuro lasciarlo abilitato su host che non sono destinati ad agire come router?==
    - **IP forwarding**: Un host con IP forwarding abilitato può agire come router, può accettare pacchetti di terze parti su un'interfaccia di rete, abbinarli a un gateway o host di destinazione su un'altra interfaccia di rete, e ritrasmettere i pacchetti.
    - **Perché non è sicuro**: 
        - Gli host che inoltrano pacchetti possono essere costretti a compromettere la sicurezza facendo apparire i pacchetti esterni come provenienti dall'interno della rete
        - Questo può eludere scanner di rete e filtri di pacchetti
        - Può essere sfruttato attraverso ARP spoofing e ICMP redirect per reindirizzare il traffico attraverso host compromessi
        - A meno che il sistema non sia destinato a funzionare come router, l'IP forwarding dovrebbe essere disabilitato
  
- ==Cos'è l'IP spoofing, e quali difese possono essere usate contro di esso?==
    - **IP spoofing**: La pratica di creare pacchetti IP con un indirizzo IP sorgente falso. Normalmente il kernel riempie l'indirizzo sorgente, ma con raw socket, il software può specificare qualsiasi indirizzo sorgente.
    - **Difese**:
        - **Filtraggio in uscita (Egress filtering)**: Bloccare i pacchetti in uscita il cui indirizzo sorgente non è all'interno del vostro spazio di indirizzi
        - **Filtraggio in entrata (Ingress filtering)**: Proteggere contro attaccanti che falsificano pacchetti esterni per apparire interni
        - **uRPF (Unicast Reverse Path Forwarding)**: Euristica che aiuta a verificare gli indirizzi sorgente dei pacchetti controllando se la sorgente è raggiungibile tramite l'interfaccia su cui è arrivata
  
- ==Cos'è il source routing IPv4, e come può un attaccante sfruttarlo?==
    - **Source routing IPv4**: Un meccanismo per specificare una serie esplicita di gateway che un pacchetto deve attraversare sulla strada verso la sua destinazione, bypassando gli algoritmi di routing next-hop normali.
    - **Sfruttamento dell'attaccante**:
        - L'attaccante può instradare astutamente un pacchetto per farlo apparire come originato all'interno della vostra rete interna invece che da Internet
        - Questo permette al pacchetto di superare i firewall che bloccano il traffico esterno
        - Può essere usato per bypassare misure di sicurezza facendo apparire gli attacchi esterni come traffico interno
        - La maggior parte dei sistemi scarta i pacchetti source-routed per default come misura di sicurezza

### [Sicurezza](12-security.md)

- ==Cosa rappresenta la triade CIA nella sicurezza informatica, e cosa significa ciascun principio?==
    - **Confidenzialità (C)**: L'accesso alle informazioni dovrebbe essere limitato a coloro che sono autorizzati ad averle (privacy dei dati)
    - **Integrità (I)**: Le informazioni sono valide e non sono state alterate in modi non autorizzati (autenticità e affidabilità delle informazioni)
    - **Disponibilità (A)**: Le informazioni devono essere accessibili agli utenti autorizzati quando ne hanno bisogno; altrimenti, i dati non hanno valore. Anche le interruzioni non causate da intrusi rientrano nella categoria dei problemi di disponibilità.
  
- ==Cos'è l'ingegneria sociale, perché è particolarmente difficile da difendere, e qual è una forma comune di questo attacco?==
    - **Ingegneria sociale**: L'uso dell'influenza psicologica per persuadere le persone a compiere azioni o rivelare informazioni. Sfrutta i fattori umani piuttosto che le vulnerabilità del software con obiettivi di raccolta informazioni, frode o accesso al sistema.
    - **Perché difficile da difendere**: Gli utenti umani (e amministratori) sono gli anelli più deboli nella catena di sicurezza e nessuna quantità di tecnologia può proteggere contro l'elemento umano.
    - **Forma comune**: **Phishing**, gli attaccanti ingannano le persone per far eseguire codice malevolo o rivelare informazioni sensibili attraverso comunicazioni ingannevoli (email, siti web, ecc.).
  
- ==Cos'è una vulnerabilità software, qual è un esempio specifico di tale vulnerabilità, e come possono le pratiche di revisione del codice open-source aiutare a ridurre queste vulnerabilità?== 
    - **Vulnerabilità software**: Un difetto o debolezza nel design, implementazione o gestione di un sistema che può essere sfruttato da un attaccante per compromettere la sua sicurezza.
    - **Esempio specifico**: **Buffer overflow**, quando un programma scrive più dati in un buffer di memoria di quanti ne possa contenere, potenzialmente permettendo agli attaccanti di eseguire codice arbitrario.
    - **Revisione del codice open-source**: Il codice open-source porta a una migliore sicurezza perché più persone possono scrutinare il codice, maggiore è la possibilità che qualcuno individui una debolezza di sicurezza ("molti occhi rendono tutti i bug superficiali").
  
- ==Qual è la differenza tra un attacco DoS e un attacco DDoS, e come questi attacchi tipicamente impattano i sistemi presi di mira?==
    - **Attacco DoS**: Attacco Denial of Service da una singola sorgente che mira a sopraffare un sistema, rendendolo non disponibile agli utenti legittimi.
    - **Attacco DDoS**: Attacco Distributed Denial of Service che usa multiple sorgenti (botnet) per sopraffare il sistema target.
    - **Implementazione DDoS**: Gli attaccanti piantano codice malevolo su dispositivi non protetti fuori dalla rete della vittima, creando una "botnet" che può essere comandata remotamente per attaccare il target.
    - **Impatto**: Entrambi rendono i sistemi non disponibili agli utenti previsti sovraccaricando le risorse (larghezza di banda di rete, CPU, memoria) o sfruttando vulnerabilità per causare crash.
  
- ==Cos'è l'abuso da insider, e perché è spesso più difficile da rilevare degli attacchi esterni?==
    - **Abuso da insider**: Quando dipendenti, contractor e consulenti abusano del loro status di fiducia e privilegi speciali per rubare/rivelare dati, interrompere sistemi per guadagno finanziario, o creare caos per ragioni politiche.
    - **Perché più difficile da rilevare**: 
        - Gli insider sono agenti fidati con accesso legittimo
        - La maggior parte delle misure di sicurezza proteggono contro minacce esterne, non interne
        - Le azioni degli insider possono apparire come normali attività autorizzate
        - Spesso il tipo di attacco più difficile da rilevare e prevenire
  
- ==Cos'è un backup nel contesto della sicurezza informatica, e quali sono le raccomandazioni chiave per gestire efficacemente i backup?==
    - **Backup**: Una copia dei dati del computer presa e memorizzata altrove in modo che possa essere usata per ripristinare l'originale dopo un evento di perdita dati. I backup regolari e testati del sistema sono una parte essenziale di qualsiasi piano di sicurezza del sito e rientrano nel bucket "disponibilità" della triade CIA.
    - **Raccomandazioni chiave**:
        - **Tutti i filesystem sono replicati**: Assicurare copertura completa
        - **Alcuni backup sono memorizzati off-site**: Proteggere contro disastri locali
        - **Testare regolarmente i backup**: Verificare che possano effettivamente essere ripristinati
        - **Crittografare i file di backup**: Proteggere i dati sensibili nei backup
        - **Limitare e monitorare l'accesso**: Controllare chi può accedere ai sistemi di backup
  
- ==Cosa sono i virus e i worm informatici, e quali sono le differenze chiave tra questi due tipi di malware?==
    - **Virus**: Un tipo di malware che, quando eseguito, si replica modificando programmi informatici e inserendo il proprio codice in quei programmi. **Richiede un programma host** per diffondersi.
    - **Worm**: Malware autonomo che si replica per diffondersi ad altri computer. **Non richiede programmi host** e può diffondersi indipendentemente attraverso le reti.
    - **Differenza chiave**: I virus hanno bisogno di file host da infettare, mentre i worm sono autonomi e si diffondono in modo indipendente.
  
- ==Cos'è un rootkit, come funziona tipicamente, e perché può essere particolarmente difficile da rilevare e rimuovere?==
    - **Rootkit**: Software, tipicamente malevolo, progettato per abilitare l'accesso a un computer o area del suo software che altrimenti non sarebbe consentito, spesso mascherando la sua esistenza o l'esistenza di altro software.
    - **Come funziona**: Opera a basso livello (kernel, boot loader, ecc.) per nascondere la sua presenza e mantenere accesso persistente al sistema.
    - **Perché difficile da rilevare/rimuovere**: 
        - I rootkit più avanzati sono consapevoli dei programmi di rimozione comuni e li sovvertono attivamente
        - Possono nascondersi a livelli di sistema molto bassi rendendo difficile la rilevazione
        - Potrebbe essere meglio cancellare il sistema e ripristinare da backup puliti piuttosto che tentare la rimozione
  
- ==Quali sono le migliori pratiche e raccomandazioni per creare password sicure, gestire le password efficacemente, e implementare MFA?==
    - **Creazione di password sicure**: 
        - Tecnicamente, la password più sicura consiste in una sequenza casuale di lettere, punteggiatura e cifre
        - Usare lunghe passphrase facili da ricordare ma difficili da indovinare
        - Mai usare la stessa password per scopi multipli
    - **Gestione delle password**: 
        - Usare un **password vault** - software che cripta le password memorizzate e richiede solo una singola password master per accedere
        - Scrivere le password se necessario, ma assicurarsi che non siano accessibili alle persone sbagliate
    - **Implementazione MFA**: 
        - I sistemi dovrebbero validare l'identità attraverso sia qualcosa che conosci (password/passphrase) che qualcosa che hai (dispositivo fisico, impronta digitale, ecc.)
        - Ora un requisito minimo assoluto per qualsiasi portale rivolto a Internet con privilegi amministrativi
  
- ==Cos'è la crittografia a chiave simmetrica, come funziona, e quali sono i suoi vantaggi e svantaggi principali?==
    - **Come funziona**: Alice e Bob condividono una chiave segreta ($K_AB$) che usano per crittografare e decrittografare messaggi. Devono prima trovare un modo per scambiare il segreto condiviso privatamente. Una volta che entrambi conoscono la chiave, possono riutilizzarla finché vogliono.
    - **Vantaggi**: 
        - Relativamente efficiente in termini di uso CPU e dimensione del payload crittografato
        - Spesso usata dove è necessaria crittografia/decrittografia efficiente
    - **Svantaggi**: 
        - **Problema di distribuzione delle chiavi**: Necessità di scambiare sicuramente la chiave segreta in anticipo
        - L'unico modo completamente sicuro è incontrarsi di persona senza interferenze
  
- ==Cos'è la crittografia a chiave pubblica, come funziona, e quali sono i suoi vantaggi e svantaggi principali?==
    - **Come funziona**: Ogni parte genera una coppia di chiavi (pubblica/privata). Le chiavi pubbliche possono essere ampiamente conosciute mentre le chiavi private rimangono segrete. Per inviare un messaggio a Bob, Alice critta con la chiave pubblica di Bob, e solo Bob può decrittare con la sua chiave privata.
    - **Vantaggi**: 
        - Risolve il problema di distribuzione delle chiavi - non c'è bisogno di condividere chiavi segrete in anticipo
        - Abilita firme digitali per l'autenticazione
    - **Svantaggi**: 
        - Molto più lenta della crittografia simmetrica
        - Le caratteristiche di performance la rendono impratica per crittografare grandi quantità di dati
        - Spesso accoppiata con crittografia simmetrica (chiave pubblica per scambio chiavi, simmetrica per crittografia bulk)

- ==Cos'è una firma digitale, qual è il suo scopo, e come può essere creata usando la crittografia a chiave pubblica?==
    - **Firma digitale**: Testo crittografato creato usando una chiave privata per verificare l'autenticità e l'integrità di un messaggio.
    - **Scopo**: 
        - **Non-ripudio**: Valida l'autenticità del mittente 
        - **Integrità**: Verifica che il messaggio non sia stato modificato
    - **Creazione con crittografia a chiave pubblica**: 
        - Alice firma il messaggio con la sua chiave privata: $E(K_A^{-1}, M) = S_{K_A^{-1}}$
        - Bob può verificare l'autenticità usando la chiave pubblica di Alice: $D(K_A, S_{K_A^{-1}}) = M$ 
        - In pratica, le firme digitali sono applicate all'hash del messaggio piuttosto che al messaggio completo per efficienza

- ==Cos'è un certificato digitale, quale scopo serve, e come viene tipicamente ottenuto?==
    - **Certificato digitale**: Un documento elettronico usato per provare la validità di una chiave pubblica. Include la chiave pubblica e informazioni su di essa, informazioni sull'identità del proprietario (subject), e la firma digitale di una Certificate Authority (CA) che ha verificato i contenuti (issuer).
    - **Scopo**: 
        - Valida l'autenticità delle chiavi pubbliche su scala Internet
        - Risolve il problema della fiducia nella crittografia a chiave pubblica
        - Previene attacchi man-in-the-middle dove un attaccante sostituisce chiavi pubbliche legittime
    - **Come viene tipicamente ottenuto**: 
        1. L'amministratore invia una **Certificate Signing Request** alla CA
        2. La CA restituisce un **certificato firmato**
        3. L'amministratore installa il certificato firmato e la chiave privata sul server web
        4. Gli utenti possono richiedere il certificato pubblico e verificarne la firma contro il loro trust store locale

- ==Cos'è una funzione hash, e quali proprietà specifiche definiscono una funzione hash crittografica?==
    - **Funzione hash**: Una funzione che accetta dati di input di qualsiasi lunghezza e genera un valore di lunghezza fissa derivato da quei dati. L'output è chiamato hash, digest, checksum o fingerprint.
    - **Proprietà che definiscono una funzione hash crittografica**:
        - **Entanglement**: Ogni bit del digest dipende da ogni bit dell'input. Cambiare un bit dell'input dovrebbe cambiare in media il 50% dei bit del digest
        - **Pseudo-casualità**: I digest dovrebbero apparire come dati casuali senza struttura interna rilevabile e nessuna relazione apparente con i dati di input
        - **Non-reversibilità**: Dato un digest, dovrebbe essere computazionalmente impossibile scoprire un altro input che generi lo stesso digest
    - **Scopo**: Verificare l'integrità dei dati, può verificare che un file di configurazione o un binario non sia stato manomesso

- ==Cos'è un firewall, come funziona uno schema di filtraggio firewall a due stadi, e quale ruolo gioca una DMZ?==
    - **Firewall**: Un dispositivo o software che previene l'accesso di pacchetti indesiderati a reti e sistemi, limitando il tipo di traffico basandosi sulle informazioni nell'header del pacchetto.
    - **Schema di filtraggio firewall a due stadi**: 
        - **Primo filtro**: Agisce come gateway verso Internet
        - **Secondo filtro**: Opera tra il gateway esterno e il resto della rete locale
        - Le connessioni Internet terminano su sistemi amministrativamente separati dal resto della rete
    - **Ruolo della DMZ**: 
        - **DMZ (Zona Demilitarizzata)**: Una sottorete fisica o logica che contiene ed espone i servizi rivolti verso l'esterno dell'organizzazione
        - Fornisce un livello di sicurezza aggiuntivo isolando i servizi pubblici dalla rete interna
        - I servizi accessibili da Internet (come server web) sono posizionati nella DMZ
    - **Approccio di sicurezza**: 
        - Iniziare con una configurazione che non permette connessioni in entrata
        - Liberalizzare gradualmente il filtro man mano che si scoprono servizi necessari
        - Spostare tutti i servizi accessibili da Internet su sistemi nella