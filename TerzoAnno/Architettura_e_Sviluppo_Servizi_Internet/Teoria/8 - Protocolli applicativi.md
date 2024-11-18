Nel contesto delle reti di calcolatori, il termine **protocollo** e' interpretato come un insieme di regole che regola il formato dei messaggi scambiati tra computer.

Tre specifiche fondamentali:
1. Procedure e regole di gestione dei messaggi
2. Processamento di errori
3. Formato dei messaggi

## Gestione dello stato del protocollo
Il progettista di applicazioni distribuite deve affrontare il problema della gestione dello stato del protocollo applicativo.
Nel paradigma di comunicazione a scambio di messaggi, e' utile ragionare in termini di messaggi come eventi che cambiano lo stato del sistema.

Uso di diversi strumenti:
- Reti di Petri
- Macchine a stati finiti
- Linguaggi formali di specifica (es. TLA)
- ecc.

## Formato dei messaggi
- Oltre alla codifica stessa delle informazioni il progettista deve occuparsi di organizzazione di informazioni in messaggi che vengono scambiati tra Client e Server
- Spazio di progetto abbastanza ampio

### Parsing messaggi
- Il parsing dei messaggi di input rappresenta un aspetto critico dello sviluppo di applicazioni distribuite da molti punti di vista (correttezza, robustezza, sicurezza, performance)
- Parsing e' **estremamente *error prone***. Caldamente consigliabile utilizzare strumenti dedicati per la generazione del codice di parsing dei messaggi, a partire dai **parser combinator**
- Possibile adozione di strumenti di testing, e di soluzioni per la verifica di correttezza formale

## Terminated data
Una possibilita' e' quella di definire uno speciale terminatore che indica la fine di una informazione a lunghezza della variabile

![[terminatedData.png]]

Pro:
- Uso di testo UTF-8 terminato da "\n" modo semplice e human friendly di realizzare protocolli (anche multilinea, come HTTP 1.* ed email/RFC822)
- Implementazione relativamente semplice sender-side indipendentemente dal terminatore

Contro:
- Notevole overhead di gestione del buffer (scansione per trovare il terminatore, eventuale spostamento di memoria, ecc.)
- Terminatore non puo' essere usato come informazione valida (uso doppio terminatore o escaping)
- Attenzione alla definizione di una massima per i messaggi
- In C non e' disponibile una funzione come `readLine` di Java

**Esempio lettura di mesaggio null-terminated**
``` C
uint8_t buffer[BUFSIZE]; /* buffer di appoggio */
uint8_t value[BUFSIZE]; /* buffer in cui metterò l’informazione */
int bytes_received = 0;
uint8_t *term_ptr;
size_t left = sizeof(buffer);
do {
	/* controllo se ho ricevuto il terminatore ‘\0’ */
	if ((term_ptr = memchr(buffer, '\0', bytes_received)) != NULL)
		break;
	/* esco se leggo dato di dimensioni maggiori di BUFSIZE */
	if (left == 0) {
		fprintf(stderr, "Protocol error!\n");
		exit(EXIT_FAILURE);
	}
	cc = read(ns, buffer + bytes_received, left);
	if (cc < 0) { perror("read"); close(ns); exit(EXIT_FAILURE); }
	bytes_received += cc; left -= cc;
} while (1);

/* calcolo dimensione informazione ricevuta */
int inform_size = term_ptr - buffer;
/* sposto informazione da buffer a value */
memcpy(value, buffer, inform_size);
/* sposto dati letti ma non ancora processari all’inizio di buffer */
memcpy(buffer, term_ptr + 1, bytes_received - inform_size - 1);
```

## Terminated data con doppio terminatore
![[terminatedData2.png]]

Una seconda possibilita' e' quella di anteporre a ogni informazione a lunghezza variabile un campo che ne indica la dimensione

Pro:
- Implementazione relativamente semplice receiver-side
- Gestione dei buffer significativamente piu' semplice rispetto a terminated data

Contro:
- Leggero aumento di complessita' sender-side rispetto ad approcci terminated data
- Impossibile discriminare informazioni tra loro

**Esempio lettura length-prefixed data in C**
```C
/* leggo dimensione campo e la ricostruisco in field_length */
cc = read(ns, buffer, 2); /* uso buffer appoggio per leggere field_length */
if (cc < 0) {
	perror("read"); exit(EXIT_FAILURE);
} else if (cc != 2) {
	fputs("Wrong protocol: cannot read next field length!", stderr);
	exit(EXIT_FAILURE);
}
field_length = (buffer[1] << 0) | (buffer[0] << 8); /* network byte order */

/* leggo field_length bytes dalla socket e li metto nel buffer value_buf */
to_read = field_length; index = 0;
while (to_read > 0) {/* equivalente della funzione read_n dello stevens */
	cc = read(ns, value_buf + index, to_read);
	if (cc < 0) { perror("read"); exit(EXIT_FAILURE); }
	to_read -= cc; index += cc;
}
```

**Esempio scrittura length-prefixed data in Java**
``` Java
/* scrivo su Socket toServer la stringa anno in codifica UTF-8 */
byte[] anno_utf8 = anno.getBytes("UTF-8");
int anno_len = anno_utf8.length;
byte[] len = new byte[2];
len[0] = (byte)((anno_len & 0xFF00) >> 8);
len[1] = (byte)(anno_len & 0xFF);

toServer.write(len);
toServer.write(anno_utf8);
```

## Tag (o Type), Length and Value
Una seconda possibilita' e' quella di anteporre a ogni informazione a lunghezza variabile un campo che ne indica la dimensione

![[tagLengthValue.png]]

Pro:
- Sostanzialmente gli stessi di length-prefixed data
- Possibile discriminare informazioni tra loro

Contro:
- Sostanzialmente gli stessi di length-prefixed data
- Aumento di complessita' rispetto a length-prefixed data
- Attenzione a non sottodimensionare il compo type

## Formati standard
Infine, e' possibile ricorrere a formati standard:
- Testuali
	- (Simplified) Canonical S-expression
	- XML
	- JSON
- Binari
	- Google Protocol Buffer (Protobuf)
	- Mempack
	- CBOR