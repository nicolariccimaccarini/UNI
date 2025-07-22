## Esame Mockup 3

# 28 maggio 2025

Durata esame: 2 ore e 30 minuti

| Sezione                             | Punti |
| ----------------------------------- | ----- |
| Analizzatore di log (§1)            | 16    |
| - Script Python (§1.1)              | 10/16 |
| - Service (§1.2)                    | 6/16  |
| Filtraggio dei pacchetti e NAT (§2) | 8     |
| Domande a risposta aperta (§3)      | 9     |

## 1. Analizzatore di log

### 1.1. Script Python

Scrivi uno script Python che analizza periodicamente un file di log, contando le occorrenze di una stringa specifica e scrivendo statistiche in un file di output. Nella tua home directory, crea una directory chiamata `log-analyzer` e, al suo interno, un file chiamato `app.py`, utilizzando questo template:

```python
# nome e cognome:
# matricola:
#
# path: 

import argparse
import os
import sys
import time
import datetime

def main():
    pass


if __name__ == "__main__":
    main()
```

Lo script deve accettare esattamente quattro argomenti obbligatori da linea di comando, analizzati con il modulo `argparse`: `--logfile`, che specifica il percorso del file di log da analizzare; `--pattern`, che indica la stringa da cercare; `--output`, che specifica il file dove scrivere le statistiche; e `--interval`, che definisce l'intervallo in secondi tra le analisi (intero positivo).

Dopo il parsing, valida gli input: verifica che il file di log esista (`os.path.exists`) e sia un file (`os.path.isfile`); controlla che la directory del file di output esista; verifica che `--interval` sia un intero positivo.

Il programma deve analizzare periodicamente il file di log, contando tutte le occorrenze della stringa specificata. Ad ogni analisi, scrivi nel file di output un record contenente: timestamp corrente, numero totale di occorrenze trovate, e numero di nuove occorrenze dall'ultima analisi. Mantieni la posizione dell'ultima lettura per identificare le nuove occorrenze.

Ad esempio, eseguendo

```shell
$ python ~/log-analyzer/app.py \
    --logfile /var/log/syslog \
    --pattern "ERROR" \
    --output ~/log-stats.txt \
    --interval 60
```

lo script deve analizzare `/var/log/syslog` ogni 60 secondi, contando le occorrenze di "ERROR" e scrivendo le statistiche in `~/log-stats.txt`.

### 1.2. Service

Crea un'unità *service* denominata `log-analyzer.service` nella tua istanza utente di `systemd`. L'unità deve avviare `~/log-analyzer/app.py` con gli argomenti `--logfile %h/application.log`, `--pattern "WARNING"`, `--output %h/warnings-stats.txt`, e `--interval 300`, partire all'avvio del sistema e ripartire automaticamente in caso di fallimenti. Usa questo template:

```
# nome e cognome:
# matricola:
#
# path: 
# 
# comando per abilitare il servizio:
# comando per avviare il servizio:
```

## 2. Filtraggio dei pacchetti e NAT

Configura un firewall Linux utilizzando `iptables`. Il firewall dispone di due interfacce:

| NIC    | Indirizzo di rete | IP del firewall | Ambito   |
| ------ | ----------------- | --------------- | -------- |
| `eth0` | `198.51.100.0/28` | `198.51.100.10` | Pubblico |
| `eth1` | `172.20.0.0/24`   | `172.20.0.1`    | Privato  |

Gli host della rete `172.20.0.0/24` utilizzano questo firewall come gateway predefinito. L'host `172.20.0.25` esegue un server DNS.

Applica le seguenti regole:

| Tabella      | Catena          | Regola                                                                                           |
| ------------ | --------------- | ------------------------------------------------------------------------------------------------ |
| `filter,nat` | `*`             | Elimina tutte le regole esistenti                                                                |
| `filter`     | `INPUT,FORWARD` | Scarta tutto a meno che non sia esplicitamente permesso                                          |
| `filter`     | `INPUT`         | Consenti pacchetti ICMP ricevuti su `eth1`                                                       |
| `filter`     | `INPUT`         | Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1`                                             |
| `filter`     | `FORWARD`       | Consenti pacchetti DNS (`udp/53` e `tcp/53`) ricevuti su `eth1`                                  |
| `filter`     | `FORWARD`       | Consenti pacchetti con stato `ESTABLISHED,RELATED`                                               |
| `nat`        | `POSTROUTING`   | MASQUERADE pacchetti in uscita su `eth0` per consentire l'accesso a Internet agli host privati  |
| `nat`        | `PREROUTING`    | DNAT per pacchetti DNS (`udp/53`) ricevuti su `eth0` verso `172.20.0.25:53`                     |
| `nat`        | `PREROUTING`    | DNAT per pacchetti DNS (`tcp/53`) ricevuti su `eth0` verso `172.20.0.25:53`                     |

Usa questo template:

```
# nome e cognome:
# matricola:
```

## 3. Domande a risposta aperta

1. Cosa sono i virus informatici e i worm, e quali sono le differenze chiave tra questi due tipi di malware?
2. Cos'è la crittografia a chiave pubblica, come funziona, e quali sono i suoi principali vantaggi e svantaggi?
3. Cos'è l'IP forwarding, e perché è solitamente non sicuro lasciarlo abilitato su host che non sono destinati ad agire come router?

Usa questo template:

```
# nome e cognome:
# matricola:

1.

2.

3.
```