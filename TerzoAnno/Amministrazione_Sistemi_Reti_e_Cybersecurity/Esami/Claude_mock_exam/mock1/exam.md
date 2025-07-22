## Esame Mockup 1

# 15 maggio 2025

Durata esame: 2 ore e 30 minuti

| Sezione                             | Punti |
| ----------------------------------- | ----- |
| Monitoraggio sistema (§1)           | 16    |
| - Script Python (§1.1)              | 10/16 |
| - Service (§1.2)                    | 6/16  |
| Filtraggio dei pacchetti e NAT (§2) | 8     |
| Domande a risposta aperta (§3)      | 9     |

Per stampare

```shell
$ stampa <path/file/da/stampare>
```

> [!warning]
> 1. Scrivere **nome**, **cognome** e numero di matricola su ogni file che si stampa
> 2. Una volta mandati in stampa i file, avvisare il docente e **rimanere seduti al posto**

> [!tip]
> 1. Se si nota un errore sul file stampato, lo si può correggere a penna

## 1. Monitoraggio sistema

### 1.1. Script Python

Scrivi uno script Python che monitora periodicamente l'utilizzo della CPU del sistema e registra un allarme quando l'utilizzo supera una soglia specificata per un certo numero di controlli consecutivi. Nella tua home directory, crea una directory chiamata `cpu-monitor` e, al suo interno, un file chiamato `app.py`, utilizzando questo template:

```python
# nome e cognome:
# matricola:
#
# path: 

import argparse
import os
import sys
import time
import psutil

def main():
    pass


if __name__ == "__main__":
    main()
```

Lo script deve accettare esattamente quattro argomenti obbligatori da linea di comando, analizzati con il modulo `argparse`: `--threshold`, che specifica la soglia di utilizzo CPU in percentuale (numero intero tra 1 e 100); `--count`, che indica il numero di controlli consecutivi sopra soglia necessari per generare un allarme (intero positivo); `--interval`, che definisce l'intervallo in secondi tra i controlli (intero positivo); e `--logfile`, che specifica il file di log dove scrivere gli allarmi.

Dopo il parsing, valida gli input: verifica che `--threshold` sia un intero compreso tra 1 e 100, che `--count` e `--interval` siano interi positivi, e che il percorso del file di log sia valido (la directory padre deve esistere).

Il programma deve monitorare continuamente l'utilizzo della CPU (`psutil.cpu_percent`), mantenendo un contatore dei controlli consecutivi sopra soglia. Quando il contatore raggiunge il valore specificato da `--count`, scrivi un messaggio di allarme nel file di log con timestamp, percentuale CPU corrente, e numero di controlli consecutivi. Resetta il contatore quando l'utilizzo scende sotto la soglia.

Ad esempio, eseguendo

```shell
$ python ~/cpu-monitor/app.py \
    --threshold 80 \
    --count 3 \
    --interval 5 \
    --logfile ~/cpu-alerts.log
```

lo script deve generare un allarme quando la CPU supera l'80% per 3 controlli consecutivi, controllando ogni 5 secondi.

### 1.2. Service

Crea un'unità *service* denominata `cpu-monitor.service` nella tua istanza utente di `systemd`. L'unità deve avviare `~/cpu-monitor/app.py` con gli argomenti `--threshold 90`, `--count 5`, `--interval 10`, e `--logfile %h/cpu-monitor.log`, partire all'avvio del sistema e ripartire automaticamente in caso di fallimenti. Usa questo template:

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
| `eth0` | `172.16.0.0/24`   | `172.16.0.1`    | Pubblico |
| `eth1` | `192.168.1.0/24`  | `192.168.1.1`   | Privato  |

Gli host della rete `192.168.1.0/24` utilizzano questo firewall come gateway di default. L'host `192.168.1.100` esegue un server SSH sulla porta `2222`.

Applica le seguenti regole:

| Tabella      | Catena          | Regola                                                                                           |
| ------------ | --------------- | ------------------------------------------------------------------------------------------------ |
| `filter,nat` | `*`             | Elimina tutte le regole esistenti                                                                |
| `filter`     | `INPUT,FORWARD` | Scarta tutto a meno che non sia esplicitamente permesso                                          |
| `filter`     | `INPUT`         | Consenti pacchetti ICMP ricevuti su `eth0` e `eth1`                                              |
| `filter`     | `INPUT`         | Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1`                                             |
| `filter`     | `FORWARD`       | Consenti pacchetti HTTP (`tcp/80`) e HTTPS (`tcp/443`) ricevuti su `eth1`                        |
| `filter`     | `FORWARD`       | Consenti pacchetti con stato `ESTABLISHED,RELATED`                                               |
| `nat`        | `POSTROUTING`   | MASQUERADE pacchetti in uscita su `eth0` per consentire l'accesso a Internet agli host privati  |
| `nat`        | `PREROUTING`    | DNAT per pacchetti SSH (`tcp/22`) ricevuti su `eth0` verso `192.168.1.100:2222`                 |

Usa questo template:

```
# nome e cognome:
# matricola:
```

## 3. Domande a risposta aperta

1. Cos'è l'ingegneria sociale, perché è particolarmente difficile da contrastare, e qual è una forma comune di questo tipo di attacco?
2. Quali sono le regole fondamentali che governano il modello di permessi tradizionale UNIX?
3. Cos'è l'ARP spoofing, quali debolezze nel protocollo ARP sfrutta, e come si sviluppa un attacco MITM in pratica?

Usa questo template:

```
# nome e cognome:
# matricola:

1.

2.

3.
```