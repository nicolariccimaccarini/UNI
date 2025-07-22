## Esame Mockup 2

# 22 maggio 2025

Durata esame: 2 ore e 30 minuti

| Sezione                             | Punti |
| ----------------------------------- | ----- |
| Backup automatico (§1)              | 16    |
| - Script Python (§1.1)              | 8/16  |
| - Service (§1.2)                    | 4/16  |
| - Timer (§1.3)                      | 4/16  |
| Filtraggio dei pacchetti e NAT (§2) | 8     |
| Domande a risposta aperta (§3)      | 9     |

## 1. Backup automatico

### 1.1. Script Python

Scrivi uno script Python che crea automaticamente un backup compresso di una directory specificata, salvandolo in una directory di destinazione con un nome che include la data e l'ora correnti. Nella tua home directory, crea una directory chiamata `backup-tool` e, al suo interno, un file chiamato `app.py`, utilizzando questo template:

```python
# nome e cognome:
# matricola:
#
# path: 

import argparse
import os
import sys
import datetime
import shutil

def main():
    pass


if __name__ == "__main__":
    main()
```

Lo script deve accettare esattamente tre argomenti obbligatori da linea di comando, analizzati con il modulo `argparse`: `--source`, che specifica il percorso assoluto della directory da sottoporre a backup; `--destination`, che indica il percorso assoluto della directory dove salvare il backup; e `--format`, che specifica il formato di compressione (`zip`, `tar`, o `gztar`).

Dopo il parsing, valida gli input: verifica che entrambi i percorsi siano assoluti (`os.path.isabs`), che esistano (`os.path.exists`), e che siano directory (`os.path.isdir`); controlla inoltre che il formato sia uno dei tre supportati.

Il programma deve creare un archivio compresso della directory sorgente nella directory di destinazione. Il nome del file di backup deve seguire il formato `backup_YYYY-MM-DD_HH-MM-SS.{estensione}`, dove l'estensione dipende dal formato scelto. Utilizza `shutil.make_archive` per creare l'archivio e stampa il percorso completo del file di backup creato.

Ad esempio, eseguendo

```shell
$ python ~/backup-tool/app.py \
    --source ~/documents \
    --destination ~/backups \
    --format zip
```

lo script deve creare un file come `~/backups/backup_2025-05-22_14-30-15.zip` contenente tutti i file di `~/documents`.

### 1.2. Service

Crea un'unità *service* denominata `backup-tool.service` nella tua istanza utente di `systemd`. Configurala per avviare `~/backup-tool/app.py` con gli argomenti `--source %h/projects`, `--destination %h/backups`, e `--format gztar`. Usa questo template:

```
# nome e cognome:
# matricola:
#
# path: 
```

### 1.3. Timer

Crea un'unità *timer* denominata `backup-tool.timer` nella tua istanza utente di `systemd`. Configurala per attivare `backup-tool.service` ogni giorno alle 02:00. Usa questo template:

```
# nome e cognome:
# matricola:
#
# path: 
#
# comando per abilitare il timer:
# comando per avviare il timer:
```

## 2. Filtraggio dei pacchetti e NAT

Configura un firewall Linux usando `iptables`. Il firewall dispone di due interfacce:

| NIC    | Indirizzo di rete | IP del firewall | Ambito   |
| ------ | ----------------- | --------------- | -------- |
| `eth0` | `203.0.113.0/28`  | `203.0.113.5`   | Pubblico |
| `eth1` | `10.0.0.0/24`     | `10.0.0.1`      | Privato  |

Gli host della rete `10.0.0.0/24` utilizzano questo firewall come gateway predefinito. L'host `10.0.0.50` esegue un server web (HTTP e HTTPS) e l'host `10.0.0.60` esegue un server di posta (SMTP).

Applica le seguenti regole:

| Tabella      | Catena          | Regola                                                                                           |
| ------------ | --------------- | ------------------------------------------------------------------------------------------------ |
| `filter,nat` | `*`             | Elimina tutte le regole esistenti                                                                |
| `filter`     | `INPUT,FORWARD` | Scarta tutto a meno che non sia esplicitamente permesso                                          |
| `filter`     | `INPUT`         | Consenti pacchetti ICMP ricevuti su `eth1`                                                       |
| `filter`     | `INPUT`         | Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1`                                             |
| `filter`     | `FORWARD`       | Consenti tutti i pacchetti ricevuti su `eth1` e diretti su `eth0`                                |
| `filter`     | `FORWARD`       | Consenti pacchetti con stato `ESTABLISHED,RELATED`                                               |
| `nat`        | `POSTROUTING`   | SNAT per pacchetti in uscita su `eth0` utilizzando l'IP del firewall                            |
| `nat`        | `PREROUTING`    | DNAT per pacchetti HTTP (`tcp/80`) ricevuti su `eth0` verso `10.0.0.50:80`                      |
| `nat`        | `PREROUTING`    | DNAT per pacchetti HTTPS (`tcp/443`) ricevuti su `eth0` verso `10.0.0.50:443`                   |
| `nat`        | `PREROUTING`    | DNAT per pacchetti SMTP (`tcp/25`) ricevuti su `eth0` verso `10.0.0.60:25`                      |

Usa questo template:

```
# nome e cognome:
# matricola:
```

## 3. Domande a risposta aperta

1. Cos'è una vulnerabilità software, qual è un esempio specifico di tale vulnerabilità, e come le pratiche di revisione del codice open-source possono aiutare a ridurre queste vulnerabilità?
2. Cos'è un backup nel contesto della sicurezza informatica, e quali sono le raccomandazioni chiave per gestire efficacemente i backup?
3. Quali sono gli scopi dei bit set-UID, set-GID, e sticky, a quali file regolari o directory si applicano ciascuno, e come alterano i controlli di permesso?

Usa questo template:

```
# nome e cognome:
# matricola:

1.

2.

3.
```