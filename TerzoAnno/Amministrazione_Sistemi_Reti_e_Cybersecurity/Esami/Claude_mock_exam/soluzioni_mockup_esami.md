# Mockup Esami Aggiuntivi per Preparazione

## Esame Mockup 4

# 3 settembre 2025

Durata esame: 2 ore e 30 minuti

| Sezione                             | Punti |
| ----------------------------------- | ----- |
| Processo periodico (§1)             | 16    |
| - Script Python (§1.1)              | 8/16  |
| - Service (§1.2)                    | 4/16  |
| - Timer (§1.3)                      | 4/16  |
| Filtraggio dei pacchetti e NAT (§2) | 8     |
| Domande a risposta aperta (§3)      | 9     |

## 1. Processo periodico

### 1.1. Script Python

Scrivi uno script Python che monitora i processi in esecuzione sul sistema e termina quelli che superano una soglia di utilizzo CPU specificata. Nella tua home directory, crea una directory chiamata `process-killer` e, al suo interno, un file chiamato `app.py`, utilizzando questo template:

```python
# nome e cognome:
# matricola:
#
# path: 

import argparse
import os
import sys
import psutil
import signal

def main():
    pass

if __name__ == "__main__":
    main()
```

Lo script deve accettare esattamente tre argomenti obbligatori da linea di comando, analizzati con il modulo `argparse`: `--cpu-threshold`, che specifica la soglia di utilizzo CPU in percentuale (numero intero tra 1 e 100); `--exclude`, che indica il nome di un processo da non terminare mai (stringa); e `--log`, che specifica il percorso del file di log dove registrare le terminazioni.

Dopo il parsing, valida gli input: verifica che `--cpu-threshold` sia un intero compreso tra 1 e 100; controlla che la directory del file di log esista (`os.path.exists`, `os.path.dirname`).

Il programma deve scansionare tutti i processi in esecuzione (`psutil.process_iter`), identificare quelli che superano la soglia CPU e terminarli con `SIGTERM` (`process.terminate()`), eccetto quelli il cui nome corrisponde al parametro `--exclude`. Per ogni processo terminato, scrivi una riga nel file di log contenente PID, nome del processo, utilizzo CPU e timestamp.

Ad esempio, eseguendo

```shell
$ python ~/process-killer/app.py \
    --cpu-threshold 90 \
    --exclude python3 \
    --log ~/process-killer.log
```

lo script deve terminare tutti i processi che usano più del 90% di CPU, eccetto quelli chiamati "python3".

### 1.2. Service

Crea un'unità *service* denominata `process-killer.service` nella tua istanza utente di `systemd`. Configurala per avviare `~/process-killer/app.py` con gli argomenti `--cpu-threshold 95`, `--exclude systemd`, e `--log %h/process-killer.log`. Usa questo template:

```
# nome e cognome:
# matricola:
#
# path: 
```

### 1.3. Timer

Crea un'unità *timer* denominata `process-killer.timer` nella tua istanza utente di `systemd`. Configurala per attivare `process-killer.service` ogni 5 minuti. Usa questo template:

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
| `eth0` | `198.51.100.0/28` | `198.51.100.5`  | Pubblico |
| `eth1` | `10.0.1.0/24`     | `10.0.1.1`      | Privato  |

Gli host della rete `10.0.1.0/24` utilizzano questo firewall come gateway predefinito. L'host `10.0.1.50` esegue un server MySQL sulla porta `3306`.

Applica le seguenti regole:

| Tabella      | Catena          | Regola                                                                                           |
| ------------ | --------------- | ------------------------------------------------------------------------------------------------ |
| `filter,nat` | `*`             | Elimina tutte le regole esistenti                                                                |
| `filter`     | `INPUT,FORWARD` | Scarta tutto a meno che non sia esplicitamente permesso                                          |
| `filter`     | `INPUT`         | Consenti pacchetti ICMP ricevuti su `eth1`                                                       |
| `filter`     | `INPUT`         | Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1` solo da `10.0.1.100`                       |
| `filter`     | `FORWARD`       | Consenti pacchetti DNS (`udp/53`) ricevuti su `eth1`                                             |
| `filter`     | `FORWARD`       | Consenti pacchetti con stato `ESTABLISHED,RELATED`                                               |
| `nat`        | `POSTROUTING`   | SNAT per pacchetti in uscita su `eth0` usando l'IP del firewall                                  |
| `nat`        | `PREROUTING`    | DNAT per pacchetti MySQL (`tcp/3306`) ricevuti su `eth0` verso `10.0.1.50:3306`                 |

Usa questo template:

```
# nome e cognome:
# matricola:
```

## 3. Domande a risposta aperta

1. Cos'è un backup nel contesto della sicurezza informatica, e quali sono le raccomandazioni chiave per gestire efficacemente i backup?
2. Quali sono gli scopi dei bit set-UID, set-GID, e sticky, a quali file regolari o directory si applicano ciascuno, e come alterano i controlli di permesso?
3. Cos'è l'IP spoofing, e quali difese possono essere usate contro di esso?

Usa questo template:

```
# nome e cognome:
# matricola:

1.

2.

3.
```

---

## Esame Mockup 5

# 8 settembre 2025

Durata esame: 2 ore e 30 minuti

| Sezione                             | Punti |
| ----------------------------------- | ----- |
| Demone (§1)                         | 16    |
| - Script Python (§1.1)              | 10/16 |
| - Service (§1.2)                    | 6/16  |
| Filtraggio dei pacchetti e NAT (§2) | 8     |
| Domande a risposta aperta (§3)      | 9     |

## 1. Demone

### 1.1. Script Python

Scrivi uno script Python che monitora continuamente la temperatura del sistema e scrive allarmi in un file di log quando la temperatura supera soglie critiche. Nella tua home directory, crea una directory chiamata `thermal-monitor` e, al suo interno, un file chiamato `app.py`, utilizzando questo template:

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
import datetime

def main():
    pass

if __name__ == "__main__":
    main()
```

Lo script deve accettare esattamente tre argomenti obbligatori da linea di comando, analizzati con il modulo `argparse`: `--warning-temp`, che specifica la temperatura di avvertimento in gradi Celsius (numero intero); `--critical-temp`, che specifica la temperatura critica in gradi Celsius (numero intero); e `--interval`, che definisce l'intervallo di controllo in secondi (intero positivo).

Dopo il parsing, valida gli input: verifica che `--critical-temp` sia maggiore di `--warning-temp` e che `--interval` sia un intero positivo.

Il programma deve monitorare continuamente le temperature del sistema (`psutil.sensors_temperatures()`), controllando ogni sensore disponibile. Se una temperatura supera la soglia di warning, scrive nel file `~/thermal-monitor/thermal.log` un messaggio di avvertimento. Se supera la soglia critica, scrive un allarme critico. Il ciclo si ripete ogni `--interval` secondi.

Ad esempio, eseguendo

```shell
$ python ~/thermal-monitor/app.py \
    --warning-temp 70 \
    --critical-temp 85 \
    --interval 30
```

lo script deve monitorare le temperature ogni 30 secondi, generando warning a 70°C e allarmi critici a 85°C.

### 1.2. Service

Crea un'unità *service* denominata `thermal-monitor.service` nella tua istanza utente di `systemd`. L'unità deve avviare `~/thermal-monitor/app.py` con gli argomenti `--warning-temp 75`, `--critical-temp 90`, e `--interval 60`, partire all'avvio del sistema e ripartire automaticamente in caso di fallimenti. Usa questo template:

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
| `eth0` | `203.0.113.0/28`  | `203.0.113.15`  | Pubblico |
| `eth1` | `172.16.10.0/24`  | `172.16.10.1`   | Privato  |

Gli host della rete `172.16.10.0/24` utilizzano questo firewall come gateway predefinito. L'host `172.16.10.80` esegue un server SMTP (porta 25) e l'host `172.16.10.90` esegue un server POP3 (porta 110).

Applica le seguenti regole:

| Tabella      | Catena          | Regola                                                                                           |
| ------------ | --------------- | ------------------------------------------------------------------------------------------------ |
| `filter,nat` | `*`             | Elimina tutte le regole esistenti                                                                |
| `filter`     | `INPUT,FORWARD` | Scarta tutto a meno che non sia esplicitamente permesso                                          |
| `filter`     | `INPUT`         | Consenti pacchetti ICMP ricevuti su `eth0` e `eth1`                                              |
| `filter`     | `INPUT`         | Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1`                                             |
| `filter`     | `FORWARD`       | Consenti tutti i pacchetti ricevuti su `eth1` e diretti su `eth0`                                |
| `filter`     | `FORWARD`       | Consenti pacchetti con stato `ESTABLISHED,RELATED`                                               |
| `nat`        | `POSTROUTING`   | MASQUERADE pacchetti in uscita su `eth0`                                                         |
| `nat`        | `PREROUTING`    | DNAT per pacchetti SMTP (`tcp/25`) ricevuti su `eth0` verso `172.16.10.80:25`                   |
| `nat`        | `PREROUTING`    | DNAT per pacchetti POP3 (`tcp/110`) ricevuti su `eth0` verso `172.16.10.90:110`                 |

Usa questo template:

```
# nome e cognome:
# matricola:
```

## 3. Domande a risposta aperta

1. Cos'è un rootkit, come funziona tipicamente, e perché può essere particolarmente difficile da rilevare e rimuovere?
2. Chi può cambiare i bit di permesso di un file, quale comando può usare, e come viene invocato quel comando?
3. Come può un attaccante montare un attacco MITM con messaggi di redirect ICMP, e quali debolezze nel protocollo ICMP rendono questo possibile?

Usa questo template:

```
# nome e cognome:
# matricola:

1.

2.

3.
```

---

## Esame Mockup 6

# 12 settembre 2025

Durata esame: 2 ore e 30 minuti

| Sezione                             | Punti |
| ----------------------------------- | ----- |
| Processo periodico (§1)             | 16    |
| - Script Python (§1.1)              | 8/16  |
| - Service (§1.2)                    | 4/16  |
| - Timer (§1.3)                      | 4/16  |
| Filtraggio dei pacchetti e NAT (§2) | 8     |
| Domande a risposta aperta (§3)      | 9     |

## 1. Processo periodico

### 1.1. Script Python

Scrivi uno script Python che controlla la memoria disponibile del sistema e libera la cache quando la memoria libera scende sotto una soglia specificata. Nella tua home directory, crea una directory chiamata `memory-cleaner` e, al suo interno, un file chiamato `app.py`, utilizzando questo template:

```python
# nome e cognome:
# matricola:
#
# path: 

import argparse
import os
import sys
import psutil
import subprocess
import datetime

def main():
    pass

if __name__ == "__main__":
    main()
```

Lo script deve accettare esattamente due argomenti obbligatori da linea di comando, analizzati con il modulo `argparse`: `--threshold`, che specifica la soglia minima di memoria libera in MB (intero positivo); e `--logfile`, che indica il file di log dove registrare le operazioni di pulizia.

Dopo il parsing, valida gli input: verifica che `--threshold` sia un intero positivo e che la directory del file di log esista.

Il programma deve controllare la memoria disponibile del sistema (`psutil.virtual_memory()`). Se la memoria disponibile in MB è inferiore alla soglia specificata, deve eseguire il comando `sync && echo 3 > /proc/sys/vm/drop_caches` per liberare la cache (usando `subprocess.run` con `shell=True`). Ogni operazione di pulizia deve essere registrata nel file di log con timestamp e quantità di memoria liberata.

Ad esempio, eseguendo

```shell
$ python ~/memory-cleaner/app.py \
    --threshold 1024 \
    --logfile ~/memory-cleaner.log
```

lo script deve liberare la cache quando la memoria disponibile scende sotto 1024 MB.

### 1.2. Service

Crea un'unità *service* denominata `memory-cleaner.service` nella tua istanza utente di `systemd`. Configurala per avviare `~/memory-cleaner/app.py` con gli argomenti `--threshold 2048` e `--logfile %h/memory-cleaner.log`. Usa questo template:

```
# nome e cognome:
# matricola:
#
# path: 
```

### 1.3. Timer

Crea un'unità *timer* denominata `memory-cleaner.timer` nella tua istanza utente di `systemd`. Configurala per attivare `memory-cleaner.service` ogni ora dalle 9:00 alle 18:00 nei giorni lavorativi (Lunedì-Venerdì). Usa questo template:

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
| `eth0` | `192.0.2.0/24`    | `192.0.2.10`    | Pubblico |
| `eth1` | `10.20.30.0/24`   | `10.20.30.1`    | Privato  |

Gli host della rete `10.20.30.0/24` utilizzano questo firewall come gateway predefinito. L'host `10.20.30.40` esegue un server NTP (porta 123 UDP) e l'host `10.20.30.50` esegue un server Syslog (porta 514 UDP).

Applica le seguenti regole:

| Tabella      | Catena          | Regola                                                                                           |
| ------------ | --------------- | ------------------------------------------------------------------------------------------------ |
| `filter,nat` | `*`             | Elimina tutte le regole esistenti                                                                |
| `filter`     | `INPUT,FORWARD` | Scarta tutto a meno che non sia esplicitamente permesso                                          |
| `filter`     | `INPUT`         | Consenti pacchetti ICMP ricevuti su `eth1`                                                       |
| `filter`     | `INPUT`         | Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1` dalle ore 8:00 alle 20:00                  |
| `filter`     | `FORWARD`       | Consenti pacchetti NTP (`udp/123`) e Syslog (`udp/514`) ricevuti su `eth1`                      |
| `filter`     | `FORWARD`       | Consenti pacchetti con stato `ESTABLISHED,RELATED`                                               |
| `nat`        | `POSTROUTING`   | SNAT per pacchetti in uscita su `eth0` usando l'IP del firewall                                  |
| `nat`        | `PREROUTING`    | DNAT per pacchetti NTP (`udp/123`) ricevuti su `eth0` verso `10.20.30.40:123`                   |
| `nat`        | `PREROUTING`    | DNAT per pacchetti Syslog (`udp/514`) ricevuti su `eth0` verso `10.20.30.50:514`                |

Usa questo template:

```
# nome e cognome:
# matricola:
```

## 3. Domande a risposta aperta

1. Cosa rappresenta la triade CIA nella sicurezza informatica, e cosa significa ciascun principio?
2. Quali tipi di file supporta UNIX, e come i nove bit di permesso (`rwx` per utente, gruppo, e altri) governano le operazioni consentite su ciascun tipo?
3. Cos'è l'IPv4 source routing, e come può un attaccante sfruttarlo?

Usa questo template:

```
# nome e cognome:
# matricola:

1.

2.

3.
```

---

## Soluzioni Complete

### Esame Mockup 4 - Soluzioni

#### 1.1. Script Python - Process Killer

```python
# nome e cognome: Mario Rossi
# matricola: 123456
#
# path: ~/process-killer/app.py

import argparse
import os
import sys
import psutil
import signal
import datetime

def main():
    parser = argparse.ArgumentParser(description='Process Killer')
    parser.add_argument('--cpu-threshold', type=int, required=True, help='CPU usage threshold (1-100)')
    parser.add_argument('--exclude', type=str, required=True, help='Process name to exclude from termination')
    parser.add_argument('--log', type=str, required=True, help='Log file path')
    
    args = parser.parse_args()
    
    # Validazione input
    if not (1 <= args.cpu_threshold <= 100):
        print("Errore: cpu-threshold deve essere compreso tra 1 e 100", file=sys.stderr)
        sys.exit(1)
    
    log_dir = os.path.dirname(os.path.abspath(args.log))
    if not os.path.exists(log_dir):
        print(f"Errore: la directory {log_dir} non esiste", file=sys.stderr)
        sys.exit(1)
    
    print(f"Monitoraggio processi avviato. Soglia CPU: {args.cpu_threshold}%, Escluso: {args.exclude}")
    
    try:
        for proc in psutil.process_iter(['pid', 'name', 'cpu_percent']):
            try:
                # Ottieni informazioni sul processo
                proc_info = proc.info
                cpu_usage = proc.cpu_percent(interval=1)
                
                if cpu_usage >= args.cpu_threshold and proc_info['name'] != args.exclude:
                    # Termina il processo
                    proc.terminate()
                    
                    # Log dell'operazione
                    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                    log_message = f"[{timestamp}] Terminated PID {proc_info['pid']} ({proc_info['name']}) - CPU: {cpu_usage:.1f}%\n"
                    
                    with open(args.log, 'a') as f:
                        f.write(log_message)
                    
                    print(f"Terminato processo {proc_info['pid']} ({proc_info['name']}) - CPU: {cpu_usage:.1f}%")
                    
            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                # Ignora processi che non esistono più o non sono accessibili
                pass
                
    except KeyboardInterrupt:
        print("\nMonitoraggio interrotto")
    except Exception as e:
        print(f"Errore: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
```

#### 1.2. Service - Process Killer

```ini
# nome e cognome: Mario Rossi
# matricola: 123456
#
# path: ~/.config/systemd/user/process-killer.service

[Unit]
Description=Process Killer Service

[Service]
Type=oneshot
Environment=PYTHONUNBUFFERED=1
WorkingDirectory=%h/process-killer
ExecStart=/usr/bin/python3 app.py --cpu-threshold 95 --exclude systemd --log %h/process-killer.log
```

#### 1.3. Timer - Process Killer

```ini
# nome e cognome: Mario Rossi
# matricola: 123456
#
# path: ~/.config/systemd/user/process-killer.timer
#
# comando per abilitare il timer: systemctl --user enable process-killer.timer
# comando per avviare il timer: systemctl --user start process-killer.timer

[Unit]
Description=Process Killer Timer

[Timer]
Unit=process-killer.service
OnCalendar=*:0/5

[Install]
WantedBy=timers.target
```

#### 2. Firewall Configuration - Mockup 4

```bash
# nome e cognome: Mario Rossi
# matricola: 123456

# Flush di tutte le regole esistenti
iptables -F
iptables -t nat -F

# Impostazione policy di default
iptables -P INPUT DROP
iptables -P FORWARD DROP

# Consenti pacchetti ICMP ricevuti su eth1
iptables -A INPUT -i eth1 -p icmp -j ACCEPT

# Consenti pacchetti SSH ricevuti su eth1 solo da 10.0.1.100
iptables -A INPUT -i eth1 -s 10.0.1.100 -p tcp --dport 22 -j ACCEPT

# Consenti pacchetti DNS ricevuti su eth1
iptables -A FORWARD -i eth1 -p udp --dport 53 -j ACCEPT

# Consenti pacchetti con stato ESTABLISHED,RELATED
iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

# SNAT per pacchetti in uscita su eth0
iptables -t nat -A POSTROUTING -o eth0 -j SNAT --to-source 198.51.100.5

# DNAT per MySQL verso 10.0.1.50:3306
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 3306 -j DNAT --to-destination 10.0.1.50:3306
iptables -A FORWARD -i eth0 -o eth1 -p tcp -d 10.0.1.50 --dport 3306 -j ACCEPT
```

### Esame Mockup 5 - Soluzioni

#### 1.1. Script Python - Thermal Monitor

```python
# nome e cognome: Mario Rossi
# matricola: 123456
#
# path: ~/thermal-monitor/app.py

import argparse
import os
import sys
import time
import psutil
import datetime

def main():
    parser = argparse.ArgumentParser(description='Thermal Monitor')
    parser.add_argument('--warning-temp', type=int, required=True, help='Warning temperature in Celsius')
    parser.add_argument('--critical-temp', type=int, required=True, help='Critical temperature in Celsius')
    parser.add_argument('--interval', type=int, required=True, help='Check interval in seconds')
    
    args = parser.parse_args()
    
    # Validazione input
    if args.critical_temp <= args.warning_temp:
        print("Errore: critical-temp deve essere maggiore di warning-temp", file=sys.stderr)
        sys.exit(1)
    
    if args.interval <= 0:
        print("Errore: interval deve essere un intero positivo", file=sys.stderr)
        sys.exit(1)
    
    # Crea directory per il log
    log_dir = os.path.expanduser("~/thermal-monitor")
    os.makedirs(log_dir, exist_ok=True)
    log_path = os.path.join(log_dir, "thermal.log")
    
    print(f"Monitoraggio termico avviato. Warning: {args.warning_temp}°C, Critical: {args.critical_temp}°C")
    
    try:
        while True:
            try:
                temperatures = psutil.sensors_temperatures()
                
                for sensor_name, sensor_list in temperatures.items():
                    for sensor in sensor_list:
                        if sensor.current is not None:
                            temp = sensor.current
                            timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                            
                            if temp >= args.critical_temp:
                                message = f"[{timestamp}] CRITICAL: {sensor_name} - {temp:.1f}°C\n"
                                with open(log_path, 'a') as f:
                                    f.write(message)
                                print(f"CRITICAL: {sensor_name} - {temp:.1f}°C")
                                
                            elif temp >= args.warning_temp:
                                message = f"[{timestamp}] WARNING: {sensor_name} - {temp:.1f}°C\n"
                                with open(log_path, 'a') as f:
                                    f.write(message)
                                print(f"WARNING: {sensor_name} - {temp:.1f}°C")
                                
            except Exception as e:
                print(f"Errore nella lettura sensori: {e}")
            
            time.sleep(args.interval)
            
    except KeyboardInterrupt:
        print("\nMonitoraggio interrotto")
    except Exception as e:
        print(f"Errore: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
```

#### 1.2. Service - Thermal Monitor

```ini
# nome e cognome: Mario Rossi
# matricola: 123456
#
# path: ~/.config/systemd/user/thermal-monitor.service
# 
# comando per abilitare il servizio: systemctl --user enable thermal-monitor.service
# comando per avviare il servizio: systemctl --user start thermal-monitor.service

[Unit]
Description=Thermal Monitor Service
After=multi-user.target

[Service]
Type=simple
Environment=PYTHONUNBUFFERED=1
WorkingDirectory=%h/thermal-monitor
ExecStart=/usr/bin/python3 app.py --warning-temp 75 --critical-temp 90 --interval 60
Restart=always
RestartSec=10

[Install]
WantedBy=default.target
```

### Esame Mockup 6 - Soluzioni

#### 1.1. Script Python - Memory Cleaner

```python
# nome e cognome: Mario Rossi
# matricola: 123456
#
# path: ~/memory-cleaner/app.py

import argparse
import os
import sys
import psutil
import subprocess
import datetime

def main():
    parser = argparse.ArgumentParser(description='Memory Cleaner')
    parser.add_argument('--threshold', type=int, required=True, help='Minimum free memory in MB')
    parser.add_argument('--logfile', type=str, required=True, help='Log file path')
    
    args = parser.parse_args()
    
    # Validazione input
    if args.threshold <= 0:
        print("Errore: threshold deve essere un intero positivo", file=sys.stderr)
        sys.exit(1)
    
    log_dir = os.path.dirname(os.path.abspath(args.logfile))
    if not os.path.exists(log_dir):
        print(f"Errore: la directory {log_dir} non esiste", file=sys.stderr)
        sys.exit(1)
    
    try:
        # Controlla memoria disponibile
        memory = psutil.virtual_memory()
        available_mb = memory.available / (1024 * 1024)
        
        print(f"Memoria disponibile: {available_mb:.1f} MB")
        
        if available_mb < args.threshold:
            print(f"Memoria sotto soglia ({args.threshold} MB), liberando cache...")
            
            # Libera cache
            try:
                subprocess.run("sync && echo 3 > /proc/sys/vm/drop_caches", 
                             shell=True, check=True)
                
                # Controlla memoria dopo pulizia
                memory_after = psutil.virtual_memory()
                available_after_mb = memory_after.available / (1024 * 1024)
                freed_mb = available_after_mb - available_mb
                
                # Log dell'operazione
                timestamp = datetime.datetime.now().strftime("%Y-%