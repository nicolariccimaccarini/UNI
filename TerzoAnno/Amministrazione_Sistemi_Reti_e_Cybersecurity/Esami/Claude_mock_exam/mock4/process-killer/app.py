# nome e cognome:
# matricola:
#
# path: $HOME/process-killer/app.py

import argparse
import os
import sys
import psutil
import signal
import datetime

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--cpu_threshold", type=int, required=True, help="soglia di utilizzo CPU")
    parser.add_argument("--exclude", type=str, required=True, help="nome del processo da non terminare mai")
    parser.add_argument("--log", type=str, required=True, help="percorso del file log dove registrare le terminazioni")
    args = parser.parse_args()

    if not args.cpu_threshold >= 1 and args.cpu_threshold <= 100:
        print(f"error: {args.cpu_threshold} deve essere un intero compreso tra 1 e 100")
        sys.exit(1)

    log_dir = os.path.dirname(os.path.abspath(args.log))
    if not os.path.exists(log_dir):
        print(f"error: {log_dir} deve essere un path esistente")
        sys.exit(1)

    print(f"monitoraggio processi avviato. Soglia CPU: {args.cpu_threshold}%, Escluso: {args.exclude}")

    try:
        for proc in psutil.process_iter(['pid', 'name', 'cpu_percent']):
            try:
                proc_info = proc.info
                cpu_usage = proc.cpu_percent(interval=1)

                if cpu_usage > args.cpu_threshold and proc_info['name'] != args.exclude:
                    proc.terminate

                    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                    log_message = f"[{timestamp}] Terminated PID {proc_info['pid']} ({proc_info['name']}) - CPU: {cpu_usage:.1f}%\n"

                    with open(args.log, 'a') as f:
                        f.write(log_message)

                    print(f"terminato il processo {proc_info['pid']} ({proc_info['name']}) - CPU: {cpu_usage:.1f}%")

            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):# Ignora processi che non esistono più o non sono accessibili
                pass

    except KeyboardInterrupt:
        print("\nMonitoraggio interrotto")
    except Exception as e:
        print(f"Errore: {e}", file=sys.stderr)
        sys.exit(1) 
    

if __name__ == "__main__":
    main()