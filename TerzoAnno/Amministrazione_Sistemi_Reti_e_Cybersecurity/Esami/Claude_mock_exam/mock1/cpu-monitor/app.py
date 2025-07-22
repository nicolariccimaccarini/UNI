# nome e cognome: Nicola Ricci Maccarini
# matricola: 185792
#
# path: $HOME/cpu-monitor/app.py

import argparse
import os
import sys
import time
import psutil

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--threshold", type=int, required=True, help="soglia utilizzo CPU")
    parser.add_argument("--count", type=int, required=True, help="numero di controlli consecutivi")
    parser.add_argument("--interval", type=int, required=True, help="intervallo in secondi tra un controllo e l'altro")
    parser.add_argument("--logfile", type=str, required=True, help="file di log dove scrivere gli allarmi")
    args = parser.parse_args()

    if not args.threshold >= 1 and args.threshold <= 100:
        print(f"error: {args.threshold} deve essere un numero compreso tra 1 e 100")
        sys.exit(1)

    if not args.count >= 0:
        print(f"error: {args.count} deve essere un intero positivo")
        sys.exit(1)

    if not args.interval >= 0:
        print(f"error: {args.interval} deve essere un intero positivo")
        sys.exit(1)

    log_dir = os.path.dirname(args.logfile)
    if not os.path.exists(log_dir):
        print(f"error: {log_dir} deve essere un percorso esistente")
        sys.exit(1)

    consecutive = 0

    while True:
        cpu = psutil.cpu_percent()
        if cpu > args.threshol:
            consecutive += 1
            if consecutive >= args.count:
                with open(args.logfile, "a") as f:
                    timestamp = time.time()
                    f.write(f"[{timestamp}] ALERT: CPU {cpu}% for {consecutive} samples\n")
                consecutive = 0
        else:
            consecutive = 0
        time.sleep(args.interval)


if __name__ == "__main__":
    main()