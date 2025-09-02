# nome e cognome:
# matricola:
#
# path: $HOME/disk-usage-monitor/app.py

import argparse
from datetime import datetime
import os
import shutil
import sys


def main():
    parser = argparse.ArgumentParser(description="disk usage monitor")
    parser.add_argument("--partition", type=str, required=True, help="percorso assoluto della partizione da monitorare")    
    parser.add_argument("--threshold", type=int, required=True, help="soglia in percentuale oltre la quale dev'essere segnalato l'utilizzo")
    args = parser.parse_args()

    if not os.path.isabs(args.partition):
        print(f"error: {args.partition} deve essere un path assoluto")
        sys.exit(1)

    if not os.path.exists(args.partition):
        print(f"error: {args.partition} deve essere un path esistente")
        sys.exit(1)

    if args.threshold < 0 or args.threshold > 100:
        print(f"error: {args.threshold} deve essere un intero compreso tra 0 e 100") 
        sys.exit(1)

    total, used, _ = shutil.disk_usage(args.partition)
    percent_used   = used / total * 100
    print(f"disk usage for {args.partition}: {percent_used}")

    if percent_used >= args.threshold:
        log_dir = os.path.expanduser("~/disk-usage/monitor")
        os.makedirs(log_dir, exist_ok=True)

        log_path = os.path.join(log_dir, "disk-usage-monitor.log")
        with open(log_path, "a") as log_file:
            now = datetime.now
            log_file.write(f"{now} {percent_used}")


if __name__ == "__main__":
    main()