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
    parser = argparse.ArgumentParser()
    parser.add_argument("--logfile", type=str, required=True, help="percorso del file log da analizzare")
    parser.add_argument("--pattern", type=str, required=True, help="indica la stringa da cercare")
    parser.add_argument("--output", type=str, required=True, help="specifica il file dove scrivere le statistiche")
    parser.add_argument("--interval", type=int, required=True, help="intervallo in secondi tra le analisi")
    args = parser.parse_args()

    if not os.path.exists(args.logfile):
        print(f"error: {args.logfile} deve essere un path esistente")
        sys.exit(1)

    if not os.path.isfile(args.logfile):
        print(f"error: {args.logfile} deve essere un file")
        sys.exit(1)

    if not os.path.exists(args.output):
        print(f"error: {args.output} deve essere un path esistente")
        sys.exit(1)

    if not os.path.isdir(args.output):
        print(f"error: {args.output} deve essere una directory")
        sys.exit(1)

    if not args.interval > 0:
        print(f"error: {args.interval} deve essere >= 0")
        sys.exit(1)

    last_pos = 0
    total_count = 0

    while True:
        with open(args.logfile, "r") as f:
            f.seek(last_pos)
            new_lines = f.readlines()
            last_pos = f.tell()

        new_count = sum(1 for line in new_lines if args.pattern in line)
        total_count += new_count

        with open(args.output, "a") as out:
            timestamp = datetime.datetime.now().isoformat()
            out.write(f"{timestamp} - Totali: {total_count}, Nuove: {new_count}\n")

        time.sleep(args.interval)


if __name__ == "__main__":
    main()