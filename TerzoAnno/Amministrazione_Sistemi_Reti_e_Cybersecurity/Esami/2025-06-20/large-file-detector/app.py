# nome e cognome:
# matricola:
#
# path: $HOME/large-fie-detector/app.py

import argparse
import os
import sys
import time


LOG_FILE = "large-file-detector.log"


def walk(basepat, size, log_path):
    for filename in os.listdir(basepat):

        path = os.path.join(basepat, filename)
        if os.path.isfile(path):
            file_size = os.path.getsize(path)
            if file_size >= size:
                print(f"found large file: {path} ({file_size} bytes)")
                with open(log_path, "a") as log_file:
                    log_file.write(f"{path}\n")
        elif os.path.isdir(path):
            walk(path, size, log_path)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--target", type=str, required=True, help="percorso assoluto della directory da controllare")
    parser.add_argument("--size", type=int, required=True, help="dimensione minima in byte dei file da segnalare")
    parser.add_argument("--interval", type=int, required=True, help="intervallo in secondi tra ogni controllo")
    parser.add_argument("--log", type=str, required=True, help="dove salvare il file di log")
    args = parser.parse_args()

    if not os.path.isabs(args.target):
        print(f"error: {args.target} deve essere un path assoluto")
        sys.exit(1)

    if not os.path.exists(args.target):
        print(f"error: {args.target} deve essere un path esistente")
        sys.exit(1)

    if not os.path.isdir(args.target):
        print(f"error: {args.target} deve essere una directory")
        sys.exit(1)

    if not args.size > 0 and args.interval > 0:
        print(f"error: {args.size} e {args.interval} devono essere due interi positivi")
        sys.exit(1)

    if not os.path.exists(args.log):
        print(f"error: {args.log} deve essere un path esistente")
        sys.exit(1)

    if not os.path.isdir(args.log):
        print(f"error: {args.log} deve essere una directory")
        sys.exit(1)

    log_path = os.path.join(args.log, LOG_FILE)
    while True:
        walk(args.target, args.size, log_path)
        time.sleep(args.interval)

if __name__ == "__main__":
    main()