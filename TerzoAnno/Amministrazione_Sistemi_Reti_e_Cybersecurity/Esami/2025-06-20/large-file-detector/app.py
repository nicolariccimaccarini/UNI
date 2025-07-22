# nome e cognome: Nicola Ricci Maccarini    
# matricola: 185792
#
# path: $HOME/large-file-detector/app.py

import argparse
import os
import sys
import time


FILE_NAME = "large-file-detector.log"


def walk(basepath, minimum_size, log_path):
    for filename in os.listdir(basepath):
        path = os.path.join(basepath, filename)

        if os.path.isdir(path):
            walk(path, minimum_size, log_path)
        elif os.path.isfile(path):
            file_dim = os.path.getsize(path)
            if file_dim >= minimum_size:
                print(f"found large file: {path} ({file_dim} bytes)")
                with open(log_path, "a") as log_file:
                    log_file.write(f"{path}\n")



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--target", type=str, required=True, help="percorso assoluto della directory da controllare")
    parser.add_argument("--size", type=int, required=True, help="dimensione minima in byte del file da segnalare")
    parser.add_argument("--interval", type=int, required=True, help="intervallo in secondi tra ogni controllo")
    parser.add_argument("--log", type=str, required=True, help="percorso dove verra' salvato il file di log")
    args = parser.parse_args()

    if not os.path.isabs(args.target):
        print(f"error: {args.target} deve essere assoluto")
        sys.exit(1)

    if not os.path.exists(args.target):
        print(f"error: {args.target} deve essere un percorso esistente")
        sys.exit(1)

    if not os.path.isdir(args.target):
        print(f"error: {args.target} deve essere una directory esistente")
        sys.exit(1)

    if not args.size > 0:
        print(f"error: size ({args.size}) deve essere un intero positivo")
        sys.exit(1)
    
    if not args.interval > 0:
        print(f"error: size ({args.interval}) deve essere un intero positivo")
        sys.exit(1)

    if not os.path.exists(args.log):
        print(f"error: {args.log} deve essere un percorso esistente")
        sys.exit(1)

    if not os.path.isdir(args.log):
        print(f"error: {args.log} deve essere una directory esistente")
        sys.exit(1)

    log_path = os.path.join(args.log, FILE_NAME)

    while True:
        walk(args.target, args.size, log_path)
        try:
           time.sleep(args.interval)
        except KeyboardInterrupt:
            break


if __name__ == "__main__":
    main()