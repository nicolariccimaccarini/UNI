# nome e cognome:
# matricola:
#
# path: $HOME/file-archiver/app.py

import argparse
import os
import sys
import shutil
import time


def walk(basepath, seconds, archive_path):
    for filename in os.listdir(basepath):
        path = os.path.join(basepath, filename)

        if os.path.isfile(path):
            file_time = time.time() - os.path.getmtime(path)
            if file_time >= seconds:
                shutil.move(path, archive_path)
                print(f"spostato {path} in {archive_path}")

        elif os.path.isdir(path):
            walk(path, seconds, archive_path)


def main():
    parser = argparse.ArgumentParser(description="file archiver")
    parser.add_argument("--path", type=str, required=True, help="percorso assoluto della directory da controllare")
    parser.add_argument("--seconds", type=int, required=True, help="eta' massima (sec) oltre la quale i file devono essere spostati")
    args = parser.parse_args()

    if not os.path.isabs(args.path):
        print(f"error: {args.path} deve essere un path assolto")
        sys.exit(1)

    if not os.path.exists(args.path):
        print(f"error: {args.path} deve essere un path esistente")
        sys.exit(1)

    if not os.path.isdir(args.path):
        print(f"error: {args.path} deve essere il path di una directory")
        sys.exit(1)

    if not args.seconds > 0:
        print(f"error: {args.seconds} deve essere un intero positivo")
        sys.exit(1)

    archive_path = os.path.expanduser("~/archive")
    os.makedirs(archive_path, exist_ok=True)
    walk(args.path, args.seconds, archive_path)

if __name__ == "__main__":
    main()