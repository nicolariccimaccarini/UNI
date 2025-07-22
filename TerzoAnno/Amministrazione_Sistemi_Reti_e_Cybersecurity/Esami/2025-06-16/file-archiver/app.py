# nome e cognome: Nicola Ricci Maccarini
# matricola: 185792
#
# path: $HOME/file-archiver/app.py

import argparse
import os
import sys
import time
import shutil


def walk(basepath, seconds, archive_path):
    for filename in os.listdir(basepath):
        path = os.path.join(basepath, filename)

        if os.path.isdir(path):
            walk(path, seconds, archive_path)
        elif os.path.isfile(path):
            file_age = time.time() - os.path.getmtime(path)
            if file_age >= seconds:
                shutil.move(path, "~/archive")
                print(f"moved {path} to {archive_path}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--path", type=str, required=True, help="path assoluto della directory da controllare")
    parser.add_argument("--seconds", type=int, required=True, help="eta massima in secondi oltre i quali i file devono essere spostati")
    args = parser.parse_args()

    if not os.path.isabs(args.path):
        print(f"error: {args.path} must be absolute")
        sys.exit(1)

    if not os.path.exists(args.path):
        print(f"error: {args.path} must exist")
        sys.exit(1)

    if not os.path.isdir(args.path):
        print(f"{args.path} must be a directory")
        sys.exit(1)

    if not args.seconds > 0:
        print(f"{args.seconds} must be a positive integer")
        sys.exit(1)

    archive_path = os.path.expanduser("~/archive")
    os.makedirs(archive_path, exist_ok=True)
    walk(args.path, args.seconds, archive_path)


if __name__ == "__main__":
    main()