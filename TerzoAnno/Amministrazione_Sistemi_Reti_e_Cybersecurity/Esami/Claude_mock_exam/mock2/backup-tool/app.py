# nome e cognome: Nicola Ricci Maccarini 
# matricola: 185792
#
# path: $HOME/backup-tool/app.py

import argparse
import os
import sys
import datetime
import shutil


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=str, required=True, help="percorso assoluto della directory da sottoporre a backup")
    parser.add_argument("--destination", type=str, required=True, help="percorso assoluto della directory dove salvare il backup")
    parser.add_argument("--format", type=str, required=True, help="formato di compressione (zip, tar, gztar)")
    args = parser.parse_args()

    if not os.path.isabs(args.source):
        print(f"error: {args.source} deve essere un path assoluto")
        sys.exit(1)

    if not os.path.exists(args.source):
        print(f"error: {args.source} deve esistere")
        sys.exit(1)

    if not os.path.isdir(args.source):
        print(f"error: {args.source} deve essere una directory")
        sys.exit(1)
    
    if not os.path.isabs(args.destination):
        print(f"error: {args.destination} deve essere un path assoluto")
        sys.exit(1)

    if not os.path.exists(args.destination):
        print(f"error: {args.destination} deve esistere")
        sys.exit(1)

    if not os.path.isdir(args.destination):
        print(f"error: {args.destination} deve essere una directory")
        sys.exit(1)

    if not args.format == "zip" or args.format == "tar" or args.format == "gztar":
        print(f"error: {args.format} deve essere uno dei tre: zip, tar o gztar")
        sys.exit(1)

    timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    base_name = os.path.join(args.destination, f"backup_{timestamp}")
    archive_path = shutil.make_archive(base_name, args.format, args.source)
    print(f"Backup creato: {archive_path}")


if __name__ == "__main__":
    main()