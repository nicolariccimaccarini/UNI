# first and last name: Nicola Ricci Maccarini
# serial number: 185792
#
# path: $HOME/file-cleaner/app.py


import argparse
import os
import sys


def walk(basepath, extension):
    for filename in os.listdir(basepath):
        path = os.path.join(basepath, filename)

        if os.path.isfile(path) and path.endswith(extension):
            print(f"removing {path}")
            os.remove(path)
        elif os.path.isdir(path):
            walk(path, extension)


def main():
    parser = argparse.ArgumentParser(description="file cleaner")
    parser.add_argument("--path", type=str, required=True, help="absolute path of the directory to clean")
    parser.add_argument("--extension", type=str, required=True, help="file extensio to remuve, must begin with a dot")
    args = parser.parse_args()

    if not os.path.isabs(args.path):
        print(f"error: {args.path} is not an absolute path")
        sys.exit(1)

    if not os.path.exists(args.path):
        print(f"error: {args.path} does not exist")
        sys.exit(1)

    if not os.path.isdir(args.path):
        print(f"error: {args.path} is not a directory")
        sys.exit(1)

    if not args.extension.startwith("."):
        print(f"error: {args.extension} does not start with a dot (.)")
        sys.exit(1)

    walk(args.path, args.extension)


if __name__ == "__main__":
    main()