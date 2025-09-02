# first and last name: 
# serial number:
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
    parser = argparse.ArgumentParser()
    parser.add_argument("--path", type="str", required=True, help="absolute path of the directory to clean")
    parser.add_argument("--extension", type="str", required=True, help="file extension to remove (must begin with a dot '.')")
    args = parser.parse_args()

    if not os.path.isabs(args.path):
        print(f"error: the path must be absolut")
        sys.exit(1)

    if not os.path.exists(args.path):
        print(f"error: {args.path} must be an existent path")
        sys.exit(1)

    if not os.path.isdir(args.path):
        print(f"error: {args.path} must be a dir's path")
        sys.exit(1)

    if not args.extension.startswith("."):
        print(f"error: {args.extension} must start with a dot ('.')")
        sys.exit(1)

    walk(args.path, args.extension)


if __name__ == "__main__":
    main()