import hashlib
from collections import defaultdict
import os


extensions = ['jpg', 'jpeg']


def is_image(file_path, extensions):
    for extension in extensions:
        if file_path.lower().endswith(extension.lower()):
            return True

    return False


def add_path(file_path, d):
    try:
        with open(file_path, 'rb') as f:
            file_hash = hashlib.md5()
            for chunk in iter(lambda: f.read(4096), b''):
                file_hash.update(chunk)
            
            digest = file_hash.hexdigest()
            
            d[digest].append(file_path)
            
        return d
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return d


def walk_images(dirname, d):
    for root, _, files in os.walk(dirname):
        for filename in files:
            file_path = os.path.join(root, filename)
            if is_image(file_path, extensions):
                d = add_path(file_path, d)
    return d
    

def main():
    d = defaultdict(list)
    walk_images(".", d)
    for digest, paths in d.items():
        if len(paths) > 1:
            for path in paths:
                print(path)


if __name__ == '__main__':
    main()