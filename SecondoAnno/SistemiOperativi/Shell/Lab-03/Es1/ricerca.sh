#!/bin/sh
# uso: ricrca.sh num sorg dest ext1 ext2 ... extN

num=$1
shift
sorg=$1
shift
dest=$1
shift

cd "$sorg"

for ext in $*
do 
    for file in "`ls *$ext 2>/dev/null`"
    do 
        if test -f "$file"
        then 
            if test -r "$file" -a `wc -l < "$file"` -gt $num 
            then 
                echo "$file" >> /tmp/.counter.tmp
                cp "$file" "$dest/$file"
            fi 
        fi 
    done 
done 

# RICORSIONE
