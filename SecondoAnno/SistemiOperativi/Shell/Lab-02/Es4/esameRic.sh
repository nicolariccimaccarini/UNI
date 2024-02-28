#!/bin/sh
# esameRic $estensione $dir1 $dir2

est="$1"
dir1="$2"
dir2="$3"

cd "$dir1"

# cerca file
for i in *$est
do 
    if test -f "$i" -a ! "$dir1/$i"
    then 
        cp "$i" "$dir2/$i"
        echo "Copia di $i"
    fi 
done 

# ricerca ricorsiva
for i in *
do 
    # controllando il permesso di esecuzione evito loop
    if test -d "$i" -a -x "$i"
    then 
        esameRic.sh "$est" "$dir1/$i" "$dir2"
    fi 
done 