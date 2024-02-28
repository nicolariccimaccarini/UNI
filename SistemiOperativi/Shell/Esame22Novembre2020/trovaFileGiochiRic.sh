#!/bin/sh
# trovaFileGiochiRic.sh <dir>

dir="$1"
shift

cd "$dir"

for i in `ls *.txt 2>/dev/null`
do 
    if test -f "$i" -a -r "$i"
    then 
        # file che abbiamo come prima riga la stringa "giochi"
        if test `head -1 "$i"`="giochi"
        then 
            # memorizzo il path di $i
            echo "`pwd`/$i" >> "$TROVATO"
            nl=`wc -l < "$i"`
            # controllo del massimo
            if test $nl -gt `cat "$MAX_COUNTER"`
            then 
                echo $nl > "$MAX_COUNTER"
                echo "`pwd`/$i" > "$MAX_FILE"
            fi 
        fi 
    fi 
done 

# esploro ricorsivamente il sottoalbero definito da dir
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        sh trovaFileGiochiRic.sh "$d"
    fi 
done 