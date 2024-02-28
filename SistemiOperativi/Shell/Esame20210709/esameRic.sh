#!/bin/sh
# esameRic <nome> <anno>

nome="$1"
shift 
anno="$1"
shift

cd "$anno"

> /tmp/livello.tmp

for i in `ls *.txt 2>/dev/null`
do 
    if test -r "$i" 
    then
        # salvo nel file temporaneo le informazioni del fiume in base al corso d'acqua e al giorno
        echo "`grep $nome $i | cut -f 1,3,5 -d ','`" >> $FIUMI
        echo "`grep $nome $i | cut -f 1 -d ','`" > /tmp/livello.tmp

        # if livello.tmp < MIN_LIVELLO --> MIN_LIVELLO=livello.tmp
        if test `cat /tmp/livello.tmp` -lt `cat $MIN_LIVELLO`
        then 
            echo /tmp/livello.tmp > $MIN_LIVELLO
            echo "$i" > $MIN_GIORNO
        fi 
    fi 
done 

# lancio la ricorsione
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        sh esameRic.sh "$nome" "$d"
    fi 
done 