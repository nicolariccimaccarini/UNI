#!bin/sh
# identificaRic.sh <dir> <messaggio>

dir="$1"
shift
messaggio="$1"
shift

cd "$dir"

for i in *
do 
    # controllo che il file sia leggibile
    if test -f "$i" -a -r "$i" 
    then 
        # conto le righe in cui appare la stringa <messaggio>
        counter=`grep -c "$messaggio" "$i"`

        # se il messaggio appare almeno una volta, inserisco in TROVATI il numero di occorrenze e il nome del file
        if $counter -ge 1 
        then
            echo "$counter, `pwd`/$i" >> "$TROVATI"
        fi 

        # controllo del massimo
        if $counter -gt `cat /tmp/max_counter.tmp`
        then 
            echo $counter > /tmp/max_counter.tmp
            echo "`pwd`/$i" > "$MAX"
        fi 
    fi 
done

# lancio la ricorsione
for d in *
do 
    if test -d "$d" -a -x "$d"
    then
        sh identificaRic.sh "$d" "$messaggio"
    fi 
done 