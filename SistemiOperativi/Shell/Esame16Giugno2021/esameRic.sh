#!/bin/sh

genere="$1"
shift
tipo="$1"
shift
anno="$1"
shift

# entro nella directory
cd "$anno"

# controllo ciascun file con estensione .txt nel PWD
counter=0
for f in `ls *.txt 2>/dev/null`
do 
    # controllo di avere i permessi in lettura su <f>
    if test -f $f -a -r "$f"
    then 
        # conto i titoli di interesse in <f>
        counter=`grep "$genere" "$f" | grep -c "$tipo"`

        # controllo se il file corrente e' il file con piu' titoli di interesse
        if test $counter -gt `cat $MAX_COUNTER`
        then 
            echo "$f" > "$MAX_FILE"
            echo "$counter" > "$MAX_COUNTER"
        fi 

        # se in <f> ci sono titoli di interesse, riporto "recensione, titolo, durata"
        if test $counter -gt 0
        then 
            grep "$genere" "$f" | grep  "$tipo" | cut -d ',' -f 1,3,6 >> $HOME/risultati.txt
        fi 
    fi 
done 

# lancio la ricorsione
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        esameRic.sh "$genere" "$tipo" `pwd`/"$d"
    fi 
done 