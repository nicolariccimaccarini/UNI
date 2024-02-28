#!/bin/sh
# esameRic <nome> <anno>

nome="$1"
shift
anno="$1"
shift

cd "$anno"

for i in *.txt 2>/dev/null 
do 
    if test -f "$i" -a -r "$i"
    then 
        # vado ad analizzare i dati richiesti
        ris=`grep "$nome" "$i" | cut -f 1,3,5 -d ','`
        # se nel file log ci sono infotmazioni relative al fiume di interesse
        if test "$ris"
        then 
            # aggiungo le informazioni al file di log
            echo "$ris" >> "$FILELOG"
            # prelevo il livello idrometrico
            livello=`echo "$ris" | cut -f 1 -d ','`
            # se il valore riscontrato e' piu' basso di quello trovat finora lo aggiorno
            if test "$livello" -lt `cat "$LIVELLOMINIMO"`
            then 
                echo "$livello" > "$LIVELLOMINIMO"
                echo "$i" > "$GIORNOMINIMO"
            fi 
        fi 
    fi 
done 

for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        sh esameRic.sh "$nome" "$anno"
    fi 
done 