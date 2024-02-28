#!/bin/sh

dir="$1"
shift
messaggio="$1"
shift

cd "$dir"

# cerco i file sui quali ho diritto di lettura che contengono il messaggio deginito in $messaggio
for i in *
do 
    if test -f "$i" -a -r "$i"
    then 
        # message counter
        mc=`grep -c "$messaggio" "$i"
        if test $mc -gt 0
        then 
            echo "$md, `pwd`/$i" >> "$TROVATI"
        fi

        # Controllo del massimo
        if test $mc -gt `cat /tmp/max_counter.tmp`
        then 
            echo $mc > /tmp/max_counter.tmp
            echo "`pwd`/$i" > "$MAX"
        fi 
    fi 
done 

# esploro ricorsivamente il sottoalbero definito da dir
for d in *
do  
    if test -d "$d" -a -x "$d"
    then 
        sh identificaRic.sh "$d" "$messaggio"
    fi 
done 