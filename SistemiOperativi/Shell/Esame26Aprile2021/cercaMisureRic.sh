#!/bin/sh

dir="$1"
shift
header="$1"
shift

# entro nella directory
cd $dir

# controllo ciascun file con estensione .log nella PWD
for f in `ls *.log 2>/dev/null`
do 
    # controllo di avere i permessi di lettura e scrittura su <f>
    if test -r "$f" -a -w "$f"
    then    
        # controllo che <f> contenga <header> nella prima riga
        if test `head -n 1 "$f" | grep -c "$header"` -ge 1
        then 
            # appendo il file in $HOME/misure.txt
            echo `pwd`/"$f" >> $HOME/misure.txt

            # aggiorno il contatore
            counter=`expr $counter + 1`
        fi 
    fi 
done 

# controllo se la <dir> corrente e' la directory con piu' file log che 
# contengono <header> nella prima riga
if test $counter - gt `cat /tmp/.counter.tmp`
then   
    echo "$dir" > /tmp/.dir.tmp
    echo $counter > /tmp/.counter.tmp
fi 

# lancio la riocorsione
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        cercaMisureRic.sh `pwd`/"$d" $header
    fi 
fi 