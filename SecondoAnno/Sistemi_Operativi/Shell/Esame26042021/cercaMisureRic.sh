#!/bin/sh
# cercaMisureRic.sh <dir> <header>

dir="$1"
shift
header="$1"
shift 

cd "$dir"

counter=0
for i in `ls *.log 2>/dev/null`
do 
    # controllo che i file siano legginili e scrivibili
    if test -r "$i" -a -w "$i"
    then 
        # controllo che <i> contenga <header> nella prima riga
        if test `head -n 1 "$i" | grep -c "$header"` - ge 1
        then 
            echo "`pwd`/$i" >> $MISURE
            counter=`expr $counter + 1`
        fi 
    fi 
done 

if test $counter -gt `cat $MAX_COUNTER`
then
    echo $counter > "$MAX_FILE"
    echo "$dir" > "$MAX_SOTTODIR"
fi 

# richiamo la ricorsiva
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        sh cercaMisure.sh "$d" "$header"
    fi 
done 