#!/bin/sh
# recuperaRic.sh <dir> <recuperati> <titolo>

dir="$1"
shift
recuperati="$1"
shift
titolo="$1"
shift

cd "$dir"

COUNTER=0

for i in `ls *.bak 2>/dev/null`
do 
    if test -f $i -a -r "$i" -a `head -1 "$i"="$titolo"`
    then 
        cp $i "$recuperati"
        COUNTER=`expr $COUNTER + 1`
    fi
done 

if test `cat $MAX_NFILE` -lt $COUNTER
then 
    echo $COUNTER > $MAX_NFILE
    echo `pwd` > $MAX_DIR
fi

TOT=`cat $TOTALE`
echo `expr $TOT + $COUNTER` > $TOTALE

# ricorsione
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        recuperaRic.sh "$d" "$recuperati" "$titolo"
    fi 
done 