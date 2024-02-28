#!/bin/sh

cd "$1"
counter=0

for i in *.txt
do 
    if test -f "$i" -a -w "$i"
    then 
        # due soluzioni alternative:
        # if test `head -n 1 $1 | cut -f 3 -d ','` = preliminare
        if test `head -n 1 $1 | grep preliminare`
        then 
            echo "`pwd`/$i e' preliminare"
            # rm commentata per sicurezza
            # rm -f "$i"
            counter=`expr $counter + 1`
        else
            echo "`pwd`/$i e' definitivo"
        fi 
    fi 
done 

if test $counter -gt `cat /tmp/max_counter`
then 
    echo $counter > /tmp/max_counter
    echo `pwd` > /tmp/max_dirname
fi 

for di in *
do 
    if test -d "$d" -a -x "$d"
    then 
        cancellaVersioniPreliminariAux.sh "$d"
    fi 
done 