#!/bin/sh
# cercafile.sh <stringa> <dir> <num>

if test $# -ne 3
then 
    echo Errore: il numero degli argomenti deve essere 3
    echo "Uso: cercafile.sh <stringa> <dir> <num>"
    exit 1
fi 

if ! test -d "$2"
then 
    echo "Errore: $2 non e' una directory"
    exit 2
fi 

case $2 in 
    /*) ;;
    *) echo "Errore: il percorso $2 non e' assoluto"
    exit 3;;
esac

if !test "$3" -gt 0
then 
    echo "Errore: $3 non e' un numero positivo"
    exit 4
fi 

> /tmp/.max_counter.tmp
> /tmp/.max_dirname.tmp

PATH=$PATH:`pwd`
export PATH

cercaStringa.sh "$1" "$2" $3
echo "La directory con il maggior numero di file e': `cat /tmp/.max_dirname.tmp` con `wc -l < /tmp/.max_counter.tmp` file"

rm -f /tmp/.max_counter.tmp
rm -f /tmp/.max_dirname.tmp