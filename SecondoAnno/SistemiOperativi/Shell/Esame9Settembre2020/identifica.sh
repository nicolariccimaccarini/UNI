#!/bin/sh

# controllo parametri
if test $# -ne 2
then 
    echo "Errore parametri - Uso: identifica <dir> <messaggio>"
    exit 1
fi 

dir="$1"
shift
messaggio="$1"
shift

case $dir in
    /*) 
        echo "Errore: $dir deve essere una directory relativa, non assoluta"
        exit 2
        ;;
    *) 
        if test ! -d "$dir"
        then 
            echo "Errore: $dir non e' una directory relativa"
            exit 3
        fi 
        ;;
esac 

# controllo che <dir> sia eseguibile
if test ! -x "$dir"
then 
    echo "Errore: non posso eseguire in $dir"
    exit 4
fi 

# aggiorno il PATH
PATH=$PATH:`pwd`
export PATH

# clean files
> "$dir/Trovati"
> "$dir/Max"

# la directory dir e' relativo alla directory corrente
export TROVATI="`pwd`/$dir/Trovati"
export MAX="`pwd`/$dir/Max"

echo 0 > /tmp/max_counter.tmp

# Chiamo lo script ricorsivo
sh identificaRic.sh "$dir" "$messaggio"

echo "I 5 file che contengono il maggior numero di occorrenze sono: "
scho `sort -r -n "$TROVATI" | cut -f 2 -d ,`

rm -f /tmp/max_counter.tmp