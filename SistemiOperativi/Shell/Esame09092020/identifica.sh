#!/bin/sh
# identifica <dir> <messaggio>

# controllo argomenti
if test $# -ne 2
then 
    echo "Errore argomenti - Uso identifica <dir> <messaggio>"
    exit 1
fi 

dir="$1"
shift
messaggio="$1"
shift

# controllo che dir sia un nome relativo di directory
case $dir in
    /*) 
        echo "Errore: $dir deve essere un nome relativo di directory"
        exit 2
        ;;
    *)
        # controllo che dir sia una directori e che sia eseguibile
        if test ! -d "$dir" -a -x "$dir"
        then 
            echo "Errore: $dir deve essere una directory e deve essere eseguibile"
            exit 3
        fi 
        ;;
esac 

# aggiorno il path
PATH=$PATH:`pwd`
export PATH

# clean file
> "$dir/Trovati"
> "$dir/Max"

# creo i file di supporto
TROVATI=`pwd`/"$dir"/Trovati
export TROVATI

MAX=`pwd`/"$dir"/Max
export MAX

echo 0 > /tmp/max_counter.tmp

# lancio la ricorsione
sh identificaRic.sh "$dir" "$messaggio"

echo "I 5 file che contengono il maggior numero di occorrenze sono:"
echo `sort -r -n "$TROVATI" | cut -f 2 -d ,`

rm -f /tmp/max_counter.tmp