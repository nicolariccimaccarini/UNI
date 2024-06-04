#!/bin/sh
# trovaFileGiochi <dir>

# controllo parametri
if test $# -ne 1
then 
    echo "Errore: numero parametri errato - Uso: trovaFileGiochi <dir>"
    exit 1
fi 

dir="$1"
shift 

# controllo che $dir sia una directory relativa
case $dir in 
    /*) 
        echo "Errore: $dir deve essere un nome di directory relativa"
        exit 2
        ;;
    *)
        # controllo che sia una directory e che abbia i diritti di esecuzione
        if test ! -d "$dir"
        then 
            echo "Errore: $dir deve essere un nome di directory"
            exit 3
        fi 
        if test ! -x "$dir"
        then 
            echo "Errore: impossibile eseguire su $dir"
            exit 4
        fi 
        ;;
esac 

# aggiorno il path
PATH=$PATH:`pwd`
export PATH

TROVATO="$HOME"/tmp/trovato.txt
export TROVATO

# verifico se esiste il file trovato.txt nella home
if test ! -f "$TROVATO"
then
    > "$TROVATO"
fi

MAX_COUNTER=/tmp/.tmp_max_counter
export MAX_COUNTER

MAX_FILE=/tmp/.tmp_max_file_name
export MAX_FILE

echo 0 > "$MAX_COUNTER"
> "$MAX_FILE"

# chiamo la ricorsione
sh trovaFileGiochiRic.sh "$dir"

echo "Il file con il maggior numero di righe e' `cat $NOMEMASSIMO`"
echo "Il maggior numero di righe e' `cat $RIGHEMASSIMO`"

rm -f "$MAX_COUNTER"
rm -f "$MAX_FILE"