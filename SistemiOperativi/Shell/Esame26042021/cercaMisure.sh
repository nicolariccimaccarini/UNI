#!/bin/sh
# cercaMisure <dir> <header>

# controlloa argomenti
if test $# -ne 2
then 
    echo "Errore parametri - Uso: cercaMisure <dir> <header>"
    exit 1
fi 

dir="$1"
shift
header="$1"
shift

# controllo che dir sia una nome assoluto di directory 
case $dir in
    /*)
        # controllo che dir sia una directory e che sia eseguibile
        if test ! -d $dir -a -x $dir
        then 
            echo "Errore: $dir deve essere una directory eseguibile"
            exit 2
        fi 
        ;;
    *)
        echo "Errore: $dir deve essere un nome assoluto di directory"
        exit 3
        ;;
esac

# aggiorno il path
PATH=$PATH:`pwd`
export PATH

MISURE=$HOME/misure.txt
echo "" > $MISURE
export MISURE

# creo i file tmp
MAX_FILE=/tmp/max_file.txt
export MAX_FILE
echo "0" > $MAX_FILE

MAX_SOTTODIR=/tmp/max_sottodir.txt
export MAX_SOTTODIR
echo "" > $MAX_SOTTODIR

# chiamo la ricorsione
sh cercaMisure.sh "$dir" "$header"

# stampo i risultati
echo "La sottodirectory che contiene il numero maggiore il maggior numero di file e': `cat $MAX_SOTTODIR` con `cat $MAX_FILE` file"

# rimuovo i file tmp
rm -f $MAX_FILE
rm -f $MAX_SOTTODIR