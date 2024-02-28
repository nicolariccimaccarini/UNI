#!/bin/sh
# recupera.sh <dir> <recuperati> <titolo>

# controllo argomenti
if test $# -ne 3
then    
    echo "Errore: numero parametri errato - Uso: recupera.sh <dir> <recuperati> <titolo>"
    exit 1
fi 

dir="$1"
shift
recuperati="$1"
shift
titolo="$1"
shift

# dir e recuperati devono essere nomi assoluti di directory
case $dir in
    /*) 
        if test ! -d "$dir" -o -x "$dir"
        then 
            echo "Errore: $dir deve essere una directory e deve essere eseguibile"
            exit 2
        fi 
        ;;
    *)
        echo "Errore: $dir deve essere un path di directory assoluto"
        exit 3
        ;;
esac

case $recuperati in 
    /*) 
        if test ! -d "$recuperati" -o -x "$recuperati"
        then 
            echo "Errore: $recuperati deve essere una directory e deve essere eseguibile"
            exit 2
        fi 
        ;;
    *)
        echo "Errore: $recuperati deve essere un path di directory assoluto"
        exit 3
        ;;
esac

# Controllo esistenza della directory ripristinati
if test ! -d $ripristinati
  then
  mkdir $ripristinati
fi

# aggiorno il PATH
PATH=$PATH:`pwd`
export PATH

MAX_DIR=/tmp/.max_dir.txt
export MAX_DIR
echo "" > "$MAX_DIR"

MAX_NFILE=/tmp/.max_nfile.txt
export MAX_NFILE
echo "0" > "$MAX_NFILE"

TOT_FILE=/tmp/.tot_file.txt
export TOT_FILE
echo "0" > "$TOT_FILE"

# chiamo la ricorsione
recuperaRic.sh "$dir" "$recuperati" "$titolo"

# stampo i risultati a video
echo "La sottodirectory che contiene il maggior numero di file recuperati e': $MAX_DIR con $MAX_NFILE file"
echo "Il numero totale di file recuperati e': $TOT_FILE"

rm -f "$MAX_DIR"
rm -f "$MAX_NIFLE"
rm -f "$TOT_FILE"