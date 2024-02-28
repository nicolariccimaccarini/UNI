#!/bin/sh
# esame <genere> <tipo> <anno>

# controllo argomenti
if test $# -ne 3
then 
    echo "Errore argomenti - Uso esame <genere> <tipo> <anno>"
    exit 1
fi 

genere="$1"
shift
tipo="$1"
shift
anno="$1"

# controllo che genere e tipo siano stringhe contenenti solo lettere
case $genere in 
    *[0-9]*)
        echo "Errore: $genere deve essere una stringa di soli caratteri"
        exit 2
        ;;
esac

case $tipo in 
    *[0-9]*)
        echo "Errore: $tipo deve essere una stringa di soli caratteri"
        exit 3
        ;;
esac

# controllo che anno sia un nome assoluto di directory
case $anno in
    /*)
        # controllo che sia una director e che abbia i diritti di esecuzione
        if test ! -d "$anno" -a -x "$anno"
        then   
            echo "Errore: $anno deve essere una directory e deve essere eseguibile"
            exit 4
        fi 
        ;;
    *) 
        echo "Errore: $anno deve essere un nome assoluto di directory"
        exit 5
        ;;
esac

# esporto il path
PATH=$PATH:`pwd`
export PATH

# creo i file 
RISULTATI=$HOME/risultati.txt
echo "" > $RISULTATI
export RISULTATI

MAX_MESE=/tmp/max_mese.txt
echo "" > $MAX_MESE
export MAX_MESE

MAX_TITOLI=/tmp/max_titoli.txt
echo "0" > $MAX_TITOLI
export MAX_TITOLI

# chiamo la ricorsione
sh esameRic.sh "$genere" "$tipo" "$anno"


if test `cat $MAX_TITOLI` -gt 0
then 
    cat $RISULTATI | sort -r -n

    echo "mese con il maggior numero di titoli di interesse:"
    cat "$MAX_MESE"
    cat "$MAX_TITOLI"
else 
    echo "warning: nessun titolo di interesse rovato"
fi 

rm -f $MAX_MESE
rm -f $MAX_TITOLI