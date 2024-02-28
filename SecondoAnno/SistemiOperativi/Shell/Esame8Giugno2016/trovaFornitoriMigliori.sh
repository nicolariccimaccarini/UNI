#!/bin/sh
# trova _fornitori_ migliori <dir> <cod_parte> <num>

# controllo argomenti
if test $# -ne 3 
then 
    echo "Errore argomenti - Uso: trova _fornitori_ migliori <dir> <cod_parte> <num>" 
    exit 1
fi

dir="$1"
shift
cod_parte="$1"
shift
num="$1"
shift

# controllo che dir sia un nome assoluto di directory
case $dir in
    /*)
        # controllo che sia effettivamente una directory e  he sia eseguibile
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

# controllo che num sia composto da solo numeri
case $num in
    *[!0-9]*)
        echo "Errore: $num deve essere un numero"
        exit 4
        ;;
esac 

# aggiorno il path
PATH=$PATH:`pwd`
export PATH

MAX_FORNITORE=/tmp/max_fornitore.txt
export MAX_FORNITORE
echo "" > $MAX_FORNITORE

MAX_COUNTER=/tmp/max_counter.txt
export MAX_COUNTER
echo "0" > $MAX_COUNTER

# chiamo la ricorsiva
sh trovaFornitoriMiglioriRic.sh $*

# stampa finale
echo "Il fornitore `cat $MAX_FORNITORE` ha il maggior numero di pezzi di ricambio `cat $MAX_COUNTER` per $cod_parte"

# rimuovo i file temporanei
rm -f $MAX_FORNITORE
rm -f $MAX_COUNTER