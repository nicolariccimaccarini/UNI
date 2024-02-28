#!/bin/sh

# controllo argomenti
if test $# -ne 2
then 
    echo "Errore: numero sbagliato di argomenti"
    echo "Uso: cercaMisure.sh <dir> <header>"
    exit 1
fi 

dir="$1"
shift
header="$1"
shift

# controllo che <dir> sia nome assoluto
case $dir in 
    /*)
        # controllo che <dir> sia una directory
        if test ! -d $dir
        then 
            echo "Errore: $dir non e' una directory"
            exit 3
        fi 
    ;;
    *)
        echo "Errore: $dir non e' nome assoluto"
        exit 2 
        ;;
esac

# controllo che <dir> sia eseguibile
if test ! -x $dir 
then 
    echo "Errore: non posso eseguire $dir"
    exit 4
fi 

# aggiorno la variabile PATH con la directory corrente
PATH=$PATH:`pwd`

# creo il file temporaneo per tenere traccia della sottodirectoru che contiene il maggiorn numero di 
# file che soddisfano la condizione di ricerca
echo "" > /tmp/.dir.tmp
echo "0" > /tmp/.counter.tmp

# creo/sovrascrivo il file misure.txt in $HOME
echo "" > $HOME/misure.txt

# chiamata ricorsiva
sh cercaMisureRic.sh $dir $header

# stampa a video
cat /tmp/.dir.tmp
cat tmp/.counter.tmp

rm -f /tmp/.dir.tmp
rm -f /tmp/.counter.tmp