#!/bin/sh
# esame <nome> <anno>

# controllo argomenti
if test $# -ne 2
then 
    echo "Errore: numero di argomenti errato - Uso: esame <nome> <anno>"
    exit 1
fi 

nome="$1"
shift
anno="$1" # directory
shift

# controllo che $anno sia un nome assoluto di directory
case $anno in 
    /*)
        # controllo che sia una directory
        if test ! -d $anno
        then 
            echo "Errore: $anno non e' un nome di directory"
            exit 2
        fi 
        ;;
    *) 
        echo "Errore: $anno non e' un nome assoluto di directory"
        exit 3
        ;;
esac

# controllo che $anno abbia i diritti di esecuzione
if test ! -x "$anno"
then 
    echo "Errore: impossibile eseguire in $anno"
    exit 4
fi 

# controllo che $nome non contenga caratteri numerici
case "$nome" in 
    *[0-9]*) 
        echo "Errore: $nome deve essere solo una stringa di caratteri"
        exit 5
        ;;
esac 

# aggiorno il PATH
PATH=$PATH:`pwd`
export PATH

FILELOG="$HOME"/fiumilog.txt
export FILELOG

LIVELLOMINIMO=/tmp/livello_minimo.txt
export LIVELLOMINIMO
echo 100000 > "$LIVELLOMINIMO"

GIORNOMINIMO=/tmp/giorno_minimo.txt
export GIORNOMINIMO
> "$GIORNOMINIMO"

# elimino il file log se esiste
if test -e "$FILELOG"
then 
    > "$FILELOG"
fi 

# chiamo la ricorsione
sh esameRic.sh "$nome" "$anno"

# stampo i risultati
echo "Il minimo e' stato riscontrato il giorno: `cat $GIORNOMINIMO`"
echo "Livello minimo riscontrato: `cat $LIVELLOMINIMO` metri"

rm -f "$LIVELLOMINIMO"
rm -f "$GIORNOMINIMO"