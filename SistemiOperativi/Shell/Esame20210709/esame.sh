#!/bin/sh
# esame <nome> <anno>

# controllo parametri
if test $# -ne 2
then 
    echo "Errore numero argomenti - Uso: esame <nome> <anno>"
    exit 1
fi 

nome="$1"
shift 
anno="$1"
shift

# controllo che nome sia una stringa di soli caratteri
case $nome in
    *[0-9]*)
        echo "Errore: $nome deve essere una stringa di soli caratteri"
        exit 2
        ;;
esac

# controllo che anno sia un nome assoluto di directory
case $anno in   
    /*)
        # controllo che sia una directory e che sia eseguibile
        if test ! -d "$anno" -a -x "$anno"
        then 
            echo "Errore: $anno deve essere una directory e deve essere eseguibile"
            exit 3
        fi 
        ;;
    *)
        echo "Errore: $anno deve essere un nome assoluto di directory"
        exit 4
        ;;
esac

# esporto il PATH
PATH=$PATH:`pwd`
export PATH

# creo i file temporanei dove mi salvo il giorno in cui e' stato rilevato il livello idrometrico piu' basso
MIN_LIVELLO=/tmp/min_livello.tmp
echo "10000000000" > $MIN_LIVELLO
export MIN_LIVELLO

MIN_GIORNO=/tmp/min_giorno.tmp
echo "" > $MIN_GIORNO
export MIN_GIORNO

FIUMI=$HOME/fiumilog.txt
export FIUMI

# elimino il file log se esiste
if test -e "$FIUMI"
then 
    > "$FIUMI"
fi

# chiamo la ricorsiva
sh esameRic.sh "$nome" "$anno"

# stampo i dati
echo "Il giorno in cui e' stato misurato il livello idrometrico piu' basso (`cat $MIN_LIVELLO` metri) e': `cat $MIN_GIORNO`"

rm -f $MIN_GIORNO
rm -f $MIN_LIVELLO