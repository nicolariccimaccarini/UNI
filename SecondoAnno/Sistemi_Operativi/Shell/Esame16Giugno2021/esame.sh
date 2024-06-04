#!/bin/sh

# controllo degli argomenti
if test $# -ne 3
then 
    echo "Errore: numero sbagliato di argomenti"
    echo "Uso: esame <genere> <tipo> <anno>"
    exit 1
fi 

genere="$1"
shift
tipo="$1"
shift
anno="$1"
shift

# controllo che <anno> sia nome assoluto
case $anno in
    /*) 
        #controllo che <anno> sia una directory
        if test -d "$anno"
        then 
            echo "Errore: $anno non e' una directory"
            exit 2 
        fi 
        ;;
        *) 
            echo "Errore: $anno non e' nome assoluto"
            exit 3
            ;;
esac

# controllo che <anno> sia eseguibile
if test ! -x "$anno"
then 
    echo "Errore: non posso eseguire in $anno"
    exit 4
fi 

# aggiorno la variabile PATH con la directory corrente
PATH=$PATH:`pwd`
export PATH

# creo il file temporaneo per tenere traccia del file che contiene il
# maggior numero di titoli di interesse
export MAX_FILE=/tmp/.file.tmp
export MAX_COUNTER=/tmp/.counter.tmp
echo "" > "$MAX_FILE"
echo "0" > "$MAX_COUNTER"

# creo/sovrascrivo il file risultati.txt in $HOME
echo "" > $HOME/risultati.txt

sh esameRic.sh "$genere" "$tipo" "$anno"

if test `cat $MAX_COUNTER` -gt 0
then 
    cat $HOME/risultati.txt | sort -r -n

    echo "mese con il maggior numero di titoli di interesse: "
    cat "$MAX_FILE"
    cat "$MAX_COUNTER"
else 
    echo "Warning: nessun titolo di interesse trovato"
fi 

rf -f "$MAX_FILE"
rm -f "$MAX_COUNTER"