#!/bin/sh

if test $# -ne 1
then 
    echo "Errore: numero di argomenti"
    exit 1
fi 

case $1 in 
    /*) echo "Errore: il percorso $1 non e' relativo"; exit 2;;
    *);;
esac

if test ! -d "$1" -o ! -x "$1"
then 
    echo "Errore: $1 non e' una directory o non ho i diritti di accesso"
    exit 3
fi 

# Aggiunge il percorso corrente al PATH e lo esporta
PATH=$PATH:`pwd`
export PATH

echo `0` > /tmp/max_counter
> /tmp/max_dirname

cancellaVersioniPreliminariAux.sh "`pwd`"/"$1"

echo "La directory che conteneva il maggior numero di file cancellati e': `cat /tmp/max_dirname`"

rm -f /tmp/max_couter
rm -f /tmp/max_dirname