#!/bin/sh

# CONTROLLO PARAMETRI
if test $# != 2
then 
    echo "Uso: trova <dir> <file>"
    exit 1
fi 

# il primo argomento deve essere una directory assoluta
dir="$1"
case "$dir" in 
    /*) ;;
    *) echo "Errore: il primo argomento deve essere una directory assoluta"
    exit 2 ;;
esac

if test -d "$dir"
then 
    echo "Errore: il primo argomento non e una directory"
    exit 3
fi 

# RICERCA
file="$2"
PATH=$PATH: `pwd` # aggiunge directory script corrente al PATH
export PATH

# evita loop se non ho i permessi per entrare in una directory
if test -x "$dir"
then 
    cd $dir
    for i in *
    do 
        if test -d "$i"
        then 
            # ricorsione
            trovaRic.sh "`pwd`"/"$i" "$file"
        else 
            if test -f "$i" -a "$i" = "$file"
            then 
                echo `pwd`/$i
            fi 
        fi 
    done 
fi 