#!/bin/sh
# trovaFornitoriMiglioriRic.sh <dir> <cod_parte> <num>

dir="$1"
shift
cod_parte="$1"
shift
num="$1"

cd "$dir"

# per ogni file nella direcory dir controllo i fornitori che possiedono il pezzo di ricambio richiesto
for i in `ls *.txt 2>/dev/null`
do 
    # controllo che il fornitore abbia il numero di pezzi di ricambio >= al numero richiesto
    # nome_fornitore, codice_parte, nome_parte, num_pezzi
    counter=`grep "$cod_parte" "$i" | cut -f 4 -d ,`
    forn=`grep "$cod_parte" "$i" | cut -f 1 -d ,`

    # controllo che il numero di pezzi che il fornitore ha a disposizione siano almeno pari a quelli richiesti
    if test "$counter" -ge "$num"
    then 
        # devo trovare il fornitore che ha la scorta piu' alta di pezzi
        if test "$counter" -ge `cat "$MAX_COUNTER"`
        then 
            # aggiorno il file temp
            echo "$counter" > "$MAX_COUTNER"
            echo "$forn" > "$MAX_FORNITORE"
        fi 
    fi 
done 

# chiamo la ricorsione
for d in *
do 
    if test -d "$d"
    then 
        sh trovaFornitoriMiglioriRic.sh "$d" "$cod_parte" "$num"
    fi 
done 