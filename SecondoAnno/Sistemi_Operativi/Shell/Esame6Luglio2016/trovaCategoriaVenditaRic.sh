#!/bin/sh

dir="$1"
shift
listaArticoli="$1"
shift

# mi sposto nella directory relativa alla categoria
cd "$dir"

# creo il file temporaneo relativo alla categoria rappresentata dalla directory "$dir"
> /tmp/max_cat_counter

# per ogni file con estensione lof controllo che l'atricolo sia stato venduto 
# selezionando il campo 5 tramite cut
for i in `ls *.log 2>/dev/null`
do 
    if test -r "$i" -a "`head -l "$i" | cut -f 5 -d ','`" = "venduto"
    then 
        echo `head -l "$i" >> /tmp/cat_counter`
    fi 
done 

if test `wc -l < /tmp/cat_counter` -gt `wc -l < /tmp/max_cat_counter`
then 
    # aggiorno la categoria con piu' articoli venduti
    cat /tmp/cat_counter > /tmp/max_cat_counter
    # aggiorno il nome della categoria
    echo "$1" > /tmp/max_nome_categoria
fi 

# lancio la ricorsiva
for d in *
do 
    if test -d "$d" -a -x "$d"
    then 
        trovaCategoriaVenditaRic.sh "$d"
    fi 
done 