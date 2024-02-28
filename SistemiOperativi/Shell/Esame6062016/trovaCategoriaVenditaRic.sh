#!/bin/sh
# trovaCategoriaVenditaRic.sh dir lista_articoli

dir="$1"
shift 
lista_articoli="$1"
shift

cd "$dir"

#creo il file temporaneo relativo alla categoria rappresentata dalla directory "$dir"
> /tmp/cat_counter

for i in `ls *.log 2>/dev/null`
do 
    if test -r "$i" -a "`head -1 "$i" | cut -f 5 -d ','`" = "venduto"
    then 
        echo `head -1 "$i" >> /tmp/cat_counter`
    fi 
done 
     
if test `wc -l < /tmp/cat_counter` -gt `wc -l < /tmp/max_cat_counter`
then
    #aggiorno la categoria con più articoli venduti
    cat /tmp/cat_counter > /tmp/max_cat_counter
    #aggiorno il nome della categoria
    echo "$1" > /tmp/max_nome_categoria
fi
#lancio la ricorsione per ogni sottocartella
for d in *
do
    if test -d "$d" -a -x "$d"
    then
        trova_categoria_vendite_ric.sh "$d"
    fi
done