# /bin/sh
# esameRic <genere> <tipo> <anno>

genere="$1"
shift
tipo="$1"
shift
anno="$1"

cd "$anno"

for i in `ls *.txt 2>/dev/null`
do 
    # controllo che sia leggibile
    if test -r "$i"
    then 
        # conto i titoli di interesse in <i>
        counter=`grep "$genere" "$i" | grep "$tipo" -c`

        # controllo se il file e' il file con piu' titoli di interesse
        if test $counter -gt `cat $MAX_TITOLI`
        then 
            echo "$i" > "$MAX_MESE"
            echo "$counter" > "$MAX_TITOLI"
        fi 

        # se in <i> ci sono titoli di interesse, riporto
        # recensione, titolo, durata
        if test $counter -gt 0
        then 
            grep "$genere" "$i" | grep "$tipo" | cut -d ',' -f 1,3,6 >> $RISULTATI
        fi 
    fi 
done 

# lancio la ricorsione
for d in *
do 
    if test -d "$d" -x "$d"
    then 
        sh esameRic.sh "$genere" "$tipo" "$d"
    fi 
done 