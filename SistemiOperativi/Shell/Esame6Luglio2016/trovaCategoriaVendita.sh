#!/bin/sh
# trovaCategoriaVendita <dir> <listaArticoli>

dir="$1"
shift
listaArticoli="$1"
shift

if test $# -ne 2
then    
    echo "Errore argomenti"
    echo "USo: trovaCategoriaVendita <dir> <listaArticoli>"
    exit 230
fi 

# controllo argomenti
case $dir in
    /*) 
        # controllo che $dir sia una directory
        if test ! -d $dir
        then 
            echo "Errore: $dir non e' una directory"
            exit 1
        fi 
        ;;
    *) 
        echo "Errore: $dir non e' un path di directory assoluto"
        exit 2
    ;;
esac

# controllo che $dir sia eseguibile 
if test ! -x $dir
then 
    echo "Errore: $dir non ha i diritti di esecuzione"
    exit 3
fi 

# esporto il path 
PATH=$PATH:`pwd`
export PATH

# creo i file temporanei relativi alla categoria con piu' articoli venduti
> /tmp/max_nome_categoria.tmp
# contiene la prima riga di ogni file di log relativo ad ogni articolo venduto alla fine
# della ricerca copiata nel file di input
> /tmp/max_cat_counter.tmp

# chiamo la ricorsione
trovaCategoriaVenditaRic.sh "$dir"

# stampa dei risultati a video
echo "`cat /tmp/max_nome_categoria` e' la categoria con piu' articoli venduti, per un totale di `wc -l < /tmp/max_cat_counter` articoli"
# stampa su file lista articoli
cat /tmp/max_cat_counter > "$listaArticoli"

# rimozione file temporanei
rm -f /tmp/max_cat_counter
rm -f /tmp/max_nome_categoria