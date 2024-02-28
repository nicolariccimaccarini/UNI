#!/bin/sh
# trova _categoria_vendita dir lista_articoli

# controllo argomenti
if test $# -ne 2
then 
    echo "Errore numero argomenti - uso: trova _categoria_vendita dir lista_articoli"
    exit 1
fi 

dir="$1"
shift 
lista_articoli="$1"
shift

case $dir in
    /*) 
        echo "Errore: $dir deve essere un nome relativo di directory"
        exit 2
        ;;
    *)
        # controllo che sia una directory e che sia eseguibile
        if test ! -d "$dir" -a -x "$dir"
        then 
            echo "Errore: $dir deve essere una directory e deve essere eseguibile"
            exit 3
        fi 
        ;;
esac

# esporto il path
PATH=$PATH:`pwd`
export PATH

# creo i file temporanei
MAX_CATEGORIA=/tmp/max_categoria.tmp
echo "" > $MAX_CATEGORIA
export MAX_CATEGORIA

MAX_ARTICOLI=/tmp/max_articoli.tmp
echo "0" > $MAX_ARTICOLI
export MAX_ARTICOLI

# chiamo la ricorsiva
sh trovaCategoriaVenditaRic.sh "$dir"

# stampo
echo "La categoria col numero massimo di articoli e' `cat $MAX_CATEGORIA` con `cat $MAX_ARTICOLI` articoli"

rm -f $MAX_CATEGORIA
rm -f $MAX_ARTICOLI