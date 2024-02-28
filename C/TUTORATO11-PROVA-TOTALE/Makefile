pazienti: main.o listaPazienti.o
	gcc  -g -o pazienti main.c listaPazienti.c

main.o: main.c listaPazienti.h
	gcc -g -c main.c

listaPazienti.o: listaPazienti.c listaPazienti.h
	gcc -g -c listaPazienti.c

