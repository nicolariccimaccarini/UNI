frequenze.o: main.o listaParole.o
	gcc -g -o frequenze main.o listaParole.o

main.o: main.c listaParole.h
	gcc -g -c main.c

listaParole: listaParole.c listaParole.h
	gcc -g -c listaParole.c 