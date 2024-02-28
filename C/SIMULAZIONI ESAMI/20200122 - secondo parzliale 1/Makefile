vaccinazioni: main.o ListaCani.o
	gcc -g -o vaccinazioni main.o ListaCani.o

main.o: main.c ListaCani.c 	
	gcc -g -c main.c 

ListaCani.o: ListaCani.c ListaCani.o 
	gcc -g -c ListaCani.c 