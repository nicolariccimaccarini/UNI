#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_N 10000

// Struttura per rappresentare un nodo dell'albero di copertura minimo
typedef struct 
{
    int v;
    int key;
} MinHeapNode;


