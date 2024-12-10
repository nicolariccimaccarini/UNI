#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
    int representative;     // rappresentante del set
    int size;               // dimensione del set
} Set;

typedef struct {
    int from;               // nodo di partenza dell'arco
    int to;                 // nodo di destinazione dell'arco
    long long weight;       // peso dell'arco
} Edge;

// Funzione per scambiare due valori interi
void swap(int *x, int *y) {
    int tmp = *x;
    *x = *y;
    *y = tmp;
}

// Funzione di comparazione per l'ordinamento degli archi in base al peso
int weightComparator(const void *x, const void *y) {
    long long w1 = ((const Edge *) x)->weight;
    long long w2 = ((const Edge *) y)->weight;
    return w1 > w2;
}

// Funzione per trovare il rappresentante di un set
int findSet(int n, Set *sets) {
    while (n != sets[n].representative) {
        n = sets[n].representative;
    }
    
    return n;
}

// Funzione per unire due set
void unionSet(int x, int y, Set *sets) {
    x = findSet(x, sets);
    y = findSet(y, sets);

    if (sets[x].size < sets[y].size) {
        swap(&x, &y);
    }

    sets[x].size += sets[y].size;
    sets[y].representative = x;
}

// Funzione per verificare se due nodi appartengono allo stesso set
bool isSameSet(int x, int y, Set *sets) {
    return findSet(x, sets) == findSet(y, sets);
}

// Funzione per calcolare il peso totale dell'albero di copertura minimo utilizzando l'algoritmo di Kruskal
long long kruskal(Set *sets, Edge *edges, int nEdges) {
    Edge e;
    long long sum = 0;

    for (int i = 0; i < nEdges; i++) {
        e = edges[i];
        if (!isSameSet(e.from, e.to, sets)) {
            unionSet(e.from, e.to, sets);
            sum += e.weight;
        }
    }

    return sum;
}

// Funzione principale per risolvere il problema "sfortuna"
void sfortuna(FILE *in_file, FILE *out_file) {
    int N, M;
    
    // Leggo la prima riga dell'input
    fscanf(in_file, "%d %d", &N, &M);

    Set *sets = (Set *) malloc(N * sizeof(Set));
    Edge *edges = (Edge *) malloc(M * sizeof(Edge));

    // Inizializzo i set
    for (int i = 0; i < N; i++) {
        sets[i].representative = i;
        sets[i].size = 1;
    }

    // Leggo tutti gli archi dall'input
    for (int i = 0; i < M; i++) {
        fscanf(in_file, "%d %d %lld", &edges[i].from, &edges[i].to, &edges[i].weight);
    }

    // Ordino gli archi in base al peso
    qsort(edges, M, sizeof(Edge), weightComparator);

    fprintf(out_file, "%lld\n", kruskal(sets, edges, M));

    free(sets);
    free(edges);
}

int main(void) {
    // Apro il file di input/output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        fprintf(stderr, "Errore apertura file di input/output\n");
        exit(EXIT_FAILURE);
    }

    sfortuna(in_file, out_file);

    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        fprintf(stderr, "Errore chiusura file di input/output\n");
        exit(EXIT_FAILURE);
    }

    return 0;
}