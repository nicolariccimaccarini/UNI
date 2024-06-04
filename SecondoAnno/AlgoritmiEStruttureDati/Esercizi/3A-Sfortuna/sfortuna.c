
#include <stdio.h>
#include <stdlib.h>

// Struttura per rappresentare un arco
typedef struct {
    int u, v, w; // Assicurati che la struttura Edge abbia il campo 'w' per il peso
} Edge;


// Struttura per rappresentare un grafo
typedef struct {
    int V, E;
    Edge* edges;
} Graph;


// Struttura per rappresentare un subset per Union-Find
typedef struct {
    int parent;
    int rank;
} Subset;


// Ordino gli archi in ordine crescente di peso
int compareEdges(const void *a, const void *b) {
    Edge* edgeA = (Edge *)a;
    Edge* edgeB = (Edge *)b;
    return edgeA->w - edgeB->w;
}


// Trova il set di un elemento i (con path compression)
int find(Subset subsets[], int i) {
    if (subsets[i].parent != i) {
        subsets[i].parent = find(subsets, subsets[i].parent);
    }
    return subsets[i].parent;
}


// Unisce due set di x e y (utilizza union by rank)
void unionSets(Subset subsets[], int x, int y) {
    int rootX = find(subsets, x);
    int rootY = find(subsets, y);
    
    if (subsets[rootX].rank < subsets[rootY].rank) {
        subsets[rootX].parent = rootY;
    } else if (subsets[rootX].rank > subsets[rootY].rank) {
        subsets[rootY].parent = rootX;
    } else {
        subsets[rootY].parent = rootX;
        subsets[rootX].rank++;
    }
}


// Funzione principale per trovare il MST utilizzando l'algoritmo di Kruskal
int kruskalMST(Graph *graph) {
    int V = graph->V;
    Edge *result = malloc(V * sizeof(Edge));
    int e = 0; // Indice per gli archi del risultato
    int i = 0; // Indice per gli archi ordinati

    // Ordina tutti gli archi
    qsort(graph->edges, graph->E, sizeof(graph->edges[0]), compareEdges);

    // Alloca memoria per creare V subset
    Subset *subsets = malloc(V * sizeof(Subset));
    for (int v = 0; v < V; ++v) {
        subsets[v].parent = v;
        subsets[v].rank = 0;
    }

    // Numero di archi da essere aggiunti al MST deve essere V-1
    while (e < V - 1 && i < graph->E) {
        Edge nextEdge = graph->edges[i++];

        int x = find(subsets, nextEdge.u);
        int y = find(subsets, nextEdge.v);

        if (x != y) {
            result[e++] = nextEdge;
            unionSets(subsets, x, y);
        }
    }

    int minimumCost = 0;
    for (i = 0; i < e; ++i) {
        minimumCost += result[i].w;
    }

    free(result);
    free(subsets);

    return minimumCost;
}


int sfortuna(FILE *in_file, FILE *out_file) {
    int N, M;
    fscanf(in_file, "%d %d", &N, &M);

    Graph graph;
    graph.V = N;
    graph.E = M;
    graph.edges = malloc(M * sizeof(Edge));

    for (int i = 0; i < M; ++i) {
        fscanf(in_file, "%d %d %d", &graph.edges[i].u, &graph.edges[i].v, &graph.edges[i].w);
    }

    int minimumCost = kruskalMST(&graph);
    fprintf(out_file, "%d\n", minimumCost);

    free(graph.edges);
    return 0;
}


int main(void) {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        fprintf(stderr, "Errore nell'apertura del file di input.\n");
        return EXIT_FAILURE;
    }

    sfortuna(in_file, out_file);

    // Chiudo i file di input e output
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        fprintf(stderr, "Errore nella chiusura del file di input/output.\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}