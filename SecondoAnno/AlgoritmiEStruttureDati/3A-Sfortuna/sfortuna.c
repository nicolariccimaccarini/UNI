#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>


// Represents an edge in a graph.
typedef struct {
    int u;  // source vertex
    int v;  // destination vertex
    int w;  // weight of the edge
} Edge;


typedef struct {
    int v; // vertex number
    int d; // distance from the source
} Node;


// Swap two nodes in the heap
void swap(Node *x, Node *y) {
    Node *temp = x;
    *x = *y;
    *y = *temp;
}


// Find the vertex in the minumum distance
int minimumDistance(Node *heap, bool *visited, int size) {
    int minIndex = -1;

    for (int v = 0; v < size; v++) {
        if (!visited[v] && (minIndex == -1 || heap[v].d < heap[minIndex].d)) {
            minIndex = v;
        }
    }

    return minIndex;
}


// Using Dijkstra's algorithm for finding the minimum path
void findMinimumPath(Edge *edges, int n, int m, FILE *out_file) {
    Node heap[n];
    bool visited[n];

    for (int v = 0; v < n; v++) {
        heap[v].v = v;
        heap[v].d = INT_MAX;
        visited[v] = false;
    }

    // set the distance from teh source to istself to 0
    heap[0].d = 0;

    // Dijkstra algorithm
    for (int i = 0; i < n; i++) {
        int u = minimumDistance(heap, visited, n);

        visited[u] = true;

        for (int j = 0; j < m; j++) {
            int v = edges[j].v;
            int w = edges[j].w;

            if (!visited[v] && heap[u].d != INT_MAX && heap[u].d + w < heap[v].d) {
                heap[v].d = heap[u].d + w;
            }
        }
    }

    fprintf(out_file, "%d\n", heap[n-1].d);
}


int sfortuna(FILE *in_file, FILE *out_file) {
    int n, m;
    fscanf(in_file, "%d %d", &n, &m);

    Edge *edges = malloc(m * sizeof(Edge));
    for (int i = 0; i < m; i++) {
        fscanf(in_file, "%d %d %d", &edges[i].u, &edges[i].v, &edges[i].w);
    }

    findMinimumPath(edges, n, m, out_file);

    // Free the memory allocated for the edges array
    free(edges);
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