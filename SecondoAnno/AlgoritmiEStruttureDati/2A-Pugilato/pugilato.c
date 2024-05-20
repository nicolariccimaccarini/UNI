#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


// Struttura dati del grafo
typedef struct Node {
    int vertex;
    struct Node* next;
} Node;


typedef struct Graph {
    int numVertices;
    Node **adjLists;
    int *colors; // -1: uncolored, 0: color 0, 1: color 1
} Graph;


// creo la stack
typedef struct Stack {
    int *array;
    int top;
    int maxSize;
} Stack;


Node* createNode(int v) {
    Node* newNode = malloc(sizeof(Node));
    newNode->vertex = v;
    newNode->next = NULL;
    return newNode;
}


// Creo un grafo inizializzando le liste di adiacenza e i colori dei nodi.
Graph* createGraph(int vertices) {
    Graph *graph = malloc(sizeof(Graph));

    graph->numVertices = vertices;
    graph->adjLists = (Node **) malloc(vertices * sizeof(Node*));
    graph->colors = (int *) malloc(vertices * sizeof(int));
    
    for (int i = 0; i < vertices; i++) {
        graph->adjLists[i] = NULL;
        graph->colors[i] = -1;
    }

    return graph;
}


// Aggiungo un lato non orientato tra due nodi del grafo.
void addEdge(Graph *graph, int src, int dest) {
    Node* newNode = createNode(dest);
    newNode->next = graph->adjLists[src];
    graph->adjLists[src] = newNode;

    newNode = createNode(src);
    newNode->next = graph->adjLists[dest];
    graph->adjLists[dest] = newNode;
}


// Utilizzo la DFS colorando i nodi per verificare se una componente del grafo e' bipartita
bool isBipartite(Graph *graph, int startVertex) {
    int *stack = (int *) malloc(graph->numVertices * sizeof(int));
    int top = -1;
    stack[++top] = startVertex;
    graph->colors[startVertex] = 0;

    while (top != -1) {
        int vertex = stack[top--];
        Node *adjList = graph->adjLists[vertex];
        while (adjList != NULL) {
            int adjVertex = adjList->vertex;
            if (graph->colors[adjVertex] == -1) {
                graph->colors[adjVertex] = 1 - graph->colors[vertex]; // color with opposite color
                stack[++top] = adjVertex;
            } else if (graph->colors[adjVertex] == graph->colors[vertex]) {
                free(stack);
                return false;
            }
            adjList = adjList->next;
        }
    }

    free(stack);
    return true;
}


// Controllo se il grafo puo' essere partizionato in due insiemi senza avere rivalita' interne
bool canBePartitioned(Graph *graph) {
    for (int i=0; i<graph->numVertices; i++) {
        if (graph->colors[i] == -1) {
            if (!isBipartite(graph, i)) {
                return false;
            }
        }
    }

    return true;
}


void freeGraph(Graph *graph) {
    for (int i = 0; i < graph->numVertices; i++) {
        Node* adjList = graph->adjLists[i];
        while (adjList != NULL) {
            Node* tmp = adjList;
            adjList = adjList->next;
            free(tmp);
        }
    }

    free(graph->adjLists);
    free(graph->colors);
    free(graph);
}


int pugilato(FILE *in_file, FILE *out_file) {
    int N, M;           // N = numero totale di pugili --- M = numero di match (n righe dopo N e M)
    int boxer1, boxer2; // pugili che si scontrano per ogni match

    Graph *grafo;       // creo il grafo

    // leggo N e M
    fscanf(in_file, "%d %d", &N, &M);

    grafo = createGraph(N);

    for (int i=0; i<M; i++) {
        fscanf(in_file, "%d %d", &boxer1, &boxer2);
        addEdge(grafo, boxer1, boxer2);             // creo il collegamento
    }

    fprintf(out_file, (canBePartitioned(grafo)) ? "TRUE\n" : "FALSE\n");

    freeGraph(grafo);
    
    return 0;
}


int main(void) {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    pugilato(in_file, out_file);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}