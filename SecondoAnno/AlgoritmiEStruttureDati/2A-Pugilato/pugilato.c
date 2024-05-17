#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_SIZE 1000000

#define WHITE 0
#define GREY  1
#define BLACK 2

// creo la struttura dati di un grafo indiretto usando una lista di adiacenza
typedef struct Node {
    int vertex;
    struct Node* next;
    struct Node* prev;
} Node;


typedef struct Graph {
    int numVertices;
    Node** adjLists;
    int *colors;
} Graph;


Node* createNode(int v) {
    Node* newNode = malloc(sizeof(Node));
    newNode->vertex = v;
    newNode->next = NULL;
    return newNode;
}


Graph* createGraph(int vertices) {
    Graph *graph = malloc(sizeof(Graph));
    graph->numVertices = vertices;
    graph->adjLists = (Node **) malloc(vertices * sizeof(Node*));
    graph->colors = (int *) malloc(vertices * sizeof(int));
    for (int i = 0; i < vertices; i++) {
        graph->adjLists[i] = NULL;
        graph->colors[i] = WHITE;
    }
    return graph;
}


void addEdge(Graph *graph, int src, int dest) {
    Node* newNode = createNode(dest);
    newNode->next = graph->adjLists[src];
    graph->adjLists[src] = newNode;

    newNode = createNode(src);
    newNode->next = graph->adjLists[dest];
    graph->adjLists[dest] = newNode;
}


bool depthVisit(Graph *graph, int vertex) {
    int stack[MAX_SIZE];
    int top = -1;

    graph->colors[vertex] = GREY;

    stack[++top] = vertex;

    while (top != -1) {
        int currentVertex = stack[top--];

        Node *adjList = graph->adjLists[currentVertex];

        while (adjList != NULL) {
            int connectedVertex = adjList->vertex;

            if (graph->colors[connectedVertex] == WHITE) {
                graph->colors[connectedVertex] = GREY;
                stack[++top] = connectedVertex;
            } else if (graph->colors[connectedVertex] == GREY) {
                return true;
            }

            adjList = adjList->next;
        }

        graph->colors[currentVertex] = BLACK;
    }

    return false;
}


bool depthFirstSearch(Graph *graph) {
    for (int i = 0; i < graph->numVertices; i++) {
        if (graph->adjLists[i] != NULL && graph->colors[i] == WHITE) {
            if (depthVisit(graph, i)) {
                return true;
            }
        }
    }
 
    return false;
}


void freeGraph(Graph *graph) {
    Node *adjList, *tmp;

    for (int v = 0; v < graph->numVertices; v++) {
        adjList = graph->adjLists[v];
        while (adjList != NULL) {
            tmp = adjList;
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
    bool isAcyclic = false;

    Graph *grafo;       // creo il grafo

    // leggo N e M
    fscanf(in_file, "%d %d", &N, &M);

    grafo = createGraph(N);

    for (int i=0; i<M; i++) {
        fscanf(in_file, "%d %d", &boxer1, &boxer2);
        addEdge(grafo, boxer1, boxer2);             // creo il collegamento
    }

    isAcyclic = depthFirstSearch(grafo);
    if (isAcyclic) {
        fprintf(out_file, "FALSE");
    } else {
        fprintf(out_file, "TRUE");
    }

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