#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// creo la struttura dati di un grafo indiretto usando una lista di adiacenza
typedef struct Node {
    int vertex;
    struct Node* next;
} Node;


typedef struct Graph {
    int numVertices;
    Node **adjLists;
    bool *visited;
    int *parentVertices;
} Graph;


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


Graph* createGraph(int vertices) {
    Graph *graph = malloc(sizeof(Graph));

    graph->numVertices = vertices;
    graph->adjLists = (Node **) malloc(vertices * sizeof(Node*));
    graph->visited = (bool *) malloc(vertices * sizeof(bool));
    graph->parentVertices = (int *) malloc(vertices * sizeof(int));
    
    for (int i = 0; i < vertices; i++) {
        graph->adjLists[i] = NULL;
        graph->visited[i] = false;
        graph->parentVertices[i] = -1;
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
    free(graph->visited);
    free(graph->parentVertices);
    free(graph);
}


Stack *createStack(int maxSize) {
    Stack *stack = (Stack *) malloc(sizeof(Stack));
 
    stack->array = (int *) malloc(maxSize * sizeof(int));
    stack->top = -1;
    stack->maxSize = maxSize;
 
    return stack;
}


void pushStack(Stack *stack, int value) {
    stack->array[++stack->top] = value;
}
 
 
int popStack(Stack *stack) {
    return stack->array[stack->top--];
}
 
 
bool isEmptyStack(Stack *stack) {
    return stack->top == -1;
}
 
 
void freeStack(Stack *stack) {
    free(stack->array);
    free(stack);
}


bool depthVisit(Graph *graph, Stack *stack, int startVertex) {
    Node *adjListNode;
    int currentVertex, adjVertex;
 
    pushStack(stack, startVertex);
 
    while (!isEmptyStack(stack)) {
        currentVertex = popStack(stack);
        graph->visited[currentVertex] = true;
 
        adjListNode = graph->adjLists[currentVertex];
 
        while (adjListNode != NULL) {
            adjVertex = adjListNode->vertex;
 
            if (adjVertex != graph->parentVertices[currentVertex]) {
                if (graph->visited[adjVertex]) {
                    return false;
                }
                graph->parentVertices[adjVertex] = currentVertex;
                pushStack(stack, adjVertex);
            }
 
            adjListNode = adjListNode->next;
        }
    }

    return true;
}


bool depthFirstSearch(Graph *graph, Stack *stack) {
    for (int i = 0; i < graph->numVertices; i++) {
        if (graph->adjLists[i] != NULL && !graph->visited[i]) {
            if (!depthVisit(graph, stack, i)) {
                return false;
            }
        }
    }
    return true;
}


bool isAcyclic(Graph *graph) {
    Stack *stack;
    bool res;

    stack = createStack(graph->numVertices);
    res = depthFirstSearch(graph, stack);

    freeStack(stack);

    return res;
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

    fprintf(out_file, (isAcyclic(grafo)) ? "TRUE\n" : "FALSE\n");

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