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
    int *parent_vertices;
} Graph;


typedef struct Stack {
    int *array;
    int top;
    int max_size;
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
    graph->parent_vertices = (int *) malloc(vertices * sizeof(int));
    
    for (int i = 0; i < vertices; i++) {
        graph->adjLists[i] = NULL;
        graph->visited[i] = false;
        graph->parent_vertices[i] = -1;
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
    free(graph->parent_vertices);
    free(graph);
}


Stack *create_stack(int max_size) {
    Stack *stack = (Stack *) malloc(sizeof(Stack));
 
    stack->array = (int *) malloc(max_size * sizeof(int));
    stack->top = -1;
    stack->max_size = max_size;
 
    return stack;
}


void push_stack(Stack *stack, int value) {
    stack->array[++stack->top] = value;
}
 
 
int pop_stack(Stack *stack) {
    return stack->array[stack->top--];
}
 
 
bool is_empty_stack(Stack *stack) {
    return stack->top == -1;
}
 
 
void free_stack(Stack *stack) {
    free(stack->array);
    free(stack);
}


bool depth_visit(Graph *graph, Stack *stack, int start_vertex) {
    Node *adj_list_node;
    int current_vertex, adj_vertex;
 
    push_stack(stack, start_vertex);
 
    while (!is_empty_stack(stack)) {
        current_vertex = pop_stack(stack);
        graph->visited[current_vertex] = true;
 
        adj_list_node = graph->adj_lists[current_vertex];
 
        while (adj_list_node != NULL) {
            adj_vertex = adj_list_node->vertex;
 
            if (adj_vertex != graph->parent_vertices[current_vertex]) {
                if (graph->visited[adj_vertex]) {
                    return false;
                }
                graph->parent_vertices[adj_vertex] = current_vertex;
                push_stack(stack, adj_vertex);
            }
 
            adj_list_node = adj_list_node->next;
        }
    }

    return true;
}


bool depthFirstSearch(Graph *graph, Stack *stack) {
    for (int i = 0; i < graph->num_vertices; i++) {
        if (graph->adj_lists[i] != NULL && !graph->visited[i]) {
            if (!depth_visit(graph, stack, i)) {
                return false;
            }
        }
    }
    return true;
}


bool isAcyclic(Graph *graph) {
    Stack *stack;
    bool is_acyclic;

    stack = create_stack(graph->numVertices);
    is_acyclic = depthFirstSearch(graph, stack);

    free_stack(stack);

    return is_acyclic;
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