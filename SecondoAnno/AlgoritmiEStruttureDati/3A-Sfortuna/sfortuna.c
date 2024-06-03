#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>

#define MAXN 10004
#define MAXM 100005


typedef struct {
    int dest;
    int cost'
    struct Edge *next'
} Edge;


typedef struct {
    Edge *head;
} AdjList;


typedef struct {
    int vertex;
    int key;
} MinHeapNode;


typedef struct {
    int size;
    int capacity;
    int *pos;
    MinHeapNode **array;
} MinHeap;


Edge* newEdge(int dest, int cost) {
    Edge* newNode = (Edge *) malloc(sizeof(Edge));
    newNode->dest = dest;
    newNode->cost = cost;
    newNode->next = NULL;
    return newNode;
}


MinHeapNode* newMinHeapNode(int v, int key) {
    MinHeapNode* minHeapNode = (MinHeapNode *) malloc(sizeof(MinHeapNode));
    minHeapNode->vertex = v;
    minHeapNode->key = key;
    return minHeapNode;
}


MinHeap* createMinHeap(int capacity) {
    MinHeap* minHeap = (MinHeap *) malloc(sizeof(MinHeap));
    minHeap->pos = (int *) malloc(capacity * sizeof(int));
    minHeap->size = 0;
    minHeap->capacity = capacity;
    minHeap->array = (MinHeapNode **) malloc(capacity * sizeof(MinHeapNode*));
    return minHeap;
}


void swapMinHeapNode(MinHeapNode** a, MinHeapNode** b) {
    MinHeapNode* t = *a;
    *a = *b;
    *b = t;
}


void minHeapify(MinHeap* minHeap, int idx) {
    int smallest = idx;
    int left = 2 * idx + 1;
    int right = 2 * idx + 2;

    if (left < minHeap->size && minHeap->array[left]->key < minHeap->array[smallest]->key)
        smallest = left;

    if (right < minHeap->size && minHeap->array[right]->key < minHeap->array[smallest]->key)
        smallest = right;

    if (smallest != idx) {
        MinHeapNode *smallestNode = minHeap->array[smallest];
        MinHeapNode *idxNode = minHeap->array[idx];

        minHeap->pos[smallestNode->vertex] = idx;
        minHeap->pos[idxNode->vertex] = smallest;

        swapMinHeapNode(&minHeap->array[smallest], &minHeap->array[idx]);

        minHeapify(minHeap, smallest);
    }
}


int isEmpty(MinHeap *minHeap) {
    return minHeap->size == 0;
}


MinHeapNode* extractMin(MinHeap* minHeap) {
    if (isEmpty(minHeap))
        return NULL;

    MinHeapNode* root = minHeap->array[0];
    MinHeapNode* lastNode = minHeap->array[minHeap->size - 1];
    minHeap->array[0] = lastNode;

    minHeap->pos[root->vertex] = minHeap->size - 1;
    minHeap->pos[lastNode->vertex] = 0;

    --minHeap->size;
    minHeapify(minHeap, 0);

    return root;
}


void decreaseKey(MinHeap* minHeap, int v, int key) {
    int i = minHeap->pos[v];
    minHeap->array[i]->key = key;

    while (i && minHeap->array[i]->key < minHeap->array[(i - 1) / 2]->key) {
        minHeap->pos[minHeap->array[i]->vertex] = (i-1)/2;
        minHeap->pos[minHeap->array[(i-1)/2]->vertex] = i;
        swapMinHeapNode(&minHeap->array[i], &minHeap->array[(i - 1) / 2]);

        i = (i - 1) / 2;
    }
}


int isInMinHeap(MinHeap *minHeap, int v) {
    if (minHeap->pos[v] < minHeap->size)
        return 1;
    return 0;
}


void PrimMST(AdjList* graph, int n) {
    int parent[MAXN];
    int key[MAXN];

    MinHeap* minHeap = createMinHeap(n);

    for (int v = 1; v < n; ++v) {
        parent[v] = -1;
        key[v] = INT_MAX;
        minHeap->array[v] = newMinHeapNode(v, key[v]);
        minHeap->pos[v] = v;
    }

    key[0] = 0;
    minHeap->array[0] = newMinHeapNode(0, key[0]);
    minHeap->pos[0] = 0;
    minHeap->size = n;

    while (!isEmpty(minHeap)) {
        MinHeapNode* minHeapNode = extractMin(minHeap);
        int u = minHeapNode->vertex;

        Edge* pCrawl = graph[u].head;
        while (pCrawl != NULL) {
            int v = pCrawl->dest;

            if (isInMinHeap(minHeap, v) && pCrawl->cost < key[v]) {
                key[v] = pCrawl->cost;
                parent[v] = u;
                decreaseKey(minHeap, v, key[v]);
            }
            pCrawl = pCrawl->next;
        }
    }

    int totalCost = 0;
    for (int i = 1; i < n; ++i)
        totalCost += key[i];

    FILE *outputFile = fopen("output.txt", "w");
    if (outputFile != NULL) {
        fprintf(outputFile, "%d\n", totalCost);
        fclose(outputFile);
    }
}


int sfortuna(FILE *in_file, FILE *out_file) {
    int n, m;
    fscanf(in_file, "%d %d", &n, &m);

    AdjList *graph = (AdjList *) malloc(N * sizeof(AdjList));
    for (int i = 0; i < N; i++) 
        graph[i].head = NULL;
    
    for (int i = 0; i < M; ++i) {
        int u, v, w;
        fscanf(inputFile, "%d %d %d", &u, &v, &w);

        Edge* newNode = newEdge(v, w);
        newNode->next = graph[u].head;
        graph[u].head = newNode;

        newNode = newEdge(u, w);
        newNode->next = graph[v].head;
        graph[v].head = newNode;
    }

    PrimMST(graph, N);

        for (int i = 0; i < N; ++i) {
        Edge* pCrawl = graph[i].head;
        while (pCrawl != NULL) {
            Edge* temp = pCrawl;
            pCrawl = pCrawl->next;
            free(temp);
        }
    }

    free(graph);
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