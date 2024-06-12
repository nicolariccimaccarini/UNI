#include <stdio.h>
#include <stdlib.h>


struct Node {
    int representative;
    int size;
};


struct Edge {
    int from;
    int to;
    int weight;
};


struct Cart {
    int value;
    int original_idx;
};


int weightsComparator(const void *x, const void *y) {
    const struct Edge *a = x;
    const struct Edge *b = y;
    return a->weight < b->weight;
}


int cartsComparator(const void *x, const void *y) {
    const struct Cart *a = x;
    const struct Cart *b = y;
    return a->value < b->value;
}


int find(int n, struct Node *nodes) {
    while (n != nodes[n].representative) {
        n = nodes[n].representative;
    }
    
    return n;
}


int same(int a, int b, struct Node *nodes) {
    return find(a, nodes) == find(b, nodes);
}


void merge(int a, int b, struct Node *nodes) {
    a = find(a, nodes);
    b = find(b, nodes);

    if (nodes[a].size < nodes[b].size) {
        int temp = a;
        a = b;
        b = temp;
    }

    nodes[a].size += nodes[b].size;
    nodes[b].representative = a;
}


void kruskal(struct Node *nodes, struct Edge *edges, int nEdges, int currentWeight, int *lastIndex, int *lastAnswer) {
    while (*lastIndex < nEdges)
    {
        if (currentWeight > edges[*lastIndex].weight) {
            return;
        }

        int fromNode = edges[*lastIndex].from;
        int toNode = edges[*lastIndex].to;

        if (!same(fromNode, toNode, nodes)) {
            merge(fromNode, toNode, nodes);
            (*lastAnswer--);
        }

        (*lastIndex)++;
    }
}


void castori(FILE *in_file, FILE *out_file) {
    int N, K, T;
    fscanf(in_file, "%d %d %d", &N, &K, &T);
    
    struct Node *nodes = malloc(N * sizeof(struct Node));
    struct Edge *edges = malloc(K * sizeof(struct Edge));
    struct Cart *carts = malloc(T * sizeof(struct Cart));
    int *answers = malloc(T * sizeof(int));

    for (int i=0; i<K; i++) {
        fscanf(in_file, "%d %d %d", &edges[i].from, &edges[i].to, &edges[i].weight);
    }
    qsort(edges, K, sizeof(struct Edge), weightsComparator);

    for (int i=0; i<T; i++) {
        fscanf(in_file, "%d", &carts[i].value);
        carts[i].original_idx = i;
    }
    qsort(carts, T, sizeof(struct Carts), cartsComparator);

    int lastIndex = 0;
    int lastAnswer = N-1;
    for (int i=0; i<T; i++) {
        kruskal(nodes, edges, K, carts[i].value, &lastIndex, &lastAnswer);
        answers[carts[i].original_idx] = lastAnswer;
    }

    for (int i=0; i<T; i++) {
        fprintf(out_file, "%d", answers[i]);
    }
}


int main(void) {
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        fprintf(stderr, "Errore apertura file di input/output\n");
        EXIT_FAILURE;
    }

    castori(in_file, out_file);

    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        fprintf(stderr, "Errore chiusura file di input/output\n");
        EXIT_FAILURE;
    }

    return 0;
}