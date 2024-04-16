#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct {
    int x, y, z;
} Bar;

// Funzione per calcolare la distanza dal centro
double distanzaDalCentro(Bar bar) {
    return sqrt(bar.x * bar.x + bar.y * bar.y + bar.z * bar.z);
}

// Funzione di confronto per qsort
int confrontoBar(const void *a, const void *b) {
    Bar *barA = (Bar *)a;
    Bar *barB = (Bar *)b;
    double distanzaA = distanzaDalCentro(*barA);
    double distanzaB = distanzaDalCentro(*barB);
    return (distanzaA > distanzaB) - (distanzaA < distanzaB);
}

int main() {
    FILE *inputFile = fopen("input.txt", "r");
    FILE *outputFile = fopen("output.txt", "w");
    if (inputFile == NULL || outputFile == NULL) {
        perror("Errore nell'apertura dei file");
        return 1;
    }

    int N, Q;
    fscanf(inputFile, "%d %d", &N, &Q);
    Bar *bar = malloc(N * sizeof(Bar));

    // Leggi le coordinate dei bar-asteroidi
    for (int i = 0; i < N; i++) {
        fscanf(inputFile, "%d %d %d", &bar[i].x, &bar[i].y, &bar[i].z);
    }

    // Ordina i bar-asteroidi per distanza dal centro
    qsort(bar, N, sizeof(Bar), confrontoBar);

    // Rispondi alle query
    for (int i = 0; i < Q; i++) {
        int r, conteggio = 0;
        fscanf(inputFile, "%d", &r);
        for (int j = 0; j < N; j++) {
            if (distanzaDalCentro(bar[j]) <= r) {
                conteggio++;
            } else {
                break; // I bar sono ordinati, quindi possiamo interrompere il ciclo
            }
        }
        fprintf(outputFile, "%d\n", conteggio);
    }

    free(bar);
    fclose(inputFile);
    fclose(outputFile);
    return 0;
}