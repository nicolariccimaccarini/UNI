#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct
{
    long long x;
    long long y;
    long long z;
} coords;

/*
funzione che preso ogni set di coordinate ne calcola la distanza dal centro tramite questa formula: sqrt(x^2 + y^2 + z^2)
se la distanza e' minore del raggio incrementa un contatore e a fine di ogni raggio 
stampa su out_file il numero di set di coordinate che rientra nel raggio
*/
void checkRadius(coords coordinates[], unsigned int radius[], int N, int Q, FILE *out_file) {
    for (int i=0; i<Q; i++) {
        int insideCount = 0;

        for (int j=0; j<N; j++) {
            
            double distance = sqrt(pow(coordinates[j].x, 2) + pow(coordinates[j].y, 2) + pow(coordinates[j].z, 2));

            if (distance <= radius[i]) {
                insideCount++;
            }
        }
        // stampo insideCount in output.txt
        fprintf(out_file, "%d\n", insideCount);
    }
}

void pianetaSpritz(FILE* in_file, FILE *out_file) {
    int N, Q;   
    fscanf(in_file, "%d %d", &N, &Q);

    /* alloco dinamicamente la memoria per l'array di struct 'coords' 
    e l'array di interi dove andranno memorizzati i Q raggi */
    coords* coordinates = (coords*) malloc(N * sizeof(coords));
    unsigned int* radius = (unsigned int*) malloc(Q * sizeof(unsigned int));

    // leggo e salvo le coordinate
    for (int i=0; i<N; i++) {
        long long x, y, z;
        fscanf(in_file, "%lld %lld %lld", &x, &y, &z);

        coordinates[i].x = x;
        coordinates[i].y = y;
        coordinates[i].z = z;
    }

    // leggo e salvo le Q righe rimanenti conenenti i raggi 
    for (int i=0; i<Q; i++) {
        // i = raggio letto
        fscanf(in_file, "%u", &radius[i]);
    }
    
    checkRadius(coordinates, radius, N, Q, out_file);

    free(coordinates);
    free(radius);
}

int main(void) {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    pianetaSpritz(in_file, out_file);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}