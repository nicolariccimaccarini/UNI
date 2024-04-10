#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXN 100
#define MAXQ 10000

typedef struct
{
    int x;
    int y;
    int z;
} coords;

/*
funzione che preso ogni set di coordinate ne calcola la distanza dal centro tramite questa formula: sqrt(x^2 + y^2 + z^2)
se la distanza e' minore del raggio incrementa un contatore e a fine di ogni raggio 
stampa su out_file il numero di set di coordinate che rientra nel raggio
*/
void checkRadius(coords coordinates[], int radius[], int N, int Q, int dimension, FILE *out_file) {
    for (int i=0; i<Q; i++) {
        int insideCount = 0;

        for (int j=0; j<N; j++) {
            /*
            double distance = sqrt(pow(coordinates[j].x, 2) + pow(coordinates[j].y, 2) + pow(coordinates[j].z, 2));
            */

            double distance = 0;

            // calcolo la distanza in base alla dimensione
            if (dimension == 1) {
                distance = abs(coordinates[j].x);
            } else if (dimension == 2) {
                distance = sqrt(pow(coordinates[j].x, 2) + pow(coordinates[j].y, 2));
            } else if (dimension == 3) {
                distance = sqrt(pow(coordinates[j].x, 2) + pow(coordinates[j].y, 2) + pow(coordinates[j].z, 2));
            }

            if (distance <= radius[i]) {
                insideCount++;
            }
        }
        // stampo insideCount in output.txt
        fprintf(out_file, "%d\n", insideCount);
    }
}

void pianetaSpritz(FILE* in_file, FILE *out_file, int dimension) {
    int N, Q;   
    fscanf(in_file, "%d %d", &N, &Q);

    // controllo che N e Q rispettino i vincoli
    if (N < 1 || N > 102 || Q < 1 || Q > 10000){
        fprintf(stderr, "N e Q non rispettano i vincoli imposti");
        return -1;
    }

    /* alloco dinamicamente la memoria per l'array di struct 'coords' 
    e l'array di interi dove andranno memorizzati i Q raggi */
    coords* coordinates = (coords*) malloc(N * sizeof(coords));
    int* radius = (int*) malloc(Q * sizeof(int));

    // leggo e salvo le coordinate
    for (int i=0; i<N; i++) {
        int x, y = 0, z = 0;
        fscanf(in_file, "%d %d %d", &x, &y, &z);

        coordinates[i].x = x;
        coordinates[i].y = y;
        coordinates[i].z = z;

        if ((x < pow(-2, 30) || x > pow(2, 30)) && (y < pow(-2, 30) || y > pow(2, 30)) && (z < pow(-2, 30) || z > pow(2, 30))) {
            fprintf(stderr, "Errorre vincoli coordinate");
            return -1;
        }
    }

    // leggo e salvo le Q righe rimanenti conenenti i raggi 
    for (int i=0; i<Q; i++) {
        // i = raggio letto
        fscanf(in_file, "%d", &radius[i]);
        if (radius[i] < 0 || radius[i] > pow(2, 31)) {
            fprintf(stderr, "Errori vincoli sul raggio");
            return -1;
        }
    }
    
    // checkConstraints();
    checkRadius(coordinates, radius, N, Q, dimension, out_file);

    free(coordinates);
    free(radius);
}

int main(void) {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");
    int dimension = 3;

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    pianetaSpritz(in_file, out_file, dimension);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}