#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void merge(double arr[], int left, int mid, int right) {
    int i, j, k;
    int n1 = mid - left + 1;
    int n2 = right - mid;

    double L[n1], R[n2];

    for (i = 0; i < n1; i++)
        L[i] = arr[left + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[mid + 1 + j];

    i = 0; 
    j = 0; 
    k = left; 
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

void mergeSort(double arr[], int left, int right) {
    if (left < right) {
        int mid = left + (right - left) / 2;

        mergeSort(arr, left, mid);
        mergeSort(arr, mid + 1, right);

        merge(arr, left, mid, right);
    }
}

void pianetaSpritz(FILE* in_file, FILE *out_file) {
    int N, Q;   
    fscanf(in_file, "%d %d", &N, &Q);

    /* alloco dinamicamente la memoria per l'array di struct 'coords' 
    e l'array di interi dove andranno memorizzati i Q raggi */
    unsigned int radius;
    double* distance = (double*) malloc(N * sizeof(double));
    int insideCount = 0;

    // leggo e salvo le coordinate
    for (int i=0; i<N; i++) {
        long long x, y, z;
        fscanf(in_file, "%lld %lld %lld", &x, &y, &z);

        distance[i] = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2));
    }

    // Call mergeSort function
    mergeSort(distance, 0, N - 1);

    // leggo e salvo le Q righe rimanenti conenenti i raggi 
    for (int i=0; i<Q; i++) {
        // i = raggio letto
        insideCount = 0;
        fscanf(in_file, "%u", &radius);

        for (int j=0; i<N; j++) {
            if (distance[j] <= radius) {
                insideCount++;
            } else {
                break;
            }
        }

        fprintf(out_file, "%d\n", insideCount);
    }

    free(distance);
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