#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

struct configuration
{
    int minSize, maxSize;
    int step, repetitions, seed;
};

struct configuration initConfiguration() {
    struct configuration config;

    config.minSize = 10;
    config.maxSize = 1000;
    config.step = 10;
    config.repetitions = 50;
    config.seed = 362372;

    return config;
}

void insertionSort(int *arr, int start, int end) {
    int key, i;
    
    for (int j=start; j<end; j++) {
        key = arr[j];
        i = j-1;

        while (i >= start && arr[i] > key) {
            arr[i+1] = arr[i];
            i--;
        }
        arr[i+1] = key;
    }
}

int check(int *arr, int size) {
    for (int i=1; i<size; i++) {
        if (arr[i] < arr[i-1]) {
            return -1;
        }
    }
}

int main(void) {
    struct configuration config = initConfiguration();

    runExperiments(config);

    return 0;
}

double run(int size, int repetitions){
    double elapsedTime = 0.0;
    for (int i=0; i<repetitions; i++) {
        int arr[size];
        for (int j=0; j<size; j++) {
            arr[j] = rand();
        }

        clock_t start, end;
        start = clock();
        insertionSort(arr, start, end);     
        end = clock();

        if (check(arr, size) != 1) {
            printf("ERRORE: l'ordinamento e' scorretto.\n");
            exit(-1);
        }

        elapsedTime += (end-start) / (double) CLOCKS_PER_SEC;
    }

    return elapsedTime / repetitions;
}

void runExperiments(struct configuration config) {
    
    for (int i=config.minSize; i=config.maxSize; i+=config.step){
        srand(config.seed);
        double elapsedTime = (i, config.repetitions);

        printf("%d %g\n", i, elapsedTime);
    }
}