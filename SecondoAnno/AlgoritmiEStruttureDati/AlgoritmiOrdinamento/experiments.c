#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#define MAX_ALGO 16
#define MAX_ALGO_BUFF 64

/*
    Parametri di un esperimento.

    minSize: dimensione dell'array nel primo esperimento.
    maxSize: dimensione dell'array nell'ultimo esperimento.
    step: incremento di dimensione dell'array tra un esperimento e il successivo.
    repetitions: numero di volte in cui un esperimento e' ripetuto, al fine di ottenere tempi statisticamente validi.
    see: seme del generatore di numeri pseudocasuali, al fine di garantire la ripsoducibilita' degli esperimenti.
*/

struct configuration
{
    int minSize, maxSize, step, repetitions, seed;
    int nAlgorithms;
    char algorithms[MAX_ALGO][MAX_ALGO_BUFF];
};

void initConfiguartion(struct configuration *config) {
    printf("Inserire minSize (ad esempio, 10): ");
    scanf("%d", &config->minSize);

    printf("Inserire maxSize (ad esempio, 1000): ");
    scanf("%d", &config->maxSize);

    printf("Inserire step (ad esempio, 10): ");
    scanf("%d", &config->step);

    printf("Inserire repetitions (ad esempio, 50): ");
    scanf("%d", &config->repetitions);

    printf("Inserire seed (ad esempio, 362372): ");
    scanf("%d", &config->seed);

    // controllo valori
    if (config->minSize >= config->maxSize) exit(-1);
    if (config->repetitions <= 0) exit(-1);

    int nAlgorithms = 5;
    char *algos[] = {"INSERTIONSORT", "MERGE"};

    config->nAlgorithms = nAlgorithms;
    for (int i=0; i<nAlgorithms; i++) {
        strcpy(config->algorithms[i], algorithms[i]);
    }
}

// implementaziione di insertion sort
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

void merge(int *arr, int start, int middle, int end) {
    int n1 = middle - start + 1;
    int n2 = end - middle;

    int L[n1], R[n2];

    for (int i=0; i<n1; i++) {
        L[i] = arr[start+i];
    }
    for (int j=0; j<n2; j++) {
        R[j] = arr[middle+1+j];
    }

    int i=0, j=0, k=start;
    while (i<n1 && j<n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i<n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j<n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

// implementazione di merge sort
void mergeSort(int *arr, int start, int end) {
    if (start < end) {
        int middle = (start+end)/2;
        mergeSort(arr, start, middle);
        mergeSort(arr, middle+1, end);
        merge(arr, start, middle, end);
    }
}

typedef void(*algoPtr)(int*, int, int);

algoPtr selectAlgorithm(char *algoName) {
    if (strcmp(algoName, "INSERTIONSORT")) {
        return &insertionSort;
    } else if(strcmp(algoName, "MERGE")) {
        return &mergeSort;
    } else {
        printf("Erore - l'algoritmo %s non esiste", algoName);
        exit(-1);
    }
}

// funzione antagonista: controlla che un array sia ordinato
int check(int *arr, int size) {
    for (int i=1; i<size; i++) {
        if (arr[i] < arr[i-1]) {
            return -1;
        }
    }

    return 1;
}

/*
    Esegue un numero di misurazioni pari a quelle di 'repetitions'.
    Ogni misurazione riguarda l'applicazione di un certo algoritmo di ordinamento ad un array di dimensione size, 
    inizializzato con numeri pseudocasuali.
    Restituisce un valore 'double', contenente il tempo medio di esecuzione.
*/
double run(int size, int repetitions, algoPtr algorithm) {
    double elapsedTime = 0.0;

    for (int i=0; i<repetitions; i++) {
        int arr[size];
        for (int j=0; j<size; j++) {
            arr[j] = rand();
        }

        clock_t start, end;
        start = clock();
        algorithm(arr, start, end);
        end = clock();

        if (check(arr, size) != 1) {
            printf("ERRORE: l'ordinamento e' scorretto.\n");
            exit(-1);
        }

        elapsedTime += (end-start) / (double) CLOCKS_PER_SEC;
    }

    return elapsedTime / repetitions;
}

/*
    Manager degli esperimenti.
    Chiama un esperimento piu' volte, gestendo di volta in volta l'incremento della dimensione dell'array soggetto degli esperimenti.
    Si occupa di analizzare e variare il generatore di numeri pseudocasuali, e di stampare a video i risultati.
*/
void runExperiments(struct configuration config, FILE *report) {
    // stampo l'asse delle x
    for (int i=config.minSize; i<config.maxSize; i+=config.step) {
        fprintf(report, "%d", i);
    }
    fprintf(report, "\n");

    // per ogni algoritmo che ho caricato nella mia configurazione ...
    for (int algo_idx=0; algo_idx<config.nAlgorithms; algo_idx++) {

        // ... esegui un esperimento e stampa, passo dopo passo, l'asse y
        for (int i=config.minSize; i=config.maxSize; i+=config.step){
            srand(config.seed);
            double elapsedTime = run(i, config.repetitions);
            config.seed++;

            fprintf(report, "%g ", elapsedTime);
        }
        fprintf(report, "\n");

    }
}

/*
    Entry point per gli esperimenti.

    Dopo aver chiesto all'utente di inserire una configurazione, esegue gli esperimenti.
*/
int main(void) {
    struct configuration config;
    initConfiguartion(&config);

    FILE*report = fopen("report.txt", "w");
    if (report == NULL) {
        printf("Errore nell'apertura del file.\n");
        return -1;
    }

    runExperiments(config);

    if (fclose(report) != 0) {
        printf("Errore nella chiusura del file.\n");
        return -1;
    }

    return 0;
}