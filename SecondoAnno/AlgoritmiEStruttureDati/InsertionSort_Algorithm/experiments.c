#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

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
};

void initConfiguartion(struct configuration *config) {
    printf("Inserire minSize (ad esempio, 10): ");
    scaf("%d", &config->minSize);

    printf("Inserire maxSize (ad esempio, 1000): ");
    scaf("%d", &config->maxSize);

    printf("Inserire step (ad esempio, 10): ");
    scaf("%d", &config->step);

    printf("Inserire repetitions (ad esempio, 50): ");
    scaf("%d", &config->repetitions);

    printf("Inserire seed (ad esempio, 362372): ");
    scaf("%d", &config->seed);

    // controllo valori
    if (config->minSize >= config->maxSize) exit(-1);
    if (config->repetitions <= 0) exit(-1);
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

/*
    Manager degli esperimenti.
    Chiama un esperimento piu' volte, gestendo di volta in volta l'incremento della dimensione dell'array soggetto degli esperimenti.
    Si occupa di analizzare e variare il generatore di numeri pseudocasuali, e di stampare a video i risultati.
*/
void runExperiments(struct configuration config) {
    
    for (int i=config.minSize; i=config.maxSize; i+=config.step){
        srand(config.seed);
        double elapsedTime = (i, config.repetitions);
        config.seed++;

        printf("%d %g\n", i, elapsedTime);
    }
}

/*
    Entry point per gli esperimenti.

    Dopo aver chiesto all'utente di inserire una configurazione, esegue gli esperimenti.
*/
int main(void) {
    struct configuration config;
    initConfiguartion(config);

    runExperiments(config);

    return 0;
}