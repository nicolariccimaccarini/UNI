#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

// Struttura per rappresentare una stack
struct Stack {
    int top;
    unsigned capacity;
    int* array;
};

struct Stack* createStack(unsigned capacity) {
    struct Stack* stack = (struct Stack*) malloc(sizeof(struct Stack));
    stack->capacity = capacity;
    stack->top = -1;
    stack->array = (int*) malloc(stack->capacity * sizeof(int));
    return stack;
}

// Stack è piena quando la cima è uguale alla capacità massima
int isFull(struct Stack* stack) {
    return stack->top == stack->capacity - 1;
}

// Stack è vuota quando la cima è uguale a -1
int isEmpty(struct Stack* stack) {
    return stack->top == -1;
}

// Funzione per aggiungere un elemento alla stack. Aumenta la cima di 1
void push(struct Stack* stack, int item) {
    if (isFull(stack)) return;
    stack->array[++stack->top] = item;
}

// Funzione per rimuovere un elemento dalla stack. Diminuisce la cima di 1
int pop(struct Stack* stack) {
    if (isEmpty(stack)) return INT_MIN;
    return stack->array[stack->top--];
}

// Funzione per restituire l'elemento in cima senza rimuoverlo
int peek(struct Stack* stack) {
    if (isEmpty(stack)) return INT_MIN;
    return stack->array[stack->top];
}

// Funzione principale per ottenere l'area massima
int getMaxArea(int M, int* ufficiInVendita) {
    struct Stack* stack = createStack(M);
    int max_area = 0;   // Inizializza l'area massima
    int tp;             // Per memorizzare l'indice in cima
    int area_with_top;  // Per memorizzare l'area con la barra in cima

    int i = 0;
    while (i < M) {
        if (isEmpty(stack) || ufficiInVendita[peek(stack)] <= ufficiInVendita[i]) {
            push(stack, i++);
        } else {
            tp = pop(stack);  // Memorizza l'indice in cima

            // Calcola l'area con ufficiInVendita[tp] come altezza
            area_with_top = ufficiInVendita[tp] * (isEmpty(stack) ? i : i - peek(stack) - 1);

            // Aggiorna l'area massima, se necessario
            if (max_area < area_with_top) max_area = area_with_top;
        }
    }

    /*  
        Ora pop gli elementi rimanenti e calcola l'area con ogni
        elemento in cima come altezza. 
    */
    while (isEmpty(stack) == 0) {
        tp = pop(stack);
        area_with_top = ufficiInVendita[tp] * (isEmpty(stack) ? i : i - peek(stack) - 1);
        if (max_area < area_with_top) max_area = area_with_top;
    }

    return max_area;
}

void uffici(FILE* in_file, FILE* out_file) {
    int M, maxArea;              // M = numero di piani --- maxArea = soluzione esercizio
    fscanf(in_file, "%d", &M);   // Leggo il numero dei piani

    int* ufficiInVendita = (int*) calloc(M, sizeof(int));

    // leggo per ogni piano il numero di appartamenti in vendita
    for (int i=0; i<M; i++) {
        fscanf(in_file, "%d", &ufficiInVendita[i]);
    }

    maxArea = getMaxArea(M, ufficiInVendita);

    fprintf(out_file, "%d\n", maxArea);

    free(ufficiInVendita);
}

int main(void) {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    uffici(in_file, out_file);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}