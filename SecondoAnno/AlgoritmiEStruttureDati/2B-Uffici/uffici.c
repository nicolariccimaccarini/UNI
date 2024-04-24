#include <stdio.h>
#include <stdlib.h>

void uffici(FILE* in_file, FILE* out_file) {
    int M;
    fscanf(in_file, "%d", &M);   // Leggo il numero dei piani

    int* ufficiInVendita = (int*) calloc(M, sizeof(int));

    // leggo per ogni piano il numero di appartamenti in vendita
    for (int i=0; i<M; i++) {
        fscanf(in_file, "%d", &ufficiInVendita[i]);
    }


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