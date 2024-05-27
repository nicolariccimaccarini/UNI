#include <stdio.h>
#include <stdlib.h>


void sfortuna(FILE *in_file, FILE *out_file) {
    int N, M;   // N = numero citta' --- M = proposte di strada

    fscanf(in_file, "%d %d", &N, &M);
}


int main(void) {
        // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    sfortuna(in_file, out_file);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}