#include <stdio.h>
#include <stdlib.h>


void sfortuna(FILE *in_file, FILE *out_file) {
    
}


int main(void) {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        fprintf(stderr, "Errore nell'apertura del file di input.\n");
        return EXIT_FAILURE;
    }

    sfortuna(in_file, out_file);

    // Chiudo i file di input e output
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        fprintf(stderr, "Errore nella chiusura del file di input/output.\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}