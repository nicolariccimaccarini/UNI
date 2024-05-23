#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


typedef struct {
    char type;              // 'C' per muro circolare, 'S' per muro dritto
    int r1, r2;             // raggio per muri dritti
    int theta1, theta2;     // angoli per muri circolari
    int theta;              // angolo per muri dritti
} Wall;


int labirinto(FILE *in_file, FILE *out_file) {
    int N;  // numero di labirinti contenuti nel file

}


int main(void) {
    // Apro i fiole di input e output
    FILE *in_file  = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    pugilato(in_file, out_file);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}