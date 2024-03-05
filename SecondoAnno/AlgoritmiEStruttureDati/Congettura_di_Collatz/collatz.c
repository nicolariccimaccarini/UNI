#include <stdio.h>

int main(void) {
    FILE* input = fopen("input.txt", "r");
    FILE* output = fopen("output.txt", "w");

    if (input == NULL || output == NULL) {
        fprintf(stderr, "Errore apertura input.txt o output.txt\n");
        return -1;
    }
    
    int N, ans = 1;
    fscanf(input, "%d", &N);
    
    while (N != 1) {
  
        if (N % 2 == 0) {
            N /= 2;
        }
        else {
            N = (N * 3) + 1;
        }

        ans++;
    }

    fprintf(output, "%d", ans);

    if (fclose(input) != 0 || fclose(output) != 0) {
        fprintf(stderr, "Errore chiusura input.txt o output.txt\n");
        return -2;
    }
}