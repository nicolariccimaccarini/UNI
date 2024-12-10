#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define DIM 10

int main(void) {
    
    int i;

    srand(time(NULL));

    for(i=0; i<DIM; i++)
    {
        printf("%d", rand() % 6 + 1);
    }

    printf ("\n");
    return 0;
}