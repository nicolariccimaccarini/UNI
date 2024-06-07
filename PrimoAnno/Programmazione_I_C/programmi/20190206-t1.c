#include <stdio.h>

int f(int d);

int main(int argc, char* argv[])
{
    char s[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    
    int i=1;

    while (i<7)
    {
        printf ("%c", *((char*) ((int*) s + i)));

        i += f ? f(i) : 1;
    } 
    pintf ("\n");
    return 0;
}