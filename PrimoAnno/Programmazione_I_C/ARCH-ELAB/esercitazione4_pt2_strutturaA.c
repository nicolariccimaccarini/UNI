#include <stdio.h>
#pragma pack(push, 1)

int main()
{
    //structure A

    typedef struct 
    {
        char c;
        short int s;
    } structa_t;

    structa_t istanza = {'c', 4};
    printf ("%lu", sizeof(istanza));
    return 0;
}