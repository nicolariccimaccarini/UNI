#include <stdio.h>
#include <string.h>
#define dim 100

int main()
{
    char s[dim], s2[dim];
    int i;
    int l1, l2;

    printf ("Inserisci una parola:\n");
    scanf ("%s", s);

    l1=strlen(s);

    for (i=0; i<dim; i++)
    {
        switch (s[i]=='a'||'e'||'i'||'o'||'u')
        {
        case 0:
            s[i]='afa';
        
        case 1:
            s[i]='efe';

        case 2: 
            s[i]='ifi';

        case 3:
            s[i]='ofo';

        case 4:
            s[i]='ufu';
        
        default:
            break;
        }
    }
    
    printf ("%s\n", s);
    
    return 0;
}