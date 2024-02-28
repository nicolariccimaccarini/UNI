#include <stdio.h>
#include <stdlib.h>

typedef struct 
{
    int num;
    int den;
} Frazione;

int valore_assoluto(int n)
{
    return n>=0 ? n: -n;
}

int MCD(int a, int b)
{
    while(a!=b)
    {
        if (a>b)
        {
            a=a-b;
        }

        else
        {
            b=b-a;
        }
    }

    return a;
}

Frazione semplifica(Frazione f)
{
    Frazione fs;

    if (f.num==0)
    {
        fs.den=1;
    }

    fs.num = f.num/MCD(valore_assoluto(f.num), f.den);
    fs.den = f.den/MCD(valore_assoluto(f.num), f.den);

    return fs;
}

Frazione frazione(int n, int d)
{
    Frazione f;
    
    if (d==0)
    {
        printf ("Errore: divisione per 0\n");
        exit(1);
    }

    if (d<0)
    {
        n=-n;
        d=-d;
    }

    f.num=n;
    f.den=d;
    
    return semplifica(f);
}

void stampa_frazionaria(Frazione f)
{
    printf ("%d/%d\n", f.num, f.den);
}

void stampa_decimale(Frazione f)
{
    printf ("%f\n", (float)f.num/(float)f.den);
}

Frazione leggi()
{
    int n, d;
    
    Frazione f;
    scanf ("%d%d", &n, &d);
    f.num=n;
    f.den=d;
    
    return f;
}

Frazione somma(Frazione f1, Frazione f2)
{
    return frazione(((f1.num*f2.den)+(f2.num*f1.den)), (f1.den*f2.den));
}

Frazione sottrazione(Frazione f1, Frazione f2)
{
    return frazione(((f1.num*f2.den)-(f2.num*f1.den)), (f1.den*f2.den));
}

Frazione moltiplicazione(Frazione f1, Frazione f2)
{
    return frazione((f1.num*f2.num), (f1.den*f2.den));    
}

Frazione divisione(Frazione f1, Frazione f2)
{
    return frazione((f1.num*f2.den), (f1.den*f2.num));  
}

int main()
{
    Frazione a, b, c ;

    a=semplifica(leggi());
    b=leggi();
    c=somma(a, b);

    stampa_frazionaria(c);
    c=sottrazione(a, b);

    stampa_frazionaria(c);
    c=moltiplicazione(a, b);

    stampa_frazionaria(c);
    c=divisione(a, b);

    stampa_frazionaria(c);
    
    return 0;
}