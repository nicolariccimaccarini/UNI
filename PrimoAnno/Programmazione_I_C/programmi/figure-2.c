#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define PI 3.1415926535

typedef struct 
{
    float lato;             //struct quadrato
} DatiQuadrato;

typedef struct 
{
    float base;             //struct rettangolo 
    float altezza;
} DatiRettangolo;

typedef struct 
{
    float lato1;
    float lato2;            //struct triangolo
    float lato3;
} DatiTriangolo;

typedef struct 
{
    float raggio;           //struct cerchio
} DatiCerchio;

typedef struct 
{
    enum
    {
        Quadrato, Rettangolo, Triangolo, Cerchio
    } tipo_figura;

    union 
    {
        DatiQuadrato datiQuadrato;
        DatiRettangolo datiRettangolo;
        DatiTriangolo datiTriangolo;
        DatiCerchio datiCerchio;
    } dati_figura;
} Figura;

void quadrato(Figura *pf, float l)
{
    pf->tipo_figura=Quadrato;
    pf->dati_figura.datiQuadrato.lato=l;
}

void rettangolo(Figura *pf, float b, float h)
{
    pf->tipo_figura=Rettangolo;
    pf->dati_figura.datiRettangolo.base=b;
    pf->dati_figura.datiRettangolo.altezza=h;
}

void triangolo(Figura *pf, float a, float b, float c)
{
    pf->tipo_figura=Triangolo;
    pf->dati_figura.datiTriangolo.lato1=a;
    pf->dati_figura.datiTriangolo.lato2=b;
    pf->dati_figura.datiTriangolo.lato3=c;
}

void cerchio(Figura *pf, float r)
{
    pf->tipo_figura=Cerchio;
    pf->dati_figura.datiCerchio.raggio=r;
}

float perimetro(Figura *pf)
{
    switch (pf->tipo_figura)
    {
        case Quadrato:
            return pf->dati_figura.datiQuadrato.lato * 4;

        case Rettangolo:
            return (pf->dati_figura.datiRettangolo.base + pf->dati_figura.datiRettangolo.altezza) * 2;
        
        case Triangolo: 
            return (pf->dati_figura.datiTriangolo.lato1 + pf->dati_figura.datiTriangolo.lato2 + pf->dati_figura.datiTriangolo.lato3);

        case Cerchio:
            return (pf->dati_figura.datiCerchio.raggio * 2 * PI);
    }
}

float area(Figura *pf)
{
    switch (pf->tipo_figura)
    {
        case Quadrato:
            return pf->dati_figura.datiQuadrato.lato * pf->dati_figura.datiQuadrato.lato;
        
        case Rettangolo:
            return pf->dati_figura.datiRettangolo.base * pf->dati_figura.datiRettangolo.altezza;

        case Triangolo:
            return sqrt(((pf->dati_figura.datiTriangolo.lato1 + pf->dati_figura.datiTriangolo.lato2 + pf->dati_figura.datiTriangolo.lato3)/2) * 
            (((pf->dati_figura.datiTriangolo.lato1 + pf->dati_figura.datiTriangolo.lato2 + pf->dati_figura.datiTriangolo.lato3)/2) - pf->dati_figura.datiTriangolo.lato1)) * 
            (((pf->dati_figura.datiTriangolo.lato1 + pf->dati_figura.datiTriangolo.lato2 + pf->dati_figura.datiTriangolo.lato3)/2) - pf->dati_figura.datiTriangolo.lato2) * 
            (((pf->dati_figura.datiTriangolo.lato1 + pf->dati_figura.datiTriangolo.lato2 + pf->dati_figura.datiTriangolo.lato3)/2) - pf->dati_figura.datiTriangolo.lato3);

        case Cerchio:
            return (pf->dati_figura.datiCerchio.raggio * pf->dati_figura.datiCerchio.raggio) * PI;
    }
}

int main()
{
    Figura f;

    float lato; //elementi di input per il quadrato;
    float base, altezza; //elementi di input per il rettangolo
    float lato1, lato2, lato3; //elementi di input per il triangolo;
    float raggio; //elementi di input per il cerchio

    char a[20];
    int Q, q, R, r, T, t, C, c;

    printf ("Quadrato, Rettangolo, Triangolo o Cerchio?\n");
    scanf ("%s", a);

    Q=strcmp(a, "Quadrato");
    q=strcmp(a, "quadrato");        //confronto le stringhe
    R=strcmp(a, "Rettangolo");
    r=strcmp(a, "rettangolo");
    T=strcmp(a, "Triangolo");
    t=strcmp(a, "triangolo");
    C=strcmp(a, "Cerchio");
    c=strcmp(a, "cerchio");
    
    if (Q==0 || q==0)
    {
        printf ("Inserisci il lato:\n");
        scanf ("%f", &lato);

        quadrato(&f, lato);
        printf ("Perimetro quadrato: %f\n", perimetro(&f));
        printf ("Area quadrato: %f\n", area(&f));
    }

    else if (R==0 || r==0) 
    {
        printf ("Inserisci base e altezza:\n");
        scanf ("%f%f", &base, &altezza);

        rettangolo(&f, base, altezza);
        printf ("Perimetro rettangolo: %f\n", perimetro(&f));
        printf ("Area rettangolo: %f\n", area(&f));
    }

    else if (T==0 || t==0)
    {
        printf ("Inserisci i tre lati:\n");
        scanf ("%f%f%f", &lato1, &lato2, &lato3);

        triangolo(&f, lato1, lato2, lato3);
        printf ("Perimetro triangolo: %f\n", perimetro(&f));
        printf ("Area triangolo: %f\n", area(&f));
    }

    else if (C==0 || c==0)
    {
        printf ("Inserisci il raggio:\n");
        scanf ("%f", &raggio);

        cerchio(&f, raggio);
        printf ("Perimetro cerchio: %f\n", perimetro(&f));
        printf ("Area cerchio: %f\n", area(&f));
    }

    else
    {
        printf ("Errore!\n");
        exit(1);
    }

    return 0;
}