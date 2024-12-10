#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
    char marca[20];
    int cilindrata;
    int anno_di_immatricolazione;
    char nome_acquirente[30];
    char cognome_acquirente[30];
} automobili;

automobili inserimento_dati() //analoga alla funzione leggi()
{
    char m[20]; //marca
    int c; //cilindrata
    int adi; //annno di immatricolazione
    char na[30]; //nome acquirente
    char ca[30]; //cognome acquirente

    automobili ins;
    printf ("Inserisci i dati dell'auto: Marca, Cilindrata, Anno di immatricolazione, Nome acquirente e Cognome acquirente:\n");
    scanf ("%s %d %d %s %s", m, &c, &adi, na, ca);

    ins.marca[20]=m;
    ins.cilindrata=c;
    ins.anno_di_immatricolazione=adi;
    ins.nome_acquirente[30]=na;
    ins.cognome_acquirente[30]=ca;

    if ((ins.cilindrata<800 && ins.cilindrata>2500) && (ins.anno_di_immatricolazione<2000 && ins.anno_di_immatricolazione>2019))
    {
        printf ("Errore: non è possibile vendere l'auto con con questa cilindrata e/o anno di immatricolazione.\n");
        exit(1);
    }

    return ins;
}

automobili ricerca(automobili search)   
{
    automobili s;
    
    char marca2[20];
    printf ("Inserisci la marca da cercare:\n");
    scanf ("%s", marca2);

    if (marca2==search.marca)
    {
        //mostra tutte le macchine in vendita con quella marca
    }

    else
    {
        printf ("Nessuna macchina in vendita, selezionare un altra marca:\n");
        scanf ("%s", marca2);
        //..........
    }

    return s;
}

automobili visualizza_cognome(automobili search)
{
    automobili cogn;

    while (1)
    {
        if (cogn.cilindrata>>1800)
        {
            printf ("%s", cogn.cognome_acquirente);
        }
    }

    return cogn;
}

automobili visualizza_auto_anno(automobili search)
{
    automobili anno;

    printf ("Inserisci ann0:\n");
    scanf ("%d", &anno.anno_di_immatricolazione);

    if (anno.anno_di_immatricolazione==search.anno_di_immatricolazione)
    {
        printf ("...."); 
    }
}

automobili tabella_autosalone (automobili tab) //crea una tabella con dentro i dati delle macchine vendute e dei venditori
{

}

int main(void)
{
    
    
    return 0;
}