int main()
{
    char carattere;

    printf ("Inserisci un carattere:\n");
    scanf ("%c", &carattere);

    if (carattere>='A' && carattere<='Z')
    {
        printf ("Il carattere %c è una lettera maiuscola\n", carattere);
    }

    else if (carattere>='a' && carattere<='z')
    {
        printf ("Il carattere %c è una lettera minuscola", carattere);
    }

    else if (carattere>='0' && carattere<='9')
    {
        printf ("Il carattere %c è un numero\n", carattere);
    }

    else {printf ("Il carattere inserito è di un altro tipo\n");}
}