#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    int pezzi_disponibili[5] = {20, 20, 20, 20, 20}; // quantità disponibili di ogni pasticciotto
    float prezzi[5] = {1.50, 1.75, 2.00, 1.80, 2.50}; // prezzo di ogni pasticciotto
    char pasticciotti[5][20] = {"Pasticciotto classico", "Pasticciotto alla crema", "Pasticciotto alla Nutella", "Pasticciotto alla frutta", "Pasticciotto alla mandorla"}; // nome di ogni pasticciotto
    int scelta, quantita, acquistati = 0;
    float totale = 0.0;
    char risposta;

    printf("Benvenuto nella pasticceria dei pasticciotti leccesi!\n");
    printf("Ecco l'elenco dei nostri pasticciotti:\n");
    printf("1. Pasticciotto classico\n");
    printf("2. Pasticciotto alla crema\n");
    printf("3. Pasticciotto alla Nutella\n");
    printf("4. Pasticciotto alla frutta\n");
    printf("5. Pasticciotto alla mandorla\n");
    printf ("\n");

    do {
        printf("Scegli il pasticciotto che vuoi acquistare (1-5):  ");
        scanf("%d", &scelta);

        if (scelta < 1 || scelta > 5) {
            printf("Scelta non valida. Riprova.\n");
            continue;
        }

        printf("Hai scelto il pasticciotto \"%s\".\n", pasticciotti[scelta-1]);
        printf("Il prezzo è di %.2f euro.\n", prezzi[scelta-1]);
        printf("Ci sono ancora %d pezzi disponibili.\n", pezzi_disponibili[scelta-1]);

        do {
            printf("Quanti pasticciotti vuoi acquistare? ");
            scanf("%d", &quantita);

            if (quantita > pezzi_disponibili[scelta-1]) {
                printf("Siamo spiacenti, ma ci sono solo %d pezzi disponibili. Riprova.\n", pezzi_disponibili[scelta-1]);
            }
        } while (quantita > pezzi_disponibili[scelta-1]);

        pezzi_disponibili[scelta-1] -= quantita; // aggiornamento pezzi disponibili
        acquistati += quantita; // aggiornamento pasticciotti acquistati
        totale += prezzi[scelta-1] * quantita; // aggiornamento totale

        printf("Hai acquistato %d pasticciotti \"%s\" per un totale di %.2f euro.\n");

            if (pezzi_disponibili[scelta-1] > 0) {
        do {
            printf("Vuoi comprare altro? (S/N) ");
            scanf(" %c", &risposta);
        } while (risposta != 'S' && risposta != 's' && risposta != 'N' && risposta != 'n');
    } else {
        printf("Siamo spiacenti, ma i pasticciotti \"%s\" sono esauriti.\n", pasticciotti[scelta-1]);
        risposta = 'N'; // forza uscita dal ciclo se il pasticciotto è esaurito
    }
} while (acquistati < 100 && (risposta == 'S' || risposta == 's'));

printf("Grazie per aver acquistato nella nostra pasticceria. Il totale della tua spesa è di %.2f euro.\n", totale);
printf ("vaffanculo leccesi di merda!");

return 0;

}