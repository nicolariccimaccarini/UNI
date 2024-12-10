package Java.ProveEsame.Esame9Settembre2020;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

public class Main {
 
    public static void main(String[] args) {
        
        PipedInputStream pis = new PipedInputStream();
        PipedOutputStream pos = null;

        try {
            pos = new PipedOutputStream(pis);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-2);
        }

        GeneraDati generaDati = new GeneraDati(pos);
        Thread tgD = new Thread(generaDati);
        tgD.start();

        Storico storico = new Storico();

        Avvisi avvisi = new Avvisi(storico);
        avvisi.start();

        ObjectInputStream ois = null;
        try {
            ois = new ObjectInputStream(pis);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-2);
        }

        // per gestire la prima interazione del ciclo controllo l'umidita'
        boolean first = true;
        int umiditaPrecedente = 0;

        while (true) {

            Misure misure = null;
            try {
                misure = new (Misure) ois.readObject();
            } catch (IOException | ClassNotFoundException e) {
                System.err.println(e);
                System.exit(-2);
            }
            int umidita = misure.getUmidita();

            if (!first) {
                float variazione = (float) Math.abs(umidita - umiditaPrecedente) / umiditaPrecedente;
                System.out.println("Umidita' variazione registrata: " + variazione);
                // aggiornamento storico
                if (variazione > 0.2) {
                    System.out.println("Aggiorno storico cambi repentini");
                    storico.IncrementaCambiRepentini();
                }

                // verifico condizione di terminazione
                if (variazione > 0.4) {
                    System.out.println("Main: variazione superiore al 4%, termino");
                    generaDati.Finisci();
                    avvisi.Finisci();
                    break;
                }
            }
            else {
                first = false;
            }

            umiditaPrecedente = umidita;
        }
    }
}
