package Java.ProveEsame.Esame18Giugno2020;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
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
            System.exit(-1);
        }

        int soglia = 0;
        while (true) {
            System.out.println("Inserire valore di soglia: ");
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

            try {
                soglia = Integer.parseInt(br.readLine());
            } catch (IOException e) {
                e.printStackTrace();
            } catch (NumberFormatException e) {
                System.out.println("La soglia deve essere un intero!");
                continue;
            }

            if (soglia >= 100 && soglia <= 200) {
                break;
            }
            else {
                System.out.println("La soglia deve essere un intero compreso tra 100 e 200 (estremi inclusi)");
            }

            GeneraRilevazioni gr = new GeneraRilevazioni(pos);
            gr.start();

            ObjectInputStream ois = null;
            try {
                ois = new ObjectInputStream(pis);
            } catch (IOException e) {
                e.printStackTrace();
                System.exit(-1);
            }

            int count = 0;
            int countCons = 0;
            Rilevazione r = null;
            while (count < 10) {
                try {
                    r = (Rilevazione) ois.readObject();
                } catch (IOException e) {
                    e.printStackTrace();
                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                }

                if (r.getValore() <= 50) {
                    System.out.println("basso - " + r.getValore());
                }
                else if (r.getValore() <= 100) {
                    System.out.println("moderato - " + r.getValore());
                }
                else {
                    System.out.println("alto - " + r.getValore());
                }

                if (r.getValore() > soglia) {
                    countCons++;
                }
                else {
                    countCons = 0;
                }

                if (countCons >= 3) {
                    System.out.println("attenzione");
                }

                count++;
            }

            gr.terminaRilevazione();
            try {
                gr.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            System.out.println("Il thread GeneraRilevazioni e' terminato");
        }
    }
}
