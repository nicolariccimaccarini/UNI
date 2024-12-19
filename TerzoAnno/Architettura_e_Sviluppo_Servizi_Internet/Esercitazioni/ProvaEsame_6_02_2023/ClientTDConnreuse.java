package ProvaEsame_6_02_2023;

import java.io.*;
import java.net.*;

public class ClientTDConnreuse {
    public static void main(String[] args) {
        
        // controllo argomenti
        if (args.length != 2) {
            System.err.println("Errore! Uso: java ClientTDConnreuse server porta");
            System.exit(1);
        }

        try {
            Socket theSocket          = new Socket(args[0], Integer.parseInt(args[1]));
            BufferedReader userIn     = new BufferedReader(new InputStreamReader(System.in));
            BufferedReader networkIn  = new BufferedReader(new InputStreamReader(theSocket.getInputStream(), "UTF-8"));
            BufferedWriter networkOut = new BufferedWriter(new OutputStreamWriter(theSocket.getOutputStream(), "UTF-8")); 

            for (;;) {
                System.out.println("Inserisci nome e cognome del giocatore da ricercare ('fine' per uscire)");
                String nomeCognome = userIn.readLine();

                if (nomeCognome.equals("fine"))
                    break;

                System.out.println("Inserisci la squadra ('fine' per terminare)");
                String squadra = userIn.readLine();

                if (squadra.equals("fine"))
                    break;

                System.out.println("Inserisci l'edizione del torneo ('fine' per terminare)");
                String edizione = userIn.readLine();

                if (edizione.equals("fine"))
                    break;

                networkOut.write(nomeCognome);
                networkOut.newLine();
                networkOut.write(squadra);
                networkOut.newLine();
                networkOut.write(edizione);
                networkOut.newLine();

                String theLine;
                for (;;) {
                    theLine = networkIn.readLine();

                    if(theLine == null)
                    {
                        System.err.println("Errore! Il server ha chiuso la connessione");
                        System.exit(2);
                    }

                    System.out.println(theLine);

                    if(theLine.equals("--- END REQUEST ---"))
                    {
                        break;
                    }
                }
                theSocket.close();

            }

        } catch(IOException e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
            System.exit(3);
        }
    }
}
