package ProvaEsame_16_02_2022;

import java.io.*;
import java.net.*;

public class ClientTDConnreuse {
    public static void main(String[] args) {
        
        // Controllo argomenti
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
                // Leggo username
                System.out.println("Inserisci l'username ('fine' per uscire)");
                String username = userIn.readLine();

                if (username.equals("fine"))
                    break;

                // Leggo password
                System.out.println("Inserisci la password ('fine' per uscire)");
                String password = userIn.readLine();

                if (password.equals("fine"))
                    break;

                // Leggo nome_cognome_artista
                System.out.println("Inserire il nome e il cognome dell'artista su cui effettuare la ricerca ('fine' per uscire)");
                String nome_cognome_artista = userIn.readLine();

                if (nome_cognome_artista.equals("fine"))
                    break;

                networkOut.write(username);
                networkOut.newLine();
                networkOut.write(password);
                networkOut.newLine();
                networkOut.write(nome_cognome_artista);
                networkOut.newLine();

                String theLine;
                for(;;)
                {
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
