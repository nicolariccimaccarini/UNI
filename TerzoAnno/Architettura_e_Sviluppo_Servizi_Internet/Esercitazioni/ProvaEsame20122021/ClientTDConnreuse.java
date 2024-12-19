import java.io.*;
import java.net.*;

import javax.naming.ldap.SortKey;

public class ClientTDConnreuse {
    public static void main(String[] args) {
        
        if (args.length != 2) {
            System.err.println("Errore! Uso: java ClientTDConnreuse server porta");
            System.exit(1);
        }

        try {
            Socket theSocket = new Socket(args[0], Integer.parseInt(args[1]));
            BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));
            BufferedReader networkIn = new BufferedReader(new InputStreamReader(theSocket.getInputStream(), "UTF-8"));
            BufferedWriter networkOut = new BufferedWriter(new OutputStreamWriter(theSocket.getOutputStream(), "UTF-8"));

            for (;;) {
                System.out.println("Inserisci categoria ('fine' per terminare)");
                String categoria = userIn.readLine();

                if (categoria.equals("fine"))
                    break;

                System.out.println("Inserisci il nome del produttore da ricercare ('fine' fine per terminare)");
                String nomeProduttore = userIn.readLine();

                if (nomeProduttore.equals("fine"))
                    break;
                
                System.out.println("Inserisci la modalita' di ordinamento ('fine' per terminare)");
                String modalitaOrdinamento = userIn.readLine();

                if (modalitaOrdinamento.equals("fine"))
                    break;

                networkOut.write(categoria);
                networkOut.newLine();
                networkOut.write(nomeProduttore);
                networkOut.newLine();
                networkOut.write(modalitaOrdinamento);
                networkOut.newLine();
                networkOut.flush();

                String theLine;
                for (;;) {
                    theLine = networkIn.readLine();

                    if (theLine == null) {
                        System.err.println("Errore! Il server ha chiuso la connessione");
                        System.exit(2);
                    }

                    System.out.println(theLine);

                    if (theLine.equals("--- END REQUEST ---"))
                        break;
                }
            }
            
            theSocket.close();

        } catch (IOException e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
            System.exit(3);
        }
    }
}
