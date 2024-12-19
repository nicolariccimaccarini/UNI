import java.io.*;
import java.net.*;

public class ClientTDConnreuse {
    public static void main(String[] args) {
        
        if (args.length != 2) {
            System.err.println("Errore! Uso: java ClientTDConnreuse server porta");
            System.exit(1);
        }

        try {
            Socket thSocket           = new Socket(args[0], Integer.parseInt(args[1]));
            BufferedReader userIn     = new BufferedReader(new InputStreamReader(System.in));
            BufferedReader networkIn  = new BufferedReader(new InputStreamReader(thSocket.getInputStream(), "UTF-8"));
            BufferedWriter networkOut = new BufferedWriter(new OutputStreamWriter(thSocket.getOutputStream(), "UTF-8"));

            for (;;) {
                System.out.println("Inserisci username utente ('fine' per terminare)");
                String username = userIn.readLine();

                if (username.equals("fine"))
                    break;

                System.out.println("Inserisci nome progetto da ricercare ('fine' fine per terminare)");
                String nomeProgetto = userIn.readLine();

                if (nomeProgetto.equals("fine"))
                    break;
                
                System.out.println("Inserisci versione del progetto da ricercare ('fine' per terminare)");
                String nomeVersione = userIn.readLine();

                if (nomeVersione.equals("fine"))
                    break;

                networkOut.write(username);
                networkOut.newLine();
                networkOut.write(nomeProgetto);
                networkOut.newLine();
                networkOut.write(nomeVersione);
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
            thSocket.close();
            
        } catch (IOException e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
            System.exit(3);
        }
    }
}
