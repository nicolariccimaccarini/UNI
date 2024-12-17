import java.io.*;
import java.net.*;

public class ClientTDConnreuse {
    public static void main(String[] args) {
        try {
            Socket s               = new Socket(args[0], Integer.parseInt(args[1]));
            BufferedReader netIn   = new BufferedReader(new InputStreamReader(s.getInputStream(), "UTF-8"));
            BufferedWriter netOut  = new BufferedWriter(new OutputStreamWriter(s.getOutputStream(), "UTF-8"));
            BufferedReader localIn = new BufferedReader(new InputStreamReader(System.in));

            while (true) {
                System.out.println("Inserisci mese (YYYYMM):");
                String mese = localIn.readLine();
                if (mese.equals("fine")) {
                    System.out.println("Termino");
                    break;
                }

                System.out.println("Inserisci categoria spese:");
                String categoria = localIn.readLine();

                System.out.println("Inserisci numero di spese:");
                String numero = localIn.readLine();

                netOut.write(mese);
                netOut.newLine();
                netOut.write(categoria);
                netOut.newLine();
                netOut.write(numero);
                netOut.newLine();

                netOut.flush();

                while (true) {
                    String line;

                    line = netIn.readLine();

                    if (line.equals("--- END REQUEST ---"))
                        break;

                    System.out.println(line);
                }
            }

            s.close();
        } catch (Exception e) {
            System.err.println(e.getMessage());
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
