package Tutorato4;

import java.io.*;

public class WriteLines {
    
    public WriteLines(String filename) throws IOException {

        // PrintWriter e' uno stream di manipolazione per scrivere righe di testo
        // File Writer e' uno stream di dati che permette di collagarsi ad un file di testo in scrittura
        PrintWriter output = new PrintWriter(new FileWriter(filename));

        // BufferedReader e' uno stream di manipolazione a caratteri che consente di leggere le stringhe
        // InputStreamReader converte uno stream di byte in uno stream di caratteri
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in));

        System.out.println("Inserisci il testo da salvare: ");
        String linea = input.readLine();

        while (!linea.equals("")) {
            output.println(linea);
            linea = input.readLine();
        }

        input.close();
        output.close();
    }
}
